--  =============================================================================
--  Abohlib.Core.Domain.Sagas.Saga_Coordinator - Implementation
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

with Ada.Numerics.Discrete_Random;

package body Abohlib.Core.Domain.Sagas.Saga_Coordinator is

   --  Simple ULID generation (matching events package)
   subtype ULID_Char is Character range '0' .. 'Z';
   package Random_Char is new Ada.Numerics.Discrete_Random (ULID_Char);
   Gen : Random_Char.Generator;

   function Generate_ULID return Abohlib.Core.Domain.Events.ULID_String is
      Result : Abohlib.Core.Domain.Events.ULID_String;
   begin
      for I in Result'Range loop
         Result (I) := Random_Char.Random (Gen);
      end loop;
      return Result;
   end Generate_ULID;

   ----------------
   -- Event_Name --
   ----------------

   overriding
   function Event_Name (Event : Saga_Event) return String is
   begin
      return "Saga." & Event.Event_Type'Image;
   end Event_Name;

   --------------
   -- Add_Step --
   --------------

   procedure Add_Step
     (Definition : in out Saga_Definition; Step : Saga_Step_Access) is
   begin
      Definition.Steps.Append (Step);
   end Add_Step;

   ---------------------
   -- Create_Instance --
   ---------------------

   function Create_Instance (Definition : Saga_Definition) return Saga_Instance
   is
   begin
      return Instance : Saga_Instance do
         Instance.Id := Saga_Id_Package.New_ID;
         Instance.Definition := Definition;
         Instance.State := Created;
         Instance.Started_At := Ada.Calendar.Clock;
      end return;
   end Create_Instance;

   -----------
   -- Start --
   -----------

   function Start
     (Instance : in out Saga_Instance;
      Context  : Unbounded_String := Null_Unbounded_String)
      return Saga_Result_Package.Result
   is
      Step_Index : Natural := 0;
   begin
      Instance.State := Running;
      Instance.Context := Context;

      --  Execute each step in sequence
      for Step of Instance.Definition.Steps loop
         Step_Index := Step_Index + 1;
         Instance.Current_Step := Step_Index;

         declare
            Step_Name : constant String := Step.all.Name;
            Execution : Step_Execution;
         begin
            Execution.Step_Name := To_Unbounded_String (Step_Name);
            Execution.State := Executing;
            Execution.Started_At := Ada.Calendar.Clock;

            --  Execute with retries
            declare
               Max_Attempts   : constant Natural :=
                 (if Step.all.Is_Retriable then Step.all.Max_Retries + 1
                  else 1);
               Step_Succeeded : Boolean := False;
            begin
               for Attempt in 1 .. Max_Attempts loop
                  Execution.Retry_Count := Attempt - 1;

                  declare
                     Result : constant Step_Result_Package.Result :=
                       Step.all.Execute (Instance.Context);
                  begin
                     if Step_Result_Package.Is_Ok (Result) then
                        Execution.Result :=
                          Step_Result_Package.Get_Ok (Result);
                        Execution.State := Completed;
                        Step_Succeeded := True;
                        exit;
                     else
                        Execution.Error_Message :=
                          Step_Result_Package.Get_Err (Result);
                        if Attempt = Max_Attempts then
                           Execution.State := Failed;
                        end if;
                     end if;
                  end;
               end loop;

               Execution.Completed_At := Ada.Calendar.Clock;
               Instance.Executions.Insert (Execution.Step_Name, Execution);

               if not Step_Succeeded then
                  --  Step failed, start compensation
                  Instance.State := Compensating;
                  Instance.Failure_Reason :=
                    To_Unbounded_String
                      ("Step '"
                       & Step_Name
                       & "' failed: "
                       & To_String (Execution.Error_Message));

                  --  Compensate all completed steps
                  Compensate_From_Step (Instance, Step_Index - 1);
                  Instance.State := Failed;
                  Instance.Completed_At := Ada.Calendar.Clock;

                  return Saga_Result_Package.Err (Instance.Failure_Reason);
               end if;
            end;
         end;
      end loop;

      --  All steps completed successfully
      Instance.State := Completed;
      Instance.Completed_At := Ada.Calendar.Clock;
      return Saga_Result_Package.Ok (True);
   end Start;

   ---------------------
   -- Get_Executions --
   ---------------------

   function Get_Executions (Instance : Saga_Instance) return Execution_Maps.Map
   is
   begin
      return Instance.Executions;
   end Get_Executions;

   ----------------------
   -- Get_Current_Step --
   ----------------------

   function Get_Current_Step (Instance : Saga_Instance) return String is
   begin
      if Instance.Current_Step = 0
        or else Instance.Current_Step
                > Natural (Instance.Definition.Steps.Length)
      then
         return "";
      else
         return Instance.Definition.Steps (Instance.Current_Step).all.Name;
      end if;
   end Get_Current_Step;

   -----------------------
   -- Get_Failure_Reason --
   -----------------------

   function Get_Failure_Reason (Instance : Saga_Instance) return String is
   begin
      return To_String (Instance.Failure_Reason);
   end Get_Failure_Reason;

   ---------------------------
   -- Compensate_From_Step --
   ---------------------------

   procedure Compensate_From_Step
     (Instance : in out Saga_Instance; Step_Num : Natural) is
   begin
      --  Compensate in reverse order
      for I in reverse 1 .. Step_Num loop
         declare
            Step      : Saga_Step_Access renames Instance.Definition.Steps (I);
            Step_Name : constant String := Step.all.Name;
            Key       : constant Unbounded_String :=
              To_Unbounded_String (Step_Name);
         begin
            if Instance.Executions.Contains (Key) then
               declare
                  Execution : Step_Execution := Instance.Executions (Key);
               begin
                  if Execution.State = Completed then
                     Execution.State := Compensating;
                     Instance.Executions.Replace (Key, Execution);

                     declare
                        Result : constant Step_Result_Package.Result :=
                          Step.all.Compensate (Instance.Context);
                     begin
                        if Step_Result_Package.Is_Ok (Result) then
                           Execution.State := Compensated;
                        else
                           --  Compensation failed, but continue with others
                           Execution.State := Failed;
                           Execution.Error_Message :=
                             Step_Result_Package.Get_Err (Result);
                        end if;
                        Instance.Executions.Replace (Key, Execution);
                     end;
                  end if;
               end;
            end if;
         end;
      end loop;
   end Compensate_From_Step;

   --------------------
   -- Register_Saga --
   --------------------

   procedure Register_Saga
     (Coordinator : in out Saga_Coordinator; Definition : Saga_Definition) is
   begin
      Coordinator.Registered_Sagas.Insert (Definition.Name, Definition);
   end Register_Saga;

   --------------
   -- Has_Saga --
   --------------

   function Has_Saga
     (Coordinator : Saga_Coordinator; Name : String) return Boolean is
   begin
      return
        Coordinator.Registered_Sagas.Contains (To_Unbounded_String (Name));
   end Has_Saga;

   ------------------
   -- Execute_Saga --
   ------------------

   function Execute_Saga
     (Coordinator : in out Saga_Coordinator;
      Saga_Name   : String;
      Context     : Unbounded_String := Null_Unbounded_String)
      return Saga_Result_Package.Result
   is
      Key : constant Unbounded_String := To_Unbounded_String (Saga_Name);
   begin
      if not Coordinator.Registered_Sagas.Contains (Key) then
         return
           Saga_Result_Package.Err
             (To_Unbounded_String ("Saga '" & Saga_Name & "' not registered"));
      end if;

      declare
         Definition   : constant Saga_Definition :=
           Coordinator.Registered_Sagas (Key);
         Instance_Ptr : constant Saga_Instance_Access :=
           new Saga_Instance'(Create_Instance (Definition));
         Instance     : Saga_Instance renames Instance_Ptr.all;
      begin
         --  Fire saga started event
         Fire_Event
           (Coordinator,
            Saga_Event'
              (Event_Id      => Generate_ULID,
               Occurred_At   => Ada.Calendar.Clock,
               Event_Version => 1,
               Event_Type    => Saga_Started,
               Saga_Id       => Instance.Id,
               Saga_Name     => Definition.Name,
               Step_Name     => Null_Unbounded_String,
               Details       => Context));

         --  Execute saga
         declare
            Result : constant Saga_Result_Package.Result :=
              Instance.Start (Context);
         begin
            --  Store instance pointer
            Coordinator.Active_Instances.Insert (Instance.Id, Instance_Ptr);

            --  Fire completion event
            if Saga_Result_Package.Is_Ok (Result) then
               Fire_Event
                 (Coordinator,
                  Saga_Event'
                    (Event_Id      => Generate_ULID,
                     Occurred_At   => Ada.Calendar.Clock,
                     Event_Version => 1,
                     Event_Type    => Saga_Completed,
                     Saga_Id       => Instance.Id,
                     Saga_Name     => Definition.Name,
                     Step_Name     => Null_Unbounded_String,
                     Details       =>
                       To_Unbounded_String ("Saga completed successfully")));
            else
               Fire_Event
                 (Coordinator,
                  Saga_Event'
                    (Event_Id      => Generate_ULID,
                     Occurred_At   => Ada.Calendar.Clock,
                     Event_Version => 1,
                     Event_Type    => Saga_Failed,
                     Saga_Id       => Instance.Id,
                     Saga_Name     => Definition.Name,
                     Step_Name     => Null_Unbounded_String,
                     Details       => Saga_Result_Package.Get_Err (Result)));
            end if;

            return Result;
         end;
      end;
   end Execute_Saga;

   --------------
   -- Get_Saga --
   --------------

   --  NOTE: Commented out due to Ada limitation with returning limited types
   --  function Get_Saga
   --    (Coordinator : Saga_Coordinator;
   --     Id          : Saga_Id) return Saga_Instance
   --  is
   --  begin
   --     return Coordinator.Active_Instances (Id);
   --  end Get_Saga;

   ---------------
   -- Subscribe --
   ---------------

   procedure Subscribe
     (Coordinator : in out Saga_Coordinator; Handler : Saga_Event_Handler) is
   begin
      Coordinator.Event_Handlers.Append (Handler);
   end Subscribe;

   ----------------
   -- Fire_Event --
   ----------------

   procedure Fire_Event
     (Coordinator : Saga_Coordinator; Event : Saga_Event'Class) is
   begin
      for Handler of Coordinator.Event_Handlers loop
         Handler (Saga_Event (Event));
      end loop;
   end Fire_Event;

begin
   --  Initialize random generator
   Random_Char.Reset (Gen);
end Abohlib.Core.Domain.Sagas.Saga_Coordinator;
