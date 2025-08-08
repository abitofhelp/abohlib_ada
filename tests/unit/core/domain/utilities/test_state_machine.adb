--   =============================================================================
--   Test_State_Machine - Unit tests for generic state machine
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--   =============================================================================

pragma Ada_2022;
pragma Warnings (Off, "subprogram body has no previous spec");
pragma Unevaluated_Use_Of_Old (Allow);

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Abohlib.Infrastructure.Testing.Test_Framework;
use Abohlib.Infrastructure.Testing.Test_Framework;

with Abohlib.Core.Domain.Utilities.State_Machine;
with Abohlib.Core.Domain.Utilities.Concurrent.Buffer_State_Machine;

package body Test_State_Machine is

   --   ==========================================================================
   --   Test State Machine Setup
   --   ==========================================================================

   --   Simple state type for testing
   type Test_State is (Initial, Running, Paused, Stopped);

   --   Simple context
   type Test_Context is record
      Value : Natural := 0;
   end record;

   --   Instantiate state machine
   package Test_SM is new Abohlib.Core.Domain.Utilities.State_Machine
     (State_Type => Test_State, Context_Type => Test_Context);

   use Test_SM;
   use Test_SM.Transition_Result;

   --   Test validator that only allows certain transitions
   function Test_Validator
     (From_State : Test_State; To_State : Test_State; Context : Test_Context)
      return Boolean
   is
      pragma Unreferenced (Context);
   begin
      case From_State is
         when Initial =>
            return To_State = Running;
         when Running =>
            return To_State in Paused | Stopped;
         when Paused =>
            return To_State in Running | Stopped;
         when Stopped =>
            return False;  -- Terminal state
      end case;
   end Test_Validator;

   --   ==========================================================================
   --   Basic State Machine Tests
   --   ==========================================================================

   function Test_Create_State_Machine return Void_Result.Result is
      Machine : constant Test_SM.State_Machine_Type := Create (Initial);
   begin
      if Machine.Get_Current_State /= Initial then
         return
           Void_Result.Err
             (Test_Error'
                (Kind      => Assertion_Failed,
                 Message   =>
                   To_Unbounded_String
                     ("Machine should start in Initial state"),
                 Details   => Null_Unbounded_String, Line_Number => 0,
                 Test_Name =>
                   To_Unbounded_String ("Test_Create_State_Machine")));
      end if;

      return Void_Result.Ok (True);
   end Test_Create_State_Machine;

   function Test_Valid_Transition return Void_Result.Result is
      Machine : Test_SM.State_Machine_Type := Create (Initial);
      Context : constant Test_Context      := (Value => 42);
   begin

      --   Transition from Initial to Running
      declare
         Result : constant Test_SM.Transition_Result.Result :=
           Machine.Transition_To (Running, Context, Test_Validator'Access);
      begin

         if not Test_SM.Transition_Result.Is_Ok (Result) then
            return
              Void_Result.Err
                (Test_Error'
                   (Kind      => Assertion_Failed,
                    Message   =>
                      To_Unbounded_String
                        ("Transition from Initial to Running should succeed"),
                    Details   => Null_Unbounded_String, Line_Number => 0,
                    Test_Name => To_Unbounded_String ("Test_Name")));
         elsif not (Machine.Get_Current_State = Running) then
            return
              Void_Result.Err
                (Test_Error'
                   (Kind      => Assertion_Failed,
                    Message   =>
                      To_Unbounded_String
                        ("Machine should be in Running state"),
                    Details   => Null_Unbounded_String, Line_Number => 0,
                    Test_Name => To_Unbounded_String ("Test_Name")));
         else
            return Void_Result.Ok (True);
         end if;
      end;
   end Test_Valid_Transition;

   function Test_Invalid_Transition return Void_Result.Result is
      Machine : Test_SM.State_Machine_Type := Create (Initial);
      Context : constant Test_Context      := (Value => 42);
   begin

      --   Try invalid transition from Initial to Stopped
      declare
         Result : constant Test_SM.Transition_Result.Result :=
           Machine.Transition_To (Stopped, Context, Test_Validator'Access);
      begin
         if not Test_SM.Transition_Result.Is_Err (Result) then
            return
              Void_Result.Err
                (Test_Error'
                   (Kind      => Assertion_Failed,
                    Message   =>
                      To_Unbounded_String
                        ("Transition from Initial to Stopped should fail"),
                    Details   => Null_Unbounded_String, Line_Number => 0,
                    Test_Name => To_Unbounded_String ("Test_Name")));
         elsif not (Machine.Get_Current_State = Initial) then
            return
              Void_Result.Err
                (Test_Error'
                   (Kind      => Assertion_Failed,
                    Message   =>
                      To_Unbounded_String
                        ("Machine should remain in Initial state"),
                    Details   => Null_Unbounded_String, Line_Number => 0,
                    Test_Name => To_Unbounded_String ("Test_Name")));
         else
            return Void_Result.Ok (True);
         end if;
      end;
   end Test_Invalid_Transition;

   function Test_History_Tracking return Void_Result.Result is
      Machine : Test_SM.State_Machine_Type := Create (Initial);
      Context : constant Test_Context      := (Value => 0);
      History : Test_SM.State_Array (1 .. 4);
   begin

      --   Make several transitions
      declare
         R1 : constant Test_SM.Transition_Result.Result :=
           Machine.Transition_To (Running, Context, Test_Validator'Access);
         R2 : constant Test_SM.Transition_Result.Result :=
           Machine.Transition_To (Paused, Context, Test_Validator'Access);
         R3 : constant Test_SM.Transition_Result.Result :=
           Machine.Transition_To (Running, Context, Test_Validator'Access);
         pragma Unreferenced (R1, R2, R3);
      begin
         null;
      end;

      --   Get history
      History := Machine.Get_History (4);

      if not (History'Length = 4) then
         return
           Void_Result.Err
             (Test_Error'
                (Kind      => Assertion_Failed,
                 Message   =>
                   To_Unbounded_String ("History should contain 4 states"),
                 Details   => Null_Unbounded_String, Line_Number => 0,
                 Test_Name => To_Unbounded_String ("Test_Name")));
      elsif not
        (History (1) = Initial and History (2) = Running and
         History (3) = Paused and History (4) = Running)
      then
         return
           Void_Result.Err
             (Test_Error'
                (Kind      => Assertion_Failed,
                 Message   =>
                   To_Unbounded_String
                     ("History should match transition sequence"),
                 Details   => Null_Unbounded_String, Line_Number => 0,
                 Test_Name => To_Unbounded_String ("Test_Name")));
      else
         return Void_Result.Ok (True);
      end if;
   end Test_History_Tracking;

   function Test_Reset return Void_Result.Result is
      Machine : Test_SM.State_Machine_Type := Create (Initial);
      Context : constant Test_Context      := (Value => 0);
   begin

      --   Make transitions
      declare
         R1 : constant Test_SM.Transition_Result.Result :=
           Machine.Transition_To (Running, Context, Test_Validator'Access);
         R2 : constant Test_SM.Transition_Result.Result :=
           Machine.Transition_To (Paused, Context, Test_Validator'Access);
         pragma Unreferenced (R1, R2);
      begin
         null;
      end;

      if not (Machine.Get_Current_State = Paused) then
         return
           Void_Result.Err
             (Test_Error'
                (Kind      => Assertion_Failed,
                 Message   =>
                   To_Unbounded_String ("Machine should be in Paused state"),
                 Details   => Null_Unbounded_String, Line_Number => 0,
                 Test_Name => To_Unbounded_String ("Test_Name")));
      end if;

      --   Reset
      Machine.Reset;

      if not (Machine.Get_Current_State = Initial) then
         return
           Void_Result.Err
             (Test_Error'
                (Kind      => Assertion_Failed,
                 Message   =>
                   To_Unbounded_String
                     ("Machine should be reset to Initial state"),
                 Details   => Null_Unbounded_String, Line_Number => 0,
                 Test_Name => To_Unbounded_String ("Test_Name")));
      else
         return Void_Result.Ok (True);
      end if;
   end Test_Reset;

   function Test_Statistics return Void_Result.Result is
      Machine : Test_SM.State_Machine_Type := Create (Initial);
      Context : constant Test_Context      := (Value => 0);
      Stats   : Test_SM.State_Statistics;
   begin

      --   Make successful and failed transitions
      declare
         R1 : constant Test_SM.Transition_Result.Result :=
           Machine.Transition_To (Running, Context, Test_Validator'Access);
         R2 : constant Test_SM.Transition_Result.Result :=
           Machine.Transition_To
             (Stopped, Context,
              Test_Validator'Access);  -- Valid, goes to terminal state
         R3 : constant Test_SM.Transition_Result.Result :=
           Machine.Transition_To
             (Paused, Context,
              Test_Validator'Access);  -- Invalid from Stopped state
         pragma Unreferenced (R1, R2, R3);
      begin
         null;
      end;

      Stats := Machine.Get_Statistics;

      if not (Stats.Total_Transitions = 2) then
         return
           Void_Result.Err
             (Test_Error'
                (Kind      => Assertion_Failed,
                 Message   =>
                   To_Unbounded_String
                     ("Should have 2 successful transitions"),
                 Details   => Null_Unbounded_String, Line_Number => 0,
                 Test_Name => To_Unbounded_String ("Test_Name")));
      elsif not (Stats.Failed_Transitions = 1) then
         return
           Void_Result.Err
             (Test_Error'
                (Kind      => Assertion_Failed,
                 Message   =>
                   To_Unbounded_String ("Should have 1 failed transition"),
                 Details   => Null_Unbounded_String, Line_Number => 0,
                 Test_Name => To_Unbounded_String ("Test_Name")));
      elsif not (Stats.Current_State = Stopped) then
         return
           Void_Result.Err
             (Test_Error'
                (Kind      => Assertion_Failed,
                 Message   =>
                   To_Unbounded_String
                     ("Current state should be Stopped (terminal state)"),
                 Details   => Null_Unbounded_String, Line_Number => 0,
                 Test_Name => To_Unbounded_String ("Test_Name")));
      else
         return Void_Result.Ok (True);
      end if;
   end Test_Statistics;

   --   ==========================================================================
   --   Buffer State Machine Tests
   --   ==========================================================================

   function Test_Buffer_State_Machine return Void_Result.Result is
      use Abohlib.Core.Domain.Utilities.Concurrent.Buffer_State_Machine;
      use Buffer_SM;

      Machine : Buffer_SM.State_Machine_Type := Create_Buffer_State_Machine;
      Context : constant Buffer_Context      :=
        (Buffer_Index => 0, Buffer_Size => 1_024, others => <>);
   begin

      --   Test producer workflow: Free -> Reading -> Ready
      if Machine.Get_Current_State /= Free then
         return
           Void_Result.Err
             (Test_Error'
                (Kind      => Assertion_Failed,
                 Message   =>
                   To_Unbounded_String ("Buffer should start in Free state"),
                 Details   => Null_Unbounded_String, Line_Number => 0,
                 Test_Name => To_Unbounded_String ("Test_Name")));
      end if;

      declare
         Result : constant Buffer_SM.Transition_Result.Result :=
           Machine.Transition_To
             (Reading, Context, Validate_Producer_Transition'Access);
      begin
         if not Buffer_SM.Transition_Result.Is_Ok (Result) then
            return
              Void_Result.Err
                (Test_Error'
                   (Kind      => Assertion_Failed,
                    Message   =>
                      To_Unbounded_String
                        ("Transition Free -> Reading should succeed"),
                    Details   => Null_Unbounded_String, Line_Number => 0,
                    Test_Name => To_Unbounded_String ("Test_Name")));
         end if;
      end;

      declare
         Result : constant Buffer_SM.Transition_Result.Result :=
           Machine.Transition_To
             (Ready, Context, Validate_Producer_Transition'Access);
      begin
         if not Buffer_SM.Transition_Result.Is_Ok (Result) then
            return
              Void_Result.Err
                (Test_Error'
                   (Kind      => Assertion_Failed,
                    Message   =>
                      To_Unbounded_String
                        ("Transition Reading -> Ready should succeed"),
                    Details   => Null_Unbounded_String, Line_Number => 0,
                    Test_Name => To_Unbounded_String ("Test_Name")));
         end if;
      end;

      --   Test consumer workflow: Ready -> Consuming -> Free
      declare
         Result : constant Buffer_SM.Transition_Result.Result :=
           Machine.Transition_To
             (Consuming, Context, Validate_Consumer_Transition'Access);
      begin
         if not Buffer_SM.Transition_Result.Is_Ok (Result) then
            return
              Void_Result.Err
                (Test_Error'
                   (Kind      => Assertion_Failed,
                    Message   =>
                      To_Unbounded_String
                        ("Transition Ready -> Consuming should succeed"),
                    Details   => Null_Unbounded_String, Line_Number => 0,
                    Test_Name => To_Unbounded_String ("Test_Name")));
         end if;
      end;

      declare
         Result : constant Buffer_SM.Transition_Result.Result :=
           Machine.Transition_To
             (Free, Context, Validate_Consumer_Transition'Access);
      begin
         if not Buffer_SM.Transition_Result.Is_Ok (Result) then
            return
              Void_Result.Err
                (Test_Error'
                   (Kind      => Assertion_Failed,
                    Message   =>
                      To_Unbounded_String
                        ("Transition Consuming -> Free should succeed"),
                    Details   => Null_Unbounded_String, Line_Number => 0,
                    Test_Name => To_Unbounded_String ("Test_Name")));
         else
            return Void_Result.Ok (True);
         end if;
      end;

   end Test_Buffer_State_Machine;

   function Test_Buffer_Invalid_Transitions return Void_Result.Result is
      use Abohlib.Core.Domain.Utilities.Concurrent.Buffer_State_Machine;
      use Buffer_SM;

      Machine : Buffer_SM.State_Machine_Type := Create_Buffer_State_Machine;
      Context : constant Buffer_Context      :=
        (Buffer_Index => 0, Buffer_Size => 1_024, others => <>);
   begin

      --   Try invalid transition: Free -> Consuming
      declare
         Result : constant Buffer_SM.Transition_Result.Result :=
           Machine.Transition_To
             (Consuming, Context, Validate_Any_Transition'Access);
      begin
         if not Buffer_SM.Transition_Result.Is_Err (Result) then
            return
              Void_Result.Err
                (Test_Error'
                   (Kind      => Assertion_Failed,
                    Message   =>
                      To_Unbounded_String
                        ("Transition Free -> Consuming should fail"),
                    Details   => Null_Unbounded_String, Line_Number => 0,
                    Test_Name => To_Unbounded_String ("Test_Name")));
         end if;
      end;

      --   Get to Ready state
      declare
         R1 : constant Buffer_SM.Transition_Result.Result :=
           Machine.Transition_To
             (Reading, Context, Validate_Producer_Transition'Access);
         R2 : constant Buffer_SM.Transition_Result.Result :=
           Machine.Transition_To
             (Ready, Context, Validate_Producer_Transition'Access);
         pragma Unreferenced (R1, R2);
      begin
         null;
      end;

      --   Try invalid transition: Ready -> Free (must go through Consuming)
      declare
         Result2 : constant Buffer_SM.Transition_Result.Result :=
           Machine.Transition_To
             (Free, Context, Validate_Any_Transition'Access);
      begin
         if not Buffer_SM.Transition_Result.Is_Err (Result2) then
            return
              Void_Result.Err
                (Test_Error'
                   (Kind      => Assertion_Failed,
                    Message   =>
                      To_Unbounded_String
                        ("Transition Ready -> Free should fail"),
                    Details   => Null_Unbounded_String, Line_Number => 0,
                    Test_Name => To_Unbounded_String ("Test_Name")));
         else
            return Void_Result.Ok (True);
         end if;
      end;

   end Test_Buffer_Invalid_Transitions;

   function Test_Buffer_Error_Recovery return Void_Result.Result is
      use Abohlib.Core.Domain.Utilities.Concurrent.Buffer_State_Machine;
      use Buffer_SM;

      Machine : Buffer_SM.State_Machine_Type := Create_Buffer_State_Machine;
      Context : constant Buffer_Context      :=
        (Buffer_Index => 0, Buffer_Size => 1_024, others => <>);
   begin

      --   Get to Reading state
      declare
         R1 : constant Buffer_SM.Transition_Result.Result :=
           Machine.Transition_To
             (Reading, Context, Validate_Producer_Transition'Access);
         pragma Unreferenced (R1);
      begin
         null;
      end;

      --   Transition to Error state
      declare
         Result : constant Buffer_SM.Transition_Result.Result :=
           Machine.Transition_To
             (Error, Context, Validate_Producer_Transition'Access);
      begin
         if not Buffer_SM.Transition_Result.Is_Ok (Result) then
            return
              Void_Result.Err
                (Test_Error'
                   (Kind      => Assertion_Failed,
                    Message   =>
                      To_Unbounded_String
                        ("Transition Reading -> Error should succeed"),
                    Details   => Null_Unbounded_String, Line_Number => 0,
                    Test_Name => To_Unbounded_String ("Test_Name")));
         end if;
      end;

      --   Recover from Error to Free
      declare
         Result : constant Buffer_SM.Transition_Result.Result :=
           Machine.Transition_To
             (Free, Context, Validate_Consumer_Transition'Access);
      begin
         if not Buffer_SM.Transition_Result.Is_Ok (Result) then
            return
              Void_Result.Err
                (Test_Error'
                   (Kind      => Assertion_Failed,
                    Message   =>
                      To_Unbounded_String
                        ("Transition Error -> Free should succeed"),
                    Details   => Null_Unbounded_String, Line_Number => 0,
                    Test_Name => To_Unbounded_String ("Test_Name")));
         else
            return Void_Result.Ok (True);
         end if;
      end;

   end Test_Buffer_Error_Recovery;

   function Test_Allowed_Next_States return Void_Result.Result is
      use Abohlib.Core.Domain.Utilities.Concurrent.Buffer_State_Machine;
   begin

      --   From Free state
      declare
         Allowed : constant Buffer_SM.State_Array :=
           Get_Allowed_Next_States (Free);
      begin
         if not (Allowed'Length = 1 and then Allowed (1) = Reading) then
            return
              Void_Result.Err
                (Test_Error'
                   (Kind      => Assertion_Failed,
                    Message   =>
                      To_Unbounded_String
                        ("From Free, only Reading is allowed"),
                    Details   => Null_Unbounded_String, Line_Number => 0,
                    Test_Name => To_Unbounded_String ("Test_Name")));
         end if;
      end;

      --   From Reading state
      declare
         Allowed : constant Buffer_SM.State_Array :=
           Get_Allowed_Next_States (Reading);
      begin
         if not (Allowed'Length = 3) then
            return
              Void_Result.Err
                (Test_Error'
                   (Kind      => Assertion_Failed,
                    Message   =>
                      To_Unbounded_String
                        ("From Reading, 3 transitions are allowed"),
                    Details   => Null_Unbounded_String, Line_Number => 0,
                    Test_Name => To_Unbounded_String ("Test_Name")));
         end if;
      end;

      --   From Ready state
      declare
         Allowed : constant Buffer_SM.State_Array :=
           Get_Allowed_Next_States (Ready);
      begin
         if not (Allowed'Length = 1 and then Allowed (1) = Consuming) then
            return
              Void_Result.Err
                (Test_Error'
                   (Kind      => Assertion_Failed,
                    Message   =>
                      To_Unbounded_String
                        ("From Ready, only Consuming is allowed"),
                    Details   => Null_Unbounded_String, Line_Number => 0,
                    Test_Name => To_Unbounded_String ("Test_Name")));
         else
            return Void_Result.Ok (True);
         end if;
      end;

   end Test_Allowed_Next_States;

   --   Main test runner function
   function Run_All_Tests
     (Output : access Test_Output_Port'Class) return Test_Stats_Result.Result
   is
      Tests : Test_Results_Array (1 .. 10);
      Index : Positive := 1;

      procedure Add_Test_Result
        (Name : String; Test_Func : Test_Function_Access)
      is
         Result : constant Test_Result_Pkg.Result :=
           Run_Test (Name, Test_Func, Output);
      begin
         if Result.Is_Ok then
            Tests (Index) := Result.Get_Ok;
            Print_Test_Result (Tests (Index), Output);
            Index := Index + 1;
         else
            declare
               Error : constant Test_Error := Result.Get_Err;
            begin
               Tests (Index) :=
                 Test_Result'
                   (Name => To_Unbounded_String (Name), Status => Failed,
                    Message        => Error.Message, Elapsed_Time => 0.0,
                    Line_Number    => Error.Line_Number,
                    Correlation_ID => To_Unbounded_String ("TEST-" & Name));
               Print_Test_Result (Tests (Index), Output);
               Index := Index + 1;
            end;
         end if;
      end Add_Test_Result;
   begin
      Output.Write_Line ("=== Running State Machine Unit Tests ===");
      Output.Write_Line ("");

      --   Basic state machine tests
      Add_Test_Result
        ("Test_Create_State_Machine", Test_Create_State_Machine'Access);
      Add_Test_Result ("Test_Valid_Transition", Test_Valid_Transition'Access);
      Add_Test_Result
        ("Test_Invalid_Transition", Test_Invalid_Transition'Access);
      Add_Test_Result ("Test_History_Tracking", Test_History_Tracking'Access);
      Add_Test_Result ("Test_Reset", Test_Reset'Access);
      Add_Test_Result ("Test_Statistics", Test_Statistics'Access);

      --   Buffer state machine tests
      Add_Test_Result
        ("Test_Buffer_State_Machine", Test_Buffer_State_Machine'Access);
      Add_Test_Result
        ("Test_Buffer_Invalid_Transitions",
         Test_Buffer_Invalid_Transitions'Access);
      Add_Test_Result
        ("Test_Buffer_Error_Recovery", Test_Buffer_Error_Recovery'Access);
      Add_Test_Result
        ("Test_Allowed_Next_States", Test_Allowed_Next_States'Access);

      declare
         Stats_Result : constant Test_Stats_Result.Result :=
           Run_Test_Suite
             ("State_Machine_Tests", Tests (1 .. Index - 1), Output);
      begin
         if Stats_Result.Is_Ok then
            declare
               Stats :
                 constant Abohlib.Infrastructure.Testing.Test_Framework
                   .Test_Statistics :=
                 Stats_Result.Get_Ok;
            begin
               Output.Write_Line ("");
               Print_Test_Summary ("State Machine Unit Tests", Stats, Output);
               return Test_Stats_Result.Ok (Stats);
            end;
         else
            return Stats_Result;
         end if;
      end;
   end Run_All_Tests;

end Test_State_Machine;

pragma Warnings (On, "subprogram body has no previous spec");
