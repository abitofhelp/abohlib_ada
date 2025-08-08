--  =============================================================================
--  Abohlib.Core.Domain.Sagas.Saga_Coordinator - Distributed Transaction Coordinator
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Saga pattern implementation for managing distributed transactions across
--  multiple services, databases, and external systems. Supports both
--  orchestration and choreography patterns with compensation logic.
--
--  Usage Example:
--  ```ada
--  --  Define saga steps
--  type Reserve_Inventory_Step is new Saga_Step_Interface with null record;
--  overriding function Execute (...) return Step_Result_Package.Result;
--  overriding function Compensate (...) return Step_Result_Package.Result;
--
--  --  Create saga definition
--  Order_Saga : Saga_Definition;
--  Order_Saga.Add_Step (Reserve_Inventory_Step'(...));
--  Order_Saga.Add_Step (Charge_Payment_Step'(...));
--  Order_Saga.Add_Step (Ship_Order_Step'(...));
--
--  --  Execute saga
--  Instance : Saga_Instance := Create_Instance (Order_Saga);
--  Result : constant Saga_Result_Package.Result := Instance.Start (Context);
--  ```
--
--  Features:
--  - Step-by-step transaction execution
--  - Automatic compensation on failure
--  - Retry support for individual steps
--  - Event-based monitoring
--  - Timeout handling
--  - Nested saga support
--  =============================================================================

pragma Ada_2022;

with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Calendar;
with Abohlib.Core.Domain.Result;
with Abohlib.Core.Domain.Events;
with Abohlib.Core.Domain.Value_Objects.Type_Safe_Generic_Id;

package Abohlib.Core.Domain.Sagas.Saga_Coordinator is
   pragma Elaborate_Body;

   --  Saga ID type
   type Saga_Id_Category is null record;

   package Saga_Id_Package is new
     Abohlib.Core.Domain.Value_Objects.Type_Safe_Generic_Id.Generic_ID_Type
       (Category      => Saga_Id_Category,
        Category_Name => "Saga",
        Prefix        => "saga_");

   subtype Saga_Id is Saga_Id_Package.ID;
   function "<" (Left, Right : Saga_Id) return Boolean
   renames Saga_Id_Package."<";

   --  Saga state
   type Saga_State is
     (Created,           --  Saga created but not started
      Running,           --  Saga is executing steps
      Compensating,      --  Saga is rolling back due to failure
      Completed,         --  All steps completed successfully
      Failed,            --  Saga failed and compensation completed
      Aborted);          --  Saga aborted without compensation

   --  Step state
   type Step_State is
     (Pending,           --  Step not yet executed
      Executing,         --  Step currently executing
      Completed,         --  Step completed successfully
      Failed,            --  Step failed
      Compensating,      --  Step being compensated
      Compensated);      --  Step compensation completed

   --  Step execution result
   type Step_Result is record
      Success : Boolean := False;
      Message : Unbounded_String;
      Data    : Unbounded_String;  --  JSON or other serialized data
   end record;

   --  Result types
   package Step_Result_Package is new
     Abohlib.Core.Domain.Result.Result_Package
       (Ok_Type  => Step_Result,
        Err_Type => Unbounded_String);

   package Saga_Result_Package is new
     Abohlib.Core.Domain.Result.Result_Package
       (Ok_Type  => Boolean,
        Err_Type => Unbounded_String);

   --  Saga step interface
   type Saga_Step_Interface is interface;

   --  Execute the step forward
   function Execute
     (Step : Saga_Step_Interface; Context : Unbounded_String)
      return Step_Result_Package.Result
   is abstract;

   --  Compensate (rollback) the step
   function Compensate
     (Step : Saga_Step_Interface; Context : Unbounded_String)
      return Step_Result_Package.Result
   is abstract;

   --  Get step name
   function Name (Step : Saga_Step_Interface) return String is abstract;

   --  Check if step can be retried
   function Is_Retriable (Step : Saga_Step_Interface) return Boolean
   is abstract;

   --  Get maximum retry attempts
   function Max_Retries (Step : Saga_Step_Interface) return Natural
   is abstract;

   --  Step execution record
   type Step_Execution is record
      Step_Name     : Unbounded_String;
      State         : Step_State := Pending;
      Started_At    : Ada.Calendar.Time;
      Completed_At  : Ada.Calendar.Time;
      Retry_Count   : Natural := 0;
      Result        : Step_Result;
      Error_Message : Unbounded_String;
   end record;

   --  Access type for steps to handle indefinite types
   type Saga_Step_Access is access all Saga_Step_Interface'Class;

   --  Collections
   package Step_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Saga_Step_Access);

   package Execution_Maps is new
     Ada.Containers.Ordered_Maps
       (Key_Type     => Unbounded_String,
        Element_Type => Step_Execution);

   --  Saga definition
   type Saga_Definition is record
      Name        : Unbounded_String;
      Description : Unbounded_String;
      Steps       : Step_Vectors.Vector;
      Timeout_Ms  : Natural := 300_000;  --  5 minutes default
   end record;

   --  Add a step to the saga
   procedure Add_Step
     (Definition : in out Saga_Definition; Step : Saga_Step_Access)
   with
     Post =>
       Natural (Definition.Steps.Length)
       = Natural (Definition.Steps.Length'Old) + 1;

   --  Saga instance (execution context)
   type Saga_Instance is tagged limited private;

   --  Create a new saga instance
   function Create_Instance (Definition : Saga_Definition) return Saga_Instance
   with Post => Get_State (Create_Instance'Result) = Created;

   --  Start saga execution
   function Start
     (Instance : in out Saga_Instance;
      Context  : Unbounded_String := Null_Unbounded_String)
      return Saga_Result_Package.Result
   with
     Pre => Get_State (Instance) = Created,
     Post => Get_State (Instance) in Running | Completed | Failed;

   --  Get saga state
   function Get_State (Instance : Saga_Instance) return Saga_State
   with Inline;

   --  Get saga ID
   function Get_Id (Instance : Saga_Instance) return Saga_Id
   with Inline;

   --  Get execution details
   function Get_Executions
     (Instance : Saga_Instance) return Execution_Maps.Map;

   --  Get current step
   function Get_Current_Step (Instance : Saga_Instance) return String;

   --  Check if saga completed successfully
   function Is_Successful (Instance : Saga_Instance) return Boolean
   with Inline;

   --  Get failure reason if failed
   function Get_Failure_Reason (Instance : Saga_Instance) return String;

   --  Saga coordinator for managing multiple sagas
   type Saga_Coordinator is tagged limited private;

   --  Register a saga definition
   procedure Register_Saga
     (Coordinator : in out Saga_Coordinator; Definition : Saga_Definition)
   with Post => Coordinator.Has_Saga (To_String (Definition.Name));

   --  Check if saga is registered
   function Has_Saga
     (Coordinator : Saga_Coordinator; Name : String) return Boolean;

   --  Execute a saga by name
   function Execute_Saga
     (Coordinator : in out Saga_Coordinator;
      Saga_Name   : String;
      Context     : Unbounded_String := Null_Unbounded_String)
      return Saga_Result_Package.Result
   with Pre => Coordinator.Has_Saga (Saga_Name);

   --  Get saga instance by ID
   --  NOTE: Commented out due to Ada limitation with returning limited types
   --  function Get_Saga
   --    (Coordinator : Saga_Coordinator;
   --     Id          : Saga_Id) return Saga_Instance;

   --  Event-based saga support

   --  Saga events
   type Saga_Event_Type is
     (Saga_Started,
      Saga_Completed,
      Saga_Failed,
      Step_Started,
      Step_Completed,
      Step_Failed,
      Compensation_Started,
      Compensation_Completed);

   type Saga_Event is new Abohlib.Core.Domain.Events.Domain_Event with record
      Event_Type : Saga_Event_Type;
      Saga_Id    : Saga_Id_Package.ID;
      Saga_Name  : Unbounded_String;
      Step_Name  : Unbounded_String;
      Details    : Unbounded_String;
   end record;

   overriding
   function Event_Name (Event : Saga_Event) return String;

   --  Subscribe to saga events
   type Saga_Event_Handler is access procedure (Event : Saga_Event);

   procedure Subscribe
     (Coordinator : in out Saga_Coordinator; Handler : Saga_Event_Handler);

private

   --  Helper procedures
   procedure Compensate_From_Step
     (Instance : in out Saga_Instance; Step_Num : Natural);

   procedure Fire_Event
     (Coordinator : Saga_Coordinator; Event : Saga_Event'Class);

   type Saga_Instance is tagged limited record
      Id             : Saga_Id;
      Definition     : Saga_Definition;
      State          : Saga_State := Created;
      Context        : Unbounded_String;
      Current_Step   : Natural := 0;
      Executions     : Execution_Maps.Map;
      Started_At     : Ada.Calendar.Time;
      Completed_At   : Ada.Calendar.Time;
      Failure_Reason : Unbounded_String;
   end record;

   function Get_State (Instance : Saga_Instance) return Saga_State
   is (Instance.State);

   function Get_Id (Instance : Saga_Instance) return Saga_Id
   is (Instance.Id);

   function Is_Successful (Instance : Saga_Instance) return Boolean
   is (Instance.State = Completed);

   package Saga_Maps is new
     Ada.Containers.Ordered_Maps
       (Key_Type     => Unbounded_String,
        Element_Type => Saga_Definition);

   type Saga_Instance_Access is access all Saga_Instance;

   package Instance_Maps is new
     Ada.Containers.Ordered_Maps
       (Key_Type     => Saga_Id,
        Element_Type => Saga_Instance_Access);

   package Handler_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Saga_Event_Handler);

   type Saga_Coordinator is tagged limited record
      Registered_Sagas : Saga_Maps.Map;
      Active_Instances : Instance_Maps.Map;
      Event_Handlers   : Handler_Vectors.Vector;
   end record;

end Abohlib.Core.Domain.Sagas.Saga_Coordinator;
