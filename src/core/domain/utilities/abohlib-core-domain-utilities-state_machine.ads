--  =============================================================================
--  Abohlib.Core.Domain.Utilities.State_Machine - Generic State Machine
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Generic state machine abstraction for managing state transitions
--  with validation and error handling using the Result pattern.
--  =============================================================================

pragma Ada_2022;
pragma Unevaluated_Use_Of_Old (Allow);

with Abohlib.Core.Domain.Errors;
with Abohlib.Core.Domain.Result;

with Ada.Real_Time;

generic
   type State_Type is (<>);  -- Must be discrete type
   type Context_Type is private;  -- Optional context data
   
package Abohlib.Core.Domain.Utilities.State_Machine is

   --  ==========================================================================
   --  Error Types
   --  ==========================================================================
   
   type State_Machine_Error is record
      Base              : Abohlib.Core.Domain.Errors.Domain_Error;
      Current_State     : State_Type;
      Attempted_State   : State_Type;
   end record;
   
   --  Result types
   package State_Result is new Abohlib.Core.Domain.Result.Result_Package
     (Ok_Type => State_Type, Err_Type => State_Machine_Error);
     
   package Transition_Result is new Abohlib.Core.Domain.Result.Result_Package
     (Ok_Type => Boolean, Err_Type => State_Machine_Error);

   --  ==========================================================================
   --  Array Type Declaration (needed before use)
   --  ==========================================================================
   
   type State_Array is array (Positive range <>) of State_Type;
   
   --  State Machine Type
   --  ==========================================================================
   
   type State_Machine_Type is tagged private;
   --  Note: Type_Invariant would be ideal here to ensure:
   --    Total_Transitions >= Failed_Transitions
   --  However, Ada doesn't allow Type_Invariants to access private components
   --  when the type is declared as private. The invariant is maintained
   --  internally by the implementation.
   
   --  Transition validation function type
   type Transition_Validator is access function 
     (From_State : State_Type; 
      To_State   : State_Type; 
      Context    : Context_Type) return Boolean;
   
   --  Transition action procedure type  
   type Transition_Action is access procedure
     (Machine    : in out State_Machine_Type;
      From_State : State_Type;
      To_State   : State_Type;
      Context    : Context_Type);

   --  ==========================================================================
   --  State Machine Operations
   --  ==========================================================================
   
   --  Create a new state machine with initial state
   function Create (Initial_State : State_Type) return State_Machine_Type
   with
     Post => Create'Result.Get_Current_State = Initial_State;
   
   --  Get current state
   function Get_Current_State (Machine : State_Machine_Type) return State_Type;
   
   --  Attempt state transition with validation
   function Transition_To
     (Machine   : in out State_Machine_Type;
      New_State : State_Type;
      Context   : Context_Type;
      Validator : Transition_Validator := null;
      Action    : Transition_Action := null) return Transition_Result.Result
   with
     Post => (if Transition_Result.Is_Ok (Transition_To'Result) then
                Machine.Get_Current_State = New_State
              else
                Machine.Get_Current_State = Machine.Get_Current_State'Old);
   
   --  Check if transition is valid
   function Can_Transition_To
     (Machine   : State_Machine_Type;
      New_State : State_Type;
      Context   : Context_Type;
      Validator : Transition_Validator := null) return Boolean;
   
   --  Get state history (last N states)
   function Get_History 
     (Machine : State_Machine_Type; 
      Count   : Positive := 10) return State_Array
   with
     Post => Get_History'Result'Length <= Count;
   
   --  Reset to initial state
   procedure Reset (Machine : in out State_Machine_Type);
   
   --  Get statistics
   type State_Statistics is record
      Total_Transitions    : Natural := 0;
      Failed_Transitions   : Natural := 0;
      Current_State        : State_Type;
      Time_In_Current_State : Duration := 0.0;
   end record;
   
   function Get_Statistics (Machine : State_Machine_Type) return State_Statistics;
   
private

   --  Constants
   Max_History_Size : constant := 100;
   
   --  History tracking
   type State_History is record
      States : State_Array (1 .. Max_History_Size);
      Count  : Natural := 0;
      Head   : Natural := 1;  -- Circular buffer head
   end record;
   
   --  State machine implementation
   type State_Machine_Type is tagged record
      Current_State       : State_Type;
      Initial_State       : State_Type;
      History             : State_History;
      Total_Transitions   : Natural := 0;
      Failed_Transitions  : Natural := 0;
      Last_Transition_Time : Ada.Real_Time.Time;
   end record;
   
   --  Helper to add state to history
   procedure Add_To_History (Machine : in out State_Machine_Type; State : State_Type);

end Abohlib.Core.Domain.Utilities.State_Machine;