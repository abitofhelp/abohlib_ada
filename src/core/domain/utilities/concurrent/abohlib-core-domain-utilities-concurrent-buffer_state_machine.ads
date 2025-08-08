--  =============================================================================
--  Abohlib.Core.Domain.Utilities.Concurrent.Buffer_State_Machine
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  State machine implementation for buffer lifecycle management.
--  Ensures valid state transitions and provides transition validation.
--  =============================================================================

pragma Ada_2022;
pragma Unevaluated_Use_Of_Old (Allow);

with Abohlib.Core.Domain.Utilities.State_Machine;

package Abohlib.Core.Domain.Utilities.Concurrent.Buffer_State_Machine is

   --  Define buffer states directly (since Buffer_Manager is generic)
   type Buffer_State is 
     (Free,       -- Available for producer use
      Reading,    -- Producer is filling buffer
      Ready,      -- Buffer contains data, ready for consumer
      Consuming,  -- Consumer is processing buffer
      Error);     -- Buffer is in error state
   
   --  Context for buffer state transitions
   type Buffer_Context is record
      Buffer_Index : Natural := 0;
      Buffer_Size  : Natural := 0;
      Error_Detail : access String := null;
   end record;
   
   --  Instantiate generic state machine for buffers
   package Buffer_SM is new Abohlib.Core.Domain.Utilities.State_Machine
     (State_Type    => Buffer_State,
      Context_Type  => Buffer_Context);
   
   use Buffer_SM;
   
   --  ==========================================================================
   --  Transition Validators
   --  ==========================================================================
   
   --  Validator for producer transitions
   function Validate_Producer_Transition
     (From_State : Buffer_State;
      To_State   : Buffer_State;
      Context    : Buffer_Context) return Boolean
   with
     Post => (if Validate_Producer_Transition'Result then
                (From_State = Free and To_State = Reading) or
                (From_State = Reading and To_State = Ready) or
                (From_State = Reading and To_State = Error) or
                (From_State = Reading and To_State = Free));  -- Cancel operation
   
   --  Validator for consumer transitions  
   function Validate_Consumer_Transition
     (From_State : Buffer_State;
      To_State   : Buffer_State;
      Context    : Buffer_Context) return Boolean
   with
     Post => (if Validate_Consumer_Transition'Result then
                (From_State = Ready and To_State = Consuming) or
                (From_State = Consuming and To_State = Free) or
                (From_State = Error and To_State = Free));  -- Error recovery
   
   --  Combined validator for any transition
   function Validate_Any_Transition
     (From_State : Buffer_State;
      To_State   : Buffer_State;
      Context    : Buffer_Context) return Boolean
   with
     Post => Validate_Any_Transition'Result = 
             (Validate_Producer_Transition (From_State, To_State, Context) or
              Validate_Consumer_Transition (From_State, To_State, Context));

   --  ==========================================================================
   --  Transition Tables (for documentation and validation)
   --  ==========================================================================
   
   type Transition_Rule is record
      From_State : Buffer_State;
      To_State   : Buffer_State;
      Role       : String (1 .. 8);  -- "Producer" or "Consumer"
   end record;
   
   --  Valid state transitions
   Valid_Transitions : constant array (Positive range <>) of Transition_Rule :=
     [(Free,      Reading,   "Producer"),
      (Reading,   Ready,     "Producer"),
      (Reading,   Error,     "Producer"),
      (Reading,   Free,      "Producer"),  -- Cancel
      (Ready,     Consuming, "Consumer"),
      (Consuming, Free,      "Consumer"),
      (Error,     Free,      "Consumer")]; -- Recovery
   
   --  ==========================================================================
   --  Utility Functions
   --  ==========================================================================
   
   --  Check if a state transition is valid
   function Is_Valid_Transition
     (From_State : Buffer_State;
      To_State   : Buffer_State) return Boolean
   with
     Post => Is_Valid_Transition'Result = 
             (for some T of Valid_Transitions => 
                T.From_State = From_State and T.To_State = To_State);
   
   --  Get allowed next states from current state
   function Get_Allowed_Next_States
     (Current_State : Buffer_State) return Buffer_SM.State_Array
   with
     Post => (for all S of Get_Allowed_Next_States'Result =>
                Is_Valid_Transition (Current_State, S));
   
   --  Create a buffer state machine with initial state
   function Create_Buffer_State_Machine
     (Initial_State : Buffer_State := Free) return Buffer_SM.State_Machine_Type
   with
     Post => Create_Buffer_State_Machine'Result.Get_Current_State = Initial_State;

end Abohlib.Core.Domain.Utilities.Concurrent.Buffer_State_Machine;