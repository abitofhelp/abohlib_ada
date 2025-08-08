--  =============================================================================
--  Abohlib.Core.Domain.Utilities.Concurrent.Buffer_State_Machine - Implementation
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

package body Abohlib.Core.Domain.Utilities.Concurrent.Buffer_State_Machine is

   --  ==========================================================================
   --  Transition Validators Implementation
   --  ==========================================================================
   
   function Validate_Producer_Transition
     (From_State : Buffer_State;
      To_State   : Buffer_State;
      Context    : Buffer_Context) return Boolean
   is
      pragma Unreferenced (Context);
   begin
      case From_State is
         when Free =>
            return To_State = Reading;
            
         when Reading =>
            return To_State in Ready | Error | Free;
            
         when others =>
            return False;
      end case;
   end Validate_Producer_Transition;
   
   function Validate_Consumer_Transition
     (From_State : Buffer_State;
      To_State   : Buffer_State;
      Context    : Buffer_Context) return Boolean
   is
      pragma Unreferenced (Context);
   begin
      case From_State is
         when Ready =>
            return To_State = Consuming;
            
         when Consuming =>
            return To_State = Free;
            
         when Error =>
            return To_State = Free;
            
         when others =>
            return False;
      end case;
   end Validate_Consumer_Transition;
   
   function Validate_Any_Transition
     (From_State : Buffer_State;
      To_State   : Buffer_State;
      Context    : Buffer_Context) return Boolean
   is
   begin
      return Validate_Producer_Transition (From_State, To_State, Context) or else
             Validate_Consumer_Transition (From_State, To_State, Context);
   end Validate_Any_Transition;

   --  ==========================================================================
   --  Utility Functions Implementation
   --  ==========================================================================
   
   function Is_Valid_Transition
     (From_State : Buffer_State;
      To_State   : Buffer_State) return Boolean
   is
   begin
      for T of Valid_Transitions loop
         if T.From_State = From_State and T.To_State = To_State then
            return True;
         end if;
      end loop;
      return False;
   end Is_Valid_Transition;
   
   function Get_Allowed_Next_States
     (Current_State : Buffer_State) return Buffer_SM.State_Array
   is
      --  Maximum possible next states
      Max_Next_States : constant := 3;
      Temp_States : Buffer_SM.State_Array (1 .. Max_Next_States);
      Count : Natural := 0;
   begin
      --  Find all valid transitions from current state
      for T of Valid_Transitions loop
         if T.From_State = Current_State then
            Count := Count + 1;
            Temp_States (Count) := T.To_State;
         end if;
      end loop;
      
      --  Return array of actual size
      declare
         Result : Buffer_SM.State_Array (1 .. Count);
      begin
         Result := Temp_States (1 .. Count);
         return Result;
      end;
   end Get_Allowed_Next_States;
   
   function Create_Buffer_State_Machine
     (Initial_State : Buffer_State := Free) return Buffer_SM.State_Machine_Type
   is
   begin
      return Buffer_SM.Create (Initial_State);
   end Create_Buffer_State_Machine;

end Abohlib.Core.Domain.Utilities.Concurrent.Buffer_State_Machine;