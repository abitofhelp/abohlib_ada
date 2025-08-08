--  =============================================================================
--  Abohlib.Core.Domain.Utilities.State_Machine - Implementation
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

package body Abohlib.Core.Domain.Utilities.State_Machine is

   use Ada.Real_Time;
   use Abohlib.Core.Domain.Errors;

   --  ==========================================================================
   --  Public Operations
   --  ==========================================================================
   
   function Create (Initial_State : State_Type) return State_Machine_Type is
   begin
      return Machine : State_Machine_Type do
         Machine.Current_State := Initial_State;
         Machine.Initial_State := Initial_State;
         Machine.Last_Transition_Time := Clock;
         Add_To_History (Machine, Initial_State);
      end return;
   end Create;
   
   function Get_Current_State (Machine : State_Machine_Type) return State_Type is
     (Machine.Current_State);
   
   function Transition_To
     (Machine   : in out State_Machine_Type;
      New_State : State_Type;
      Context   : Context_Type;
      Validator : Transition_Validator := null;
      Action    : Transition_Action := null) return Transition_Result.Result
   is
      Old_State : constant State_Type := Machine.Current_State;
   begin
      --  Check if transition is valid
      if Validator /= null and then not Validator (Old_State, New_State, Context) then
         Machine.Failed_Transitions := Machine.Failed_Transitions + 1;
         
         declare
            Error : State_Machine_Error;
         begin
            Error.Base := Default_Domain_Error;
            Error.Base.Category := Business_Rule_Error;
            Error.Base.Message := Error_Strings.To_Bounded_String
              ("Invalid state transition from " & Old_State'Image & 
               " to " & New_State'Image);
            Error.Current_State := Old_State;
            Error.Attempted_State := New_State;
            return Transition_Result.Err (Error);
         end;
      end if;
      
      --  Perform transition
      Machine.Current_State := New_State;
      Machine.Total_Transitions := Machine.Total_Transitions + 1;
      Machine.Last_Transition_Time := Clock;
      Add_To_History (Machine, New_State);
      
      --  Execute action if provided
      if Action /= null then
         Action (Machine, Old_State, New_State, Context);
      end if;
      
      return Transition_Result.Ok (True);
   end Transition_To;
   
   function Can_Transition_To
     (Machine   : State_Machine_Type;
      New_State : State_Type;
      Context   : Context_Type;
      Validator : Transition_Validator := null) return Boolean
   is
   begin
      if Validator = null then
         return True;
      else
         return Validator (Machine.Current_State, New_State, Context);
      end if;
   end Can_Transition_To;
   
   function Get_History 
     (Machine : State_Machine_Type; 
      Count   : Positive := 10) return State_Array
   is
      Actual_Count : constant Natural := Natural'Min (Count, Machine.History.Count);
      Result : State_Array (1 .. Actual_Count);
      Start_Pos : Natural;
   begin
      if Actual_Count = 0 then
         return Result;
      end if;
      
      --  Calculate starting position in circular buffer
      if Machine.History.Count <= Max_History_Size then
         --  Buffer not full yet, start from beginning
         Start_Pos := 1;
      else
         --  Buffer is full, calculate position for most recent N items
         Start_Pos := ((Machine.History.Head - Actual_Count) mod Max_History_Size) + 1;
      end if;
      
      --  Copy states from circular buffer
      for I in 1 .. Actual_Count loop
         Result (I) := Machine.History.States ((Start_Pos + I - 2) mod Max_History_Size + 1);
      end loop;
      
      return Result;
   end Get_History;
   
   procedure Reset (Machine : in out State_Machine_Type) is
   begin
      Machine.Current_State := Machine.Initial_State;
      Machine.Last_Transition_Time := Clock;
      Machine.Total_Transitions := Machine.Total_Transitions + 1;
      Add_To_History (Machine, Machine.Initial_State);
   end Reset;
   
   function Get_Statistics (Machine : State_Machine_Type) return State_Statistics is
      Current_Time : constant Time := Clock;
      Time_In_State : constant Duration := To_Duration (Current_Time - Machine.Last_Transition_Time);
   begin
      return State_Statistics'
        (Total_Transitions     => Machine.Total_Transitions,
         Failed_Transitions    => Machine.Failed_Transitions,
         Current_State         => Machine.Current_State,
         Time_In_Current_State => Time_In_State);
   end Get_Statistics;

   --  ==========================================================================
   --  Private Operations
   --  ==========================================================================
   
   procedure Add_To_History (Machine : in out State_Machine_Type; State : State_Type) is
   begin
      Machine.History.States (Machine.History.Head) := State;
      Machine.History.Head := (Machine.History.Head mod Max_History_Size) + 1;
      
      if Machine.History.Count < Max_History_Size then
         Machine.History.Count := Machine.History.Count + 1;
      end if;
   end Add_To_History;

end Abohlib.Core.Domain.Utilities.State_Machine;