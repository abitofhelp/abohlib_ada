--   =============================================================================
--   Test_Lock_Free_Queue_Adapter - Unit tests for Lock-Free Queue Adapter
--   =============================================================================
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--   =============================================================================

pragma Ada_2022;
pragma Warnings (Off, "subprogram body has no previous spec");

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Abohlib.Core.Domain.Utilities.Concurrent.Lock_Free_Queue_Adapter;
--   with Abohlib.Core.Domain.Ports.Concurrent.Queue;

package body Test_Lock_Free_Queue_Adapter is

--   ==========================================================================
--   Test Setup
--   ==========================================================================

--   Constants
   Small_Size : constant := 8;

--   Create a test instantiation with integers
   package Int_Queue_Adapter is new
      Abohlib.Core.Domain.Utilities.Concurrent.Lock_Free_Queue_Adapter.Queue_Adapter
        (Element_Type => Integer,
         Buffer_Size  => Small_Size);

   use Int_Queue_Adapter;
--     use Int_Queue_Adapter.Queue_Interface;
--     use Int_Queue_Adapter.Ring_Buffer;

--   ==========================================================================
--   Test Functions
--   ==========================================================================

   function Test_Create_Queue return Void_Result.Result is
      Queue : Lock_Free_Queue_Type;
   begin
--   Initialize the queue
      Initialize (Queue);

--   Verify initial state
      if not Is_Empty (Queue) then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("New queue should be empty"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Create_Queue")
         ));
      end if;

      if Size (Queue) /= 0 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("New queue size should be 0"),
            Details     => To_Unbounded_String ("Got: " & Size (Queue)'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Create_Queue")
         ));
      end if;

      return Void_Result.Ok (True);
   end Test_Create_Queue;

   function Test_Try_Push_Try_Pop return Void_Result.Result is
      Queue : Lock_Free_Queue_Type;
      Value : constant Integer := 42;
      Result : Integer;
   begin
      Initialize (Queue);

--   Try_Push a value
      if not Try_Push (Queue, Value) then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Push should succeed on empty queue"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Try_Push_Try_Pop")
         ));
      end if;

--   Verify size
      if Size (Queue) /= 1 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Size should be 1 after enqueue"),
            Details     => To_Unbounded_String ("Got: " & Size (Queue)'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Try_Push_Try_Pop")
         ));
      end if;

--   Try_Pop the value
      if not Try_Pop (Queue, Result) then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Pop should succeed on non-empty queue"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Try_Push_Try_Pop")
         ));
      end if;

--   Verify value
      if Result /= Value then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Try_Popd value should match enqueued value"),
            Details     => To_Unbounded_String ("Expected: " & Value'Image &
                                               ", Got: " & Result'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Try_Push_Try_Pop")
         ));
      end if;

--   Verify empty again
      if not Is_Empty (Queue) then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Queue should be empty after dequeue"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Try_Push_Try_Pop")
         ));
      end if;

      return Void_Result.Ok (True);
   end Test_Try_Push_Try_Pop;

   function Test_Try_Operations return Void_Result.Result is
      Queue : Lock_Free_Queue_Type;
      Value : Integer;
   begin
      Initialize (Queue);

--   Try dequeue on empty queue
      if Try_Pop (Queue, Value) then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Try_Pop should fail on empty queue"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Try_Operations")
         ));
      end if;

--   Try enqueue
      if not Try_Push (Queue, 100) then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Try_Push should succeed on non-full queue"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Try_Operations")
         ));
      end if;

--   Try dequeue
      if not Try_Pop (Queue, Value) then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Try_Pop should succeed on non-empty queue"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Try_Operations")
         ));
      end if;

      if Value /= 100 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Try_Popd value should match enqueued value"),
            Details     => To_Unbounded_String ("Expected: 100, Got: " & Value'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Try_Operations")
         ));
      end if;

      return Void_Result.Ok (True);
   end Test_Try_Operations;

   function Test_FIFO_Order return Void_Result.Result is
      Queue : Lock_Free_Queue_Type;
      Values : constant array (1 .. 5) of Integer := [10, 20, 30, 40, 50];
   begin
      Initialize (Queue);

--   Try_Push values
      for V of Values loop
         if not Try_Push (Queue, V) then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Push should succeed"),
               Details     => To_Unbounded_String ("Value: " & V'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_FIFO_Order")
            ));
         end if;
      end loop;

--   Try_Pop and verify order
      for I in Values'Range loop
         declare
            Result : Integer;
         begin
            if not Try_Pop (Queue, Result) then
               return Void_Result.Err (Test_Error'(
                  Kind        => Assertion_Failed,
                  Message     => To_Unbounded_String ("Pop should succeed"),
                  Details     => To_Unbounded_String ("Index: " & I'Image),
                  Line_Number => 0,
                  Test_Name   => To_Unbounded_String ("Test_FIFO_Order")
               ));
            end if;

            if Result /= Values (I) then
               return Void_Result.Err (Test_Error'(
                  Kind        => Assertion_Failed,
                  Message     => To_Unbounded_String ("FIFO order not maintained"),
                  Details     => To_Unbounded_String ("Expected: " & Values (I)'Image &
                                                    ", Got: " & Result'Image),
                  Line_Number => 0,
                  Test_Name   => To_Unbounded_String ("Test_FIFO_Order")
               ));
            end if;
         end;
      end loop;

      return Void_Result.Ok (True);
   end Test_FIFO_Order;

   function Test_Full_Queue return Void_Result.Result is
      Queue : Lock_Free_Queue_Type;
   begin
      Initialize (Queue);

--   Fill the queue (ring buffer can hold Buffer_Size - 1 items)
      for I in 1 .. Small_Size - 1 loop
         if not Try_Push (Queue, I) then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Try_Push should succeed until queue is full"),
               Details     => To_Unbounded_String ("Failed at: " & I'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Full_Queue")
            ));
         end if;
      end loop;

--   Try to enqueue one more
      if Try_Push (Queue, 999) then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Try_Push should fail on full queue"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Full_Queue")
         ));
      end if;

--   Try_Pop one and enqueue again
      declare
         Temp : Integer;
      begin
         if not Try_Pop (Queue, Temp) then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Try_Pop should succeed on full queue"),
               Details     => Null_Unbounded_String,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Full_Queue")
            ));
         end if;

         if not Try_Push (Queue, 999) then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Try_Push should succeed after dequeue"),
               Details     => Null_Unbounded_String,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Full_Queue")
            ));
         end if;
      end;

      return Void_Result.Ok (True);
   end Test_Full_Queue;

--   ==========================================================================
--   Test Runner
--   ==========================================================================

   function Run_All_Tests
     (Output : access Test_Output_Port'Class) return Test_Stats_Result.Result
   is
      Test_Count : constant := 5;
      Tests : Test_Results_Array (1 .. Test_Count);

      procedure Run_Single_Test
        (Index : Positive;
         Name : String;
         Func : Test_Function_Access) is
         Result : constant Test_Result_Pkg.Result := Run_Test (Name, Func, Output);
      begin
         if Result.Is_Ok then
            Tests (Index) := Result.Get_Ok;
            Print_Test_Result (Tests (Index), Output);
         else
--  For simplicity, we'll create a failed test result
            Tests (Index) := Test_Result'(
               Name => To_Unbounded_String (Name),
               Status => Error,
               Message => To_Unbounded_String ("Test execution failed"),
               Elapsed_Time => 0.0,
               Line_Number => 0,
               Correlation_ID => Null_Unbounded_String
            );
            Print_Test_Result (Tests (Index), Output);
         end if;
      end Run_Single_Test;
   begin
      Output.Write_Line ("=== Running Lock-Free Queue Adapter Tests ===");

      Run_Single_Test (1, "Create Queue", Test_Create_Queue'Access);
      Run_Single_Test (2, "Try_Push Try_Pop", Test_Try_Push_Try_Pop'Access);
      Run_Single_Test (3, "Try Operations", Test_Try_Operations'Access);
      Run_Single_Test (4, "FIFO Order", Test_FIFO_Order'Access);
      Run_Single_Test (5, "Full Queue", Test_Full_Queue'Access);

      declare
         Stats_Result : constant Test_Stats_Result.Result :=
            Run_Test_Suite ("Lock-Free Queue Adapter", Tests, Output);
      begin
         if Stats_Result.Is_Ok then
            declare
               Stats : constant Test_Statistics := Stats_Result.Get_Ok;
            begin
               Output.Write_Line ("");
               Print_Test_Summary ("Lock-Free Queue Adapter Tests", Stats, Output);
               return Test_Stats_Result.Ok (Stats);
            end;
         else
            return Stats_Result;
         end if;
      end;
   end Run_All_Tests;

end Test_Lock_Free_Queue_Adapter;

pragma Warnings (On, "subprogram body has no previous spec");
