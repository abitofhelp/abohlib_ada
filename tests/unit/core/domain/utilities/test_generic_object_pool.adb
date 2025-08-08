--   =============================================================================
--   Test_Generic_Object_Pool - Unit test implementation
--   =============================================================================
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--   =============================================================================

pragma Ada_2022;
pragma Warnings (Off, "subprogram body has no previous spec");

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Abohlib.Core.Domain.Utilities.Generic_Object_Pool;
with Abohlib.Core.Domain.Types.Counts;
use Abohlib.Core.Domain.Types.Counts;

package body Test_Generic_Object_Pool is

--   ==========================================================================
--   Test Types
--   ==========================================================================

--   Simple test object with state that needs resetting
   type Test_Object is record
      Id        : Natural := 0;
      Value     : Integer := 0;
      Is_Active : Boolean := False;
      Name      : Unbounded_String := Null_Unbounded_String;
   end record;

   type Test_Object_Access is access all Test_Object;

--   Reset procedure for test objects
   procedure Reset_Test_Object (Obj : in out Test_Object) is
   begin
      Obj.Id := 0;
      Obj.Value := 0;
      Obj.Is_Active := False;
      Obj.Name := Null_Unbounded_String;
   end Reset_Test_Object;

--   Instantiate pools with different configurations
   package Small_Pool is new Abohlib.Core.Domain.Utilities.Generic_Object_Pool
     (Object_Type   => Test_Object,
      Object_Access => Test_Object_Access,
      Reset_Object  => Reset_Test_Object,
      Initial_Size  => 5,
      Max_Size      => 10);

   package Large_Pool is new Abohlib.Core.Domain.Utilities.Generic_Object_Pool
     (Object_Type   => Test_Object,
      Object_Access => Test_Object_Access,
      Reset_Object  => Reset_Test_Object,
      Initial_Size  => 1,    -- Minimal pre-allocation
      Max_Size      => 100);

--   ==========================================================================
--   Test Functions
--   ==========================================================================

   function Test_Pool_Creation return Void_Result.Result is
      Pool : Small_Pool.Object_Pool;
   begin
--   Check initial state
      if Small_Pool.Available_Count (Pool) /= 5 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String
               ("Pool should have 5 available objects after initialization"),
            Details     => To_Unbounded_String
               ("Got: " & Small_Pool.Available_Count (Pool)'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Pool_Creation")
         ));
      end if;

      if Small_Pool.Allocated_Count (Pool) /= 5 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String
               ("Pool should have allocated 5 objects initially"),
            Details     => To_Unbounded_String
               ("Got: " & Small_Pool.Allocated_Count (Pool)'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Pool_Creation")
         ));
      end if;

      if not Small_Pool.Has_Available (Pool) then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String
               ("Pool should have available objects"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Pool_Creation")
         ));
      end if;

      if Small_Pool.Capacity /= 10 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String
               ("Pool capacity should be 10"),
            Details     => To_Unbounded_String
               ("Got: " & Small_Pool.Capacity'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Pool_Creation")
         ));
      end if;

      return Void_Result.Ok (True);
   end Test_Pool_Creation;

   function Test_Get_And_Return return Void_Result.Result is
      Pool : Small_Pool.Object_Pool;
   begin
--   Get an object from pool
      declare
         Get_Result : constant Small_Pool.Pool_Result.Result := Small_Pool.Get (Pool);
      begin
         if not Small_Pool.Pool_Result.Is_Ok (Get_Result) then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Failed to get object from pool"),
               Details     => Null_Unbounded_String,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Get_And_Return")
            ));
         end if;

         declare
            Obj : Test_Object_Access := Small_Pool.Pool_Result.Get_Ok (Get_Result);
         begin
--   Verify pool state after get
            if Small_Pool.Available_Count (Pool) /= 4 then
               return Void_Result.Err (Test_Error'(
                  Kind        => Assertion_Failed,
                  Message     => To_Unbounded_String
                     ("Available count should decrease after Get"),
                  Details     => To_Unbounded_String
                     ("Got: " & Small_Pool.Available_Count (Pool)'Image),
                  Line_Number => 0,
                  Test_Name   => To_Unbounded_String ("Test_Get_And_Return")
               ));
            end if;

--   Modify object
            Obj.Id := 42;
            Obj.Value := 100;
            Obj.Is_Active := True;
            Obj.Name := To_Unbounded_String ("Test Object");

--   Return to pool
            Small_Pool.Return_To_Pool (Pool, Obj);

--   Verify object was nullified
            if Obj /= null then
               return Void_Result.Err (Test_Error'(
                  Kind        => Assertion_Failed,
                  Message     => To_Unbounded_String
                     ("Object access should be null after return"),
                  Details     => Null_Unbounded_String,
                  Line_Number => 0,
                  Test_Name   => To_Unbounded_String ("Test_Get_And_Return")
               ));
            end if;

--   Verify pool state after return
            if Small_Pool.Available_Count (Pool) /= 5 then
               return Void_Result.Err (Test_Error'(
                  Kind        => Assertion_Failed,
                  Message     => To_Unbounded_String
                     ("Available count should increase after Return"),
                  Details     => To_Unbounded_String
                     ("Got: " & Small_Pool.Available_Count (Pool)'Image),
                  Line_Number => 0,
                  Test_Name   => To_Unbounded_String ("Test_Get_And_Return")
               ));
            end if;
         end;
      end;

      return Void_Result.Ok (True);
   end Test_Get_And_Return;

   function Test_Object_Reset return Void_Result.Result is
      Pool : Small_Pool.Object_Pool;
   begin
--   Get object and modify it
      declare
         Get_Result : constant Small_Pool.Pool_Result.Result := Small_Pool.Get (Pool);
         Obj : Test_Object_Access := Small_Pool.Pool_Result.Get_Ok (Get_Result);
      begin
--   Set values
         Obj.Id := 999;
         Obj.Value := -42;
         Obj.Is_Active := True;
         Obj.Name := To_Unbounded_String ("Modified");

--   Return to pool
         Small_Pool.Return_To_Pool (Pool, Obj);
      end;

--   Get object again and verify it was reset
      declare
         Get_Result : constant Small_Pool.Pool_Result.Result := Small_Pool.Get (Pool);
         Obj : Test_Object_Access := Small_Pool.Pool_Result.Get_Ok (Get_Result);
      begin
         if Obj.Id /= 0 then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Id should be reset to 0"),
               Details     => To_Unbounded_String ("Got: " & Obj.Id'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Object_Reset")
            ));
         end if;

         if Obj.Value /= 0 then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Value should be reset to 0"),
               Details     => To_Unbounded_String ("Got: " & Obj.Value'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Object_Reset")
            ));
         end if;

         if Obj.Is_Active then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Is_Active should be reset to False"),
               Details     => Null_Unbounded_String,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Object_Reset")
            ));
         end if;

         if Obj.Name /= Null_Unbounded_String then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Name should be reset to empty"),
               Details     => To_Unbounded_String ("Got: " & To_String (Obj.Name)),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Object_Reset")
            ));
         end if;

--   Clean up
         Small_Pool.Return_To_Pool (Pool, Obj);
      end;

      return Void_Result.Ok (True);
   end Test_Object_Reset;

   function Test_Pool_Exhaustion return Void_Result.Result is
      Pool : Small_Pool.Object_Pool;
      Objects : array (1 .. 11) of Test_Object_Access := [others => null];
   begin
--   Get all 10 objects (5 pre-allocated + 5 new)
      for I in 1 .. 10 loop
         declare
            Get_Result : constant Small_Pool.Pool_Result.Result := Small_Pool.Get (Pool);
         begin
            if not Small_Pool.Pool_Result.Is_Ok (Get_Result) then
               return Void_Result.Err (Test_Error'(
                  Kind        => Assertion_Failed,
                  Message     => To_Unbounded_String
                     ("Should be able to get " & I'Image & " objects"),
                  Details     => Null_Unbounded_String,
                  Line_Number => 0,
                  Test_Name   => To_Unbounded_String ("Test_Pool_Exhaustion")
               ));
            end if;
            Objects (I) := Small_Pool.Pool_Result.Get_Ok (Get_Result);
         end;
      end loop;

--   Try to get 11th object - should fail
      declare
         Get_Result : constant Small_Pool.Pool_Result.Result := Small_Pool.Get (Pool);
      begin
         if Small_Pool.Pool_Result.Is_Ok (Get_Result) then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String
                  ("Should not be able to get 11th object from pool of 10"),
               Details     => Null_Unbounded_String,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Pool_Exhaustion")
            ));
         end if;
      end;

--   Return all objects
      for I in 1 .. 10 loop
         if Objects (I) /= null then
            Small_Pool.Return_To_Pool (Pool, Objects (I));
         end if;
      end loop;

      return Void_Result.Ok (True);
   end Test_Pool_Exhaustion;

   function Test_Preallocate return Void_Result.Result is
      Pool : Large_Pool.Object_Pool;
   begin
--   Initially 1 object (from Initial_Size)
      if Large_Pool.Available_Count (Pool) /= 1 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String
               ("Large pool should start with 1 object"),
            Details     => To_Unbounded_String
               ("Got: " & Large_Pool.Available_Count (Pool)'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Preallocate")
         ));
      end if;

--   Preallocate 20 objects
      declare
         Result : constant Large_Pool.Count_Result.Result :=
            Large_Pool.Preallocate (Pool, 20);
      begin
         if not Large_Pool.Count_Result.Is_Ok (Result) then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Preallocate should succeed"),
               Details     => Null_Unbounded_String,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Preallocate")
            ));
         end if;

         if Large_Pool.Count_Result.Get_Ok (Result) /= 20 then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Should allocate 20 objects"),
               Details     => To_Unbounded_String
                  ("Got: " & Large_Pool.Count_Result.Get_Ok (Result)'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Preallocate")
            ));
         end if;
      end;

--   Verify pool state (1 initial + 20 preallocated)
      if Large_Pool.Available_Count (Pool) /= 21 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String
               ("Pool should have 21 available after preallocate"),
            Details     => To_Unbounded_String
               ("Got: " & Large_Pool.Available_Count (Pool)'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Preallocate")
         ));
      end if;

      if Large_Pool.Allocated_Count (Pool) /= 21 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String
               ("Pool should have allocated 21 total"),
            Details     => To_Unbounded_String
               ("Got: " & Large_Pool.Allocated_Count (Pool)'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Preallocate")
         ));
      end if;

      return Void_Result.Ok (True);
   end Test_Preallocate;

   function Test_Clear_Available return Void_Result.Result is
      Pool : Small_Pool.Object_Pool;
   begin
--   Initial state - 5 objects
      if Small_Pool.Available_Count (Pool) /= 5 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Should start with 5 objects"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Clear_Available")
         ));
      end if;

--   Clear available objects
      Small_Pool.Clear_Available (Pool);

--   Verify cleared
      if Small_Pool.Available_Count (Pool) /= 0 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String
               ("Available count should be 0 after clear"),
            Details     => To_Unbounded_String
               ("Got: " & Small_Pool.Available_Count (Pool)'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Clear_Available")
         ));
      end if;

      if Small_Pool.Allocated_Count (Pool) /= 0 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String
               ("Allocated count should be 0 after clear"),
            Details     => To_Unbounded_String
               ("Got: " & Small_Pool.Allocated_Count (Pool)'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Clear_Available")
         ));
      end if;

      return Void_Result.Ok (True);
   end Test_Clear_Available;

   function Test_Pool_Overflow_Return return Void_Result.Result is
      Pool : Small_Pool.Object_Pool;
      External_Object : Test_Object_Access;
      Objects : array (1 .. 10) of Test_Object_Access := [others => null];
   begin
--   First, fill the pool completely by getting all 10 objects
      for I in 1 .. 10 loop
         declare
            Result : constant Small_Pool.Pool_Result.Result := Small_Pool.Get (Pool);
         begin
            Objects (I) := Small_Pool.Pool_Result.Get_Ok (Result);
         end;
      end loop;

--   Return all objects to fill the pool's available list
      for I in 1 .. 10 loop
         Small_Pool.Return_To_Pool (Pool, Objects (I));
      end loop;

--   Now pool's available list is full (10 objects)
--   Create an external object
      External_Object := new Test_Object;
      External_Object.Id := 12345;

--   Try to return the external object - it should be deallocated
      Small_Pool.Return_To_Pool (Pool, External_Object);

--   Verify object was nullified
      if External_Object /= null then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String
               ("External object should be nullified after return"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Pool_Overflow_Return")
         ));
      end if;

--   Pool should still have 10 objects (external object was deallocated, not pooled)
      if Small_Pool.Available_Count (Pool) /= 10 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String
               ("Pool should remain at max capacity when full"),
            Details     => To_Unbounded_String
               ("Got: " & Small_Pool.Available_Count (Pool)'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Pool_Overflow_Return")
         ));
      end if;

      return Void_Result.Ok (True);
   end Test_Pool_Overflow_Return;

--   ==========================================================================
--   Test Runner
--   ==========================================================================

   function Run_All_Tests
     (Output : access Test_Output_Port'Class) return Test_Stats_Result.Result
   is
      Test_Count : constant := 7;
      Tests : Test_Results_Array (1 .. Test_Count);
      Index : Positive := 1;

      procedure Run_Single_Test
        (Name : String;
         Func : Test_Function_Access) is
         Result : constant Test_Result_Pkg.Result := Run_Test (Name, Func, Output);
      begin
         if Result.Is_Ok then
            Tests (Index) := Result.Get_Ok;
            Print_Test_Result (Tests (Index), Output);
            Index := Index + 1;
         else
--   Handle error case
            declare
               Error : constant Test_Error := Result.Get_Err;
            begin
               Tests (Index) := Test_Result'(
                  Name           => To_Unbounded_String (Name),
                  Status         => Failed,
                  Message        => Error.Message,
                  Elapsed_Time   => 0.0,
                  Line_Number    => Error.Line_Number,
                  Correlation_ID => To_Unbounded_String ("TEST-" & Name)
               );
               Print_Test_Result (Tests (Index), Output);
               Index := Index + 1;
            end;
         end if;
      end Run_Single_Test;

   begin
      Output.Write_Line ("=== Running Generic Object Pool Tests ===");
      Output.Write_Line ("");

--   Run all tests
      Run_Single_Test ("Pool Creation", Test_Pool_Creation'Access);
      Run_Single_Test ("Get and Return", Test_Get_And_Return'Access);
      Run_Single_Test ("Object Reset", Test_Object_Reset'Access);
      Run_Single_Test ("Pool Exhaustion", Test_Pool_Exhaustion'Access);
      Run_Single_Test ("Preallocate", Test_Preallocate'Access);
      Run_Single_Test ("Clear Available", Test_Clear_Available'Access);
      Run_Single_Test ("Pool Overflow Return", Test_Pool_Overflow_Return'Access);

--   Generate summary
      declare
         Stats_Result : constant Test_Stats_Result.Result :=
            Run_Test_Suite ("Generic Object Pool", Tests (1 .. Index - 1), Output);
      begin
         if Stats_Result.Is_Ok then
            declare
               Stats : constant Test_Statistics := Stats_Result.Get_Ok;
            begin
               Output.Write_Line ("");
               Print_Test_Summary ("Generic Object Pool Tests", Stats, Output);
               return Test_Stats_Result.Ok (Stats);
            end;
         else
            return Stats_Result;
         end if;
      end;
   end Run_All_Tests;

end Test_Generic_Object_Pool;

pragma Warnings (On, "subprogram body has no previous spec");
