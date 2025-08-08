--   =============================================================================
--   Test_Generic_Stage - Test Implementation (Simple)
--   =============================================================================
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--   =============================================================================

pragma Ada_2022;
pragma Warnings (Off, "subprogram body has no previous spec");

--   with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Abohlib.Core.Domain.Services.Pipeline;
with Abohlib.Core.Domain.Services.Pipeline.Generic_Stage;

package body Test_Pipeline_Generic_Stage is

--      use Abohlib.Infrastructure.Testing.Test_Framework;

--   ==========================================================================
--   Test Stage Implementation
--   ==========================================================================

--   Simple test stage that doubles integers
   type Test_Config is record
      Multiplier : Integer := 2;
      Fail_On_Negative : Boolean := False;
   end record;

   type Test_State is record
      Process_Count : Natural := 0;
      Error_Count   : Natural := 0;
   end record;

   function Double_Integer (
      State  : in out Test_State;
      Config : Test_Config;
      Input  : Integer) return Integer
   is
   begin
      State.Process_Count := State.Process_Count + 1;

--   Simulate error for negative inputs if configured
      if Config.Fail_On_Negative and Input < 0 then
         State.Error_Count := State.Error_Count + 1;
         raise Constraint_Error with "Negative input not allowed";
      end if;

      return Input * Config.Multiplier;
   end Double_Integer;

   function Init_Test_State (Config : Test_Config) return Test_State is
      pragma Unreferenced (Config);
   begin
      return Test_State'(Process_Count => 0, Error_Count => 0);
   end Init_Test_State;

   function Is_Valid_Integer (I : Integer) return Boolean is
     (I >= -1000 and I <= 1000);

   function Is_Valid_Test_State (S : Test_State) return Boolean is
     (S.Process_Count <= 1000 and S.Error_Count <= 100);

--   Instantiate the generic stage
   pragma Warnings (Off, "check will fail at run time");
   package Integer_Doubler is new
      Abohlib.Core.Domain.Services.Pipeline.Generic_Stage
        (Input_Type       => Integer,
         Output_Type      => Integer,
         State_Type       => Test_State,
         Config_Type      => Test_Config,
         Stage_Name       => "Integer Doubler",
         Process_Element  => Double_Integer,
         Initialize_State => Init_Test_State,
         Is_Valid_Input   => Is_Valid_Integer,
         Is_Valid_Output  => Is_Valid_Integer,
         Is_Valid_State   => Is_Valid_Test_State,
         Enable_Statistics => True,
         Enable_Batch      => True,
         Enable_Parallel   => False);
   pragma Warnings (On, "check will fail at run time");

   use Integer_Doubler;

--   Simplify access to the result package
   package Stage_Result renames Integer_Doubler.Stage_Interface_Pkg.Stage_Result;

--   Parallel-enabled stage
   package Parallel_Integer_Doubler is new
      Abohlib.Core.Domain.Services.Pipeline.Generic_Stage
        (Input_Type       => Integer,
         Output_Type      => Integer,
         State_Type       => Test_State,
         Config_Type      => Test_Config,
         Stage_Name       => "Parallel Integer Doubler",
         Process_Element  => Double_Integer,
         Initialize_State => Init_Test_State,
         Is_Valid_Input   => Is_Valid_Integer,
         Is_Valid_Output  => Is_Valid_Integer,
         Is_Valid_State   => Is_Valid_Test_State,
         Enable_Statistics => True,
         Enable_Batch      => True,
         Enable_Parallel   => True);

   package Parallel_Stage_Result renames Parallel_Integer_Doubler.Stage_Interface_Pkg.Stage_Result;

--   ==========================================================================
--   Procedure Specifications
--   ==========================================================================

   function Test_Stage_Creation return Void_Result.Result;
   function Test_Basic_Processing return Void_Result.Result;
   function Test_Error_Handling return Void_Result.Result;
   function Test_Batch_Processing return Void_Result.Result;
   function Test_Statistics_Tracking return Void_Result.Result;
   function Test_Parallel_Stage_Creation return Void_Result.Result;
   function Test_Parallel_Basic_Processing return Void_Result.Result;

--   ==========================================================================
--   Test Functions
--   ==========================================================================

   function Test_Stage_Creation return Void_Result.Result is
      Config : constant Test_Config := (Multiplier => 2, Fail_On_Negative => False);
      Stage : constant Pipeline_Stage := Create (Config);
   begin
      return Assert_True (Stage.Is_Initialized, "Stage should be initialized");
   end Test_Stage_Creation;

   function Test_Basic_Processing return Void_Result.Result is
      Config : constant Test_Config := (Multiplier => 3, Fail_On_Negative => False);
      Stage : Pipeline_Stage := Create (Config);
      Result : constant Stage_Result.Result := Stage.Process (10);
   begin
      if not Stage_Result.Is_Ok (Result) then
         return Assert_True (False, "Processing should succeed");
      end if;

      return Assert_Equal (30, Stage_Result.Get_Ok (Result), "10 * 3 = 30");
   end Test_Basic_Processing;

   function Test_Error_Handling return Void_Result.Result is
      Config : constant Test_Config := (Multiplier => 2, Fail_On_Negative => True);
      Stage : Pipeline_Stage := Create (Config);
      Result : constant Stage_Result.Result := Stage.Process (-5);
   begin
      return Assert_True (not Stage_Result.Is_Ok (Result), "Negative input should fail");
   end Test_Error_Handling;

   function Test_Batch_Processing return Void_Result.Result is
      Config : constant Test_Config := (Multiplier => 2, Fail_On_Negative => False);
      Stage : Pipeline_Stage := Create (Config);
      Test_Inputs : constant Input_Array := [1, 2, 3];
      Outputs : Output_Array (Test_Inputs'Range);
      Success : Success_Array (Test_Inputs'Range);
      Errors : Error_Array (Test_Inputs'Range);
   begin
      Stage.Process_Batch (Test_Inputs, Outputs, Success, Errors);

      for I in Test_Inputs'Range loop
         if not Success (I) then
            return Assert_True (False, "Batch processing should succeed for all inputs");
         end if;
      end loop;

      return Void_Result.Ok (True);
   end Test_Batch_Processing;

   function Test_Statistics_Tracking return Void_Result.Result is
      Config : constant Test_Config := (Multiplier => 2, Fail_On_Negative => False);
      Stage : Pipeline_Stage := Create (Config);
      Initial_Count : constant Natural := Stage.Items_Processed;
      Result : constant Stage_Result.Result := Stage.Process (5);
      pragma Unreferenced (Result);
   begin
      return Assert_Equal (Initial_Count + 1, Stage.Items_Processed, "Item count should increment");
   end Test_Statistics_Tracking;

--   ==========================================================================
--   Parallel Processing Tests
--   ==========================================================================

   function Test_Parallel_Stage_Creation return Void_Result.Result is
      Config : constant Test_Config := (Multiplier => 2, Fail_On_Negative => False);
      Stage : constant Parallel_Integer_Doubler.Pipeline_Stage := Parallel_Integer_Doubler.Create (Config);
   begin
      return Assert_True (Stage.Is_Initialized, "Parallel stage should be initialized");
   end Test_Parallel_Stage_Creation;

   function Test_Parallel_Basic_Processing return Void_Result.Result is
      Config : constant Test_Config := (Multiplier => 3, Fail_On_Negative => False);
      Stage : Parallel_Integer_Doubler.Pipeline_Stage := Parallel_Integer_Doubler.Create (Config);
      Result : constant Parallel_Stage_Result.Result := Stage.Process (10);
   begin
      if not Parallel_Stage_Result.Is_Ok (Result) then
         return Assert_True (False, "Parallel processing should succeed");
      end if;

      return Assert_Equal (30, Parallel_Stage_Result.Get_Ok (Result), "10 * 3 = 30 (parallel)");
   end Test_Parallel_Basic_Processing;

--   ==========================================================================
--   Run All Tests
--   ==========================================================================

   function Run_All_Tests
     (Output : access Test_Output_Port'Class) return Test_Stats_Result.Result
   is
      Tests : Test_Results_Array (1 .. 7);  -- Updated for 7 tests
      Index : Positive := 1;

      procedure Add_Test_Result
        (Name : String;
         Test_Func : Test_Function_Access)
      is
         Result : constant Test_Result_Pkg.Result :=
            Run_Test (Name, Test_Func, Output);
      begin
         if Result.Is_Ok then
            Tests (Index) := Result.Get_Ok;
            Print_Test_Result (Tests (Index), Output);
            Index := Index + 1;
         else
            Output.Write_Error ("Failed to run test: " & Name);
         end if;
      end Add_Test_Result;

   begin
      Output.Write_Line ("=== Running Pipeline Generic Stage Unit Tests ===");
      Output.Write_Line ("");

--   Run all tests
      Add_Test_Result ("Test_Stage_Creation", Test_Stage_Creation'Access);
      Add_Test_Result ("Test_Basic_Processing", Test_Basic_Processing'Access);
      Add_Test_Result ("Test_Error_Handling", Test_Error_Handling'Access);
      Add_Test_Result ("Test_Batch_Processing", Test_Batch_Processing'Access);
      Add_Test_Result ("Test_Statistics", Test_Statistics_Tracking'Access);
      Output.Write_Line ("DEBUG: Completed 5 tests, Index=" & Index'Image);

--   Parallel processing tests
      Output.Write_Line ("DEBUG: About to add parallel tests");
      Add_Test_Result ("Test_Parallel_Stage_Creation", Test_Parallel_Stage_Creation'Access);
      Output.Write_Line ("DEBUG: Added first parallel test, Index=" & Index'Image);
      Add_Test_Result ("Test_Parallel_Basic_Processing", Test_Parallel_Basic_Processing'Access);
      Output.Write_Line ("DEBUG: Added second parallel test, Index=" & Index'Image);

--   Generate summary
      Output.Write_Line ("DEBUG: About to run test suite with Index=" & Index'Image);
      declare
         Stats_Result : constant Test_Stats_Result.Result :=
            Run_Test_Suite ("Pipeline_Generic_Stage_Tests", Tests (1 .. Index - 1), Output);
      begin
         if Stats_Result.Is_Ok then
            declare
               Stats : constant Test_Statistics := Stats_Result.Get_Ok;
            begin
               Output.Write_Line ("");
               Print_Test_Summary ("Pipeline Generic Stage Unit Tests", Stats, Output);
               return Test_Stats_Result.Ok (Stats);
            end;
         else
            return Stats_Result;
         end if;
      end;
   end Run_All_Tests;

end Test_Pipeline_Generic_Stage;

pragma Warnings (On, "subprogram body has no previous spec");
