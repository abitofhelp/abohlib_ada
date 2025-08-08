--  =============================================================================
--  Abohlib.Infrastructure.Testing.Test_Framework - Comprehensive Testing Utils
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Purpose:
--    Comprehensive testing framework providing utilities for unit and integration
--    testing with Result-based error handling, performance benchmarking,
--    memory testing, and contract verification.
--
--  Key Features:
--    - Result-based test execution (no exceptions)
--    - Automatic timing and performance metrics
--    - Test organization with suites and summaries
--    - Pluggable output adapters (console, file, etc.)
--    - Contract validation helpers
--    - Memory leak detection support
--
--  Quick Start Example:
--    -- Define your test function
--    function Test_Addition return Void_Result.Result is
--    begin
--       if 2 + 2 = 4 then
--          return Void_Result.Ok (True);
--       else
--          return Void_Result.Err (Test_Error'(
--             Kind => Assertion_Failed,
--             Message => To_Unbounded_String ("Math is broken!"),
--             ...));
--       end if;
--    end Test_Addition;
--
--    -- Run it
--    Result := Run_Test ("Test_Addition", Test_Addition'Access, Output);
--
--  Design:
--    - Follows hybrid architecture (DDD/Clean/Hexagonal)
--    - Uses Result pattern for all operations that can fail
--    - No exceptions cross architectural boundaries
--    - Dependency Inversion Principle applied through ports
--    - Type-safe with comprehensive contracts
--  =============================================================================

pragma Ada_2022;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Streams;
with Abohlib.Core.Domain.Result;

package Abohlib.Infrastructure.Testing.Test_Framework is

   --  ==========================================================================
   --  Test Result Types
   --  ==========================================================================

   --  Possible outcomes of a test execution
   type Test_Status is (
      Passed,   -- Test completed successfully
      Failed,   -- Test assertion failed
      Skipped,  -- Test was skipped (e.g., platform-specific)
      Error     -- Unexpected error during test execution
   );

   --  Complete information about a test execution
   type Test_Result is record
      Name           : Unbounded_String;  -- Test function name
      Status         : Test_Status;       -- Outcome
      Message        : Unbounded_String;  -- Error/failure details
      Elapsed_Time   : Duration;          -- Execution time
      Line_Number    : Natural;           -- Where failure occurred
      Correlation_ID : Unbounded_String;  -- For tracking across systems
   end record;

   type Test_Results_Array is array (Positive range <>) of Test_Result;

   --  ==========================================================================
   --  Test Statistics
   --  ==========================================================================

   --  Aggregate statistics for a test suite run
   type Test_Statistics is record
      Total_Tests    : Natural := 0;  -- Total number of tests executed
      Passed_Tests   : Natural := 0;
      Failed_Tests   : Natural := 0;
      Skipped_Tests  : Natural := 0;
      Error_Tests    : Natural := 0;
      Total_Duration : Duration := 0.0;
   end record;

   --  ==========================================================================
   --  Test Errors
   --  ==========================================================================

   --  Categories of test failures
   type Test_Error_Kind is (
      Assertion_Failed,      -- Test assertion was false
      Setup_Failed,          -- Test setup/initialization failed
      Cleanup_Failed,        -- Test cleanup/teardown failed
      Timeout_Exceeded,      -- Test took too long to execute
      Memory_Leak_Detected,  -- Memory wasn't properly freed
      Unexpected_Exception   -- Unhandled exception during test
   );

   --  Detailed error information for test failures
   type Test_Error is record
      Kind        : Test_Error_Kind;      -- Type of failure
      Message     : Unbounded_String;     -- Primary error message
      Details     : Unbounded_String;     -- Additional context
      Line_Number : Natural;              -- Source line of failure
      Test_Name   : Unbounded_String;     -- Which test failed
   end record;

   --  Result types for test operations
   package Test_Result_Pkg is new
     Abohlib.Core.Domain.Result.Result_Package
       (Ok_Type  => Test_Result,
        Err_Type => Test_Error);

   package Test_Stats_Result is new
     Abohlib.Core.Domain.Result.Result_Package
       (Ok_Type  => Test_Statistics,
        Err_Type => Test_Error);

   package Void_Result is new
     Abohlib.Core.Domain.Result.Result_Package
       (Ok_Type  => Boolean,  -- Dummy type for void operations
        Err_Type => Test_Error);

   --  ==========================================================================
   --  Test Framework Port (for dependency injection)
   --  ==========================================================================

   --  Output abstraction allows tests to write to console, files, etc.
   --  Implement this interface to create custom test reporters
   type Test_Output_Port is interface;

   --  Write normal test output (results, progress)
   procedure Write_Line (Self : Test_Output_Port; Message : String)
   is abstract;

   --  Write error output (failures, exceptions)
   procedure Write_Error (Self : Test_Output_Port; Message : String)
   is abstract;

   --  ==========================================================================
   --  Assertion Procedures (Result-based)
   --  ==========================================================================
   --
   --  All assertions return Result types instead of raising exceptions
   --  This allows tests to continue after failures and collect all errors
   --
   --  Usage pattern:
   --    Result := Assert_True (X > 0, "X must be positive");
   --    if Result.Is_Err then
   --       return Result;  -- Propagate the error
   --    end if;

   --  Assert that a condition is true
   function Assert_True
     (Condition : Boolean;        -- The condition to test
      Message : String;           -- Error message if false  
      Line : Natural := 0)        -- Optional line number
      return Void_Result.Result
   with
     Post =>
       (if Condition then Assert_True'Result.Is_Ok
        else Assert_True'Result.Is_Err);

   --  Assert that a condition is false
   function Assert_False
     (Condition : Boolean; Message : String; Line : Natural := 0)
      return Void_Result.Result
   with
     Post =>
       (if not Condition then Assert_False'Result.Is_Ok
        else Assert_False'Result.Is_Err);

   function Assert_Equal
     (Expected : Integer;
      Actual   : Integer;
      Message  : String;
      Line     : Natural := 0) return Void_Result.Result
   with
     Post =>
       (if Expected = Actual then Assert_Equal'Result.Is_Ok
        else Assert_Equal'Result.Is_Err);

   function Assert_Equal
     (Expected : String;
      Actual   : String;
      Message  : String;
      Line     : Natural := 0) return Void_Result.Result
   with
     Post =>
       (if Expected = Actual then Assert_Equal'Result.Is_Ok
        else Assert_Equal'Result.Is_Err);

   function Assert_Equal
     (Expected  : Duration;
      Actual    : Duration;
      Tolerance : Duration;
      Message   : String;
      Line      : Natural := 0) return Void_Result.Result
   with
     Pre => Tolerance >= 0.0,
     Post =>
       (if abs (Expected - Actual) <= Tolerance then Assert_Equal'Result.Is_Ok
        else Assert_Equal'Result.Is_Err);

   function Assert_Not_Equal
     (Unexpected : Integer;
      Actual     : Integer;
      Message    : String;
      Line       : Natural := 0) return Void_Result.Result
   with
     Post =>
       (if Unexpected /= Actual then Assert_Not_Equal'Result.Is_Ok
        else Assert_Not_Equal'Result.Is_Err);

   function Assert_Greater_Than
     (Expected : Integer;
      Actual   : Integer;
      Message  : String;
      Line     : Natural := 0) return Void_Result.Result
   with
     Post =>
       (if Actual > Expected then Assert_Greater_Than'Result.Is_Ok
        else Assert_Greater_Than'Result.Is_Err);

   function Assert_Less_Than
     (Expected : Integer;
      Actual   : Integer;
      Message  : String;
      Line     : Natural := 0) return Void_Result.Result
   with
     Post =>
       (if Actual < Expected then Assert_Less_Than'Result.Is_Ok
        else Assert_Less_Than'Result.Is_Err);

   --  ==========================================================================
   --  Stream Element Array Assertions
   --  ==========================================================================

   function Assert_Arrays_Equal
     (Expected : Ada.Streams.Stream_Element_Array;
      Actual   : Ada.Streams.Stream_Element_Array;
      Message  : String;
      Line     : Natural := 0) return Void_Result.Result;

   --  ==========================================================================
   --  Result Type Assertions
   --  ==========================================================================

   generic
      type T is private;
      type E is private;
      with package Result_Type is new
        Abohlib.Core.Domain.Result.Result_Package (T, E);
   function Generic_Assert_Result_Ok
     (Result : Result_Type.Result; Message : String; Line : Natural := 0)
      return Void_Result.Result;

   generic
      type T is private;
      type E is private;
      with package Result_Type is new
        Abohlib.Core.Domain.Result.Result_Package (T, E);
   function Generic_Assert_Result_Err
     (Result : Result_Type.Result; Message : String; Line : Natural := 0)
      return Void_Result.Result;

   --  ==========================================================================
   --  Test Execution Framework
   --  ==========================================================================

   type Test_Procedure_Access is access procedure;
   type Test_Function_Access is access function return Void_Result.Result;

   function Run_Test
     (Test_Name : String;
      Test_Func : Test_Function_Access;
      Output    : access Test_Output_Port'Class) return Test_Result_Pkg.Result
   with Pre => Test_Name'Length > 0;

   function Run_Test_Suite
     (Suite_Name : String;
      Tests      : Test_Results_Array;
      Output     : access Test_Output_Port'Class)
      return Test_Stats_Result.Result
   with Pre => Suite_Name'Length > 0 and Tests'Length > 0;

   --  ==========================================================================
   --  Test Reporting
   --  ==========================================================================

   procedure Print_Test_Result
     (Result : Test_Result; Output : access Test_Output_Port'Class)
   with Pre => Result.Name /= Null_Unbounded_String;

   procedure Print_Test_Statistics
     (Statistics : Test_Statistics; Output : access Test_Output_Port'Class);

   procedure Print_Test_Summary
     (Suite_Name : String;
      Statistics : Test_Statistics;
      Output     : access Test_Output_Port'Class)
   with Pre => Suite_Name'Length > 0;

   --  ==========================================================================
   --  Performance Testing Utilities
   --  ==========================================================================

   type Performance_Benchmark is record
      Name           : Unbounded_String;
      Iterations     : Positive;
      Total_Duration : Duration;
      Min_Duration   : Duration;
      Max_Duration   : Duration;
      Avg_Duration   : Duration;
   end record;

   package Benchmark_Result is new
     Abohlib.Core.Domain.Result.Result_Package
       (Ok_Type  => Performance_Benchmark,
        Err_Type => Test_Error);

   function Benchmark_Operation
     (Name : String; Iterations : Positive; Operation : Test_Procedure_Access)
      return Benchmark_Result.Result
   with Pre => Name'Length > 0 and Iterations > 0;

   procedure Print_Benchmark_Result
     (Benchmark : Performance_Benchmark;
      Output    : access Test_Output_Port'Class);

   --  ==========================================================================
   --  Memory Testing Utilities
   --  ==========================================================================

   function Check_Memory_Leaks
     (Test_Name : String; Test_Proc : Test_Procedure_Access)
      return Void_Result.Result
   with Pre => Test_Name'Length > 0;

   --  ==========================================================================
   --  Test Data Generation
   --  ==========================================================================

   function Generate_Test_File
     (File_Path : String; Size_MB : Positive; Pattern : String := "ABCDEFGH")
      return Void_Result.Result
   with Pre => File_Path'Length > 0 and Size_MB > 0 and Pattern'Length > 0;

   function Generate_Binary_Test_File
     (File_Path : String; Size_MB : Positive) return Void_Result.Result
   with Pre => File_Path'Length > 0 and Size_MB > 0;

   function Cleanup_Test_File (File_Path : String) return Void_Result.Result
   with Pre => File_Path'Length > 0;

   --  ==========================================================================
   --  Test Framework Service (main entry point)
   --  ==========================================================================

   type Test_Framework_Service
     (Output : not null access Test_Output_Port'Class)
   is
     tagged limited private;

   function Create
     (Output : not null access Test_Output_Port'Class)
      return Test_Framework_Service;

   --  Run a single test
   function Run_Test
     (Self      : Test_Framework_Service;
      Test_Name : String;
      Test_Func : Test_Function_Access) return Test_Result_Pkg.Result
   with Pre => Test_Name'Length > 0;

   --  Run a test suite
   function Run_Suite
     (Self       : Test_Framework_Service;
      Suite_Name : String;
      Tests      : Test_Results_Array) return Test_Stats_Result.Result
   with Pre => Suite_Name'Length > 0 and Tests'Length > 0;

private

   type Test_Framework_Service
     (Output : not null access Test_Output_Port'Class)
   is tagged limited null record;

end Abohlib.Infrastructure.Testing.Test_Framework;
