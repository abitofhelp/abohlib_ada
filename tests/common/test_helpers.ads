--   =============================================================================
--   Test_Helpers - Common Test Helper Procedures
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--   =============================================================================

pragma Ada_2022;

with Abohlib.Infrastructure.Testing.Test_Framework;

package Test_Helpers is

   use Abohlib.Infrastructure.Testing.Test_Framework;

--   Simple test statistics tracking
   type Test_Stats is record
      Total  : Natural := 0;
      Passed : Natural := 0;
      Failed : Natural := 0;
   end record;

--   Function pointer type for test functions
   type Test_Function is access function return Void_Result.Result;

--   Run a single test and update statistics
   procedure Run_Test
     (Name   : String;
      Test   : Test_Function;
      Output : access Test_Output_Port'Class;
      Stats  : in out Test_Stats);

--   Alternative signature to match usage pattern
   procedure Run_Test
     (Output : access Test_Output_Port'Class;
      Stats  : in out Test_Stats;
      Name   : String;
      Test   : Test_Function);

--   Print test summary
   procedure Print_Test_Summary
     (Title  : String;
      Stats  : Test_Stats;
      Output : access Test_Output_Port'Class);

--   Alternative signatures to match usage patterns
   procedure Print_Summary
     (Output : access Test_Output_Port'Class;
      Stats  : Test_Stats;
      Title  : String);

end Test_Helpers;