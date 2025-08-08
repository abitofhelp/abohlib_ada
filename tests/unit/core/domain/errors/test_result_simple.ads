--   =============================================================================
--   Test_Result_Simple - Simple unit tests for Result pattern
--   =============================================================================
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--
--   Purpose:
--     Simple unit tests for the Result<T,E> pattern using current APIs
--   =============================================================================

pragma Ada_2022;

with Abohlib.Infrastructure.Testing.Test_Framework;

package Test_Result_Simple is

   use Abohlib.Infrastructure.Testing.Test_Framework;

--   Test suite entry point
   function Run_All_Tests
     (Output : access Test_Output_Port'Class) return Test_Stats_Result.Result;

private

--   Individual test functions
   function Test_Ok_Creation return Void_Result.Result;
   function Test_Err_Creation return Void_Result.Result;
   function Test_Get_Ok_Or return Void_Result.Result;
   function Test_Get_Err_Or return Void_Result.Result;
   function Test_Result_Pattern_Matching return Void_Result.Result;

end Test_Result_Simple;
