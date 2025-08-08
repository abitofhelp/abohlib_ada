--   =============================================================================
--   Test_Result - Unit tests for Result<T,E> Pattern
--   =============================================================================
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--
--   Purpose:
--     Unit tests for the Result generic package that provides functional
--     error handling without exceptions.
--   =============================================================================

pragma Ada_2022;

with Abohlib.Infrastructure.Testing.Test_Framework;

package Test_Result is

   use Abohlib.Infrastructure.Testing.Test_Framework;

--   Test suite entry point
   function Run_All_Tests
     (Output : access Test_Output_Port'Class) return Test_Stats_Result.Result;

private

--   Test creating Ok results
   function Test_Ok_Creation return Void_Result.Result;

--   Test creating Err results
   function Test_Err_Creation return Void_Result.Result;

--   Test Is_Ok and Is_Err predicates
   function Test_Result_Predicates return Void_Result.Result;

--   Test unwrapping Ok values
   function Test_Unwrap_Ok return Void_Result.Result;

--   Test unwrapping Err values
   function Test_Unwrap_Err return Void_Result.Result;

--   Test Get_Ok and Get_Err accessors
   function Test_Get_Accessors return Void_Result.Result;

--   Test Map function for transforming Ok values
   function Test_Map_Function return Void_Result.Result;

--   Test Map_Err function for transforming errors
   function Test_Map_Err_Function return Void_Result.Result;

--   Test And_Then for chaining operations
   function Test_And_Then_Chaining return Void_Result.Result;

--   Test Or_Else for error recovery
   function Test_Or_Else_Recovery return Void_Result.Result;

end Test_Result;
