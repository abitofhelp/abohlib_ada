--   =============================================================================
--   Test_Simple_Validator - Unit Tests for Simple Validator Use Case
--   =============================================================================
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--
--   Purpose:
--     Unit tests for the Simple_Validator use case to increase application
--     layer test coverage.
--   =============================================================================

pragma Ada_2022;

with Abohlib.Infrastructure.Testing.Test_Framework;

package Test_Simple_Validator is

   use Abohlib.Infrastructure.Testing.Test_Framework;

--   Test suite entry point
   function Run_All_Tests
     (Output : access Test_Output_Port'Class) return Test_Stats_Result.Result;

private

--   Length validation tests
   function Test_Valid_Length return Void_Result.Result;
   function Test_Too_Short_Length return Void_Result.Result;
   function Test_Too_Long_Length return Void_Result.Result;

--   Alphanumeric validation tests
   function Test_Valid_Alphanumeric return Void_Result.Result;
   function Test_Invalid_Special_Characters return Void_Result.Result;

--   String sanitization tests
   function Test_Sanitize_Clean_String return Void_Result.Result;
   function Test_Sanitize_With_Special_Chars return Void_Result.Result;

--   Batch validation tests
   function Test_Batch_Validation_Success return Void_Result.Result;
   function Test_Batch_Validation_Failure return Void_Result.Result;

--   Safety check tests
   function Test_Safe_String_Check return Void_Result.Result;

end Test_Simple_Validator;
