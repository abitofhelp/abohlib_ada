--   =============================================================================
--   Test_Retry_Handler - Unit tests for Generic Retry Handler
--   =============================================================================
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--
--   Purpose:
--     Unit tests for the generic retry handler infrastructure component
--   =============================================================================

pragma Ada_2022;

with Abohlib.Infrastructure.Testing.Test_Framework;

package Test_Retry_Handler_Unit is

   use Abohlib.Infrastructure.Testing.Test_Framework;

--   Test suite entry point
   function Run_All_Tests
     (Output : access Test_Output_Port'Class) return Test_Stats_Result.Result;

private

--   Individual test functions
   function Test_Successful_Operation_No_Retry return Void_Result.Result;
   function Test_Eventually_Successful_Operation return Void_Result.Result;
   function Test_Always_Failing_Operation return Void_Result.Result;
   function Test_Exponential_Backoff_Strategy return Void_Result.Result;
   function Test_Linear_Backoff_Strategy return Void_Result.Result;
   function Test_Fixed_Delay_Strategy return Void_Result.Result;
   function Test_Retry_Policy_Builder return Void_Result.Result;
   function Test_Custom_Retry_Predicate return Void_Result.Result;

end Test_Retry_Handler_Unit;
