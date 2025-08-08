--   =============================================================================
--   Test_Application_Errors - Unit tests for Application Layer Errors
--   =============================================================================
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--
--   Purpose:
--     Unit tests for application layer error types including use case errors
--     and workflow errors.
--   =============================================================================

pragma Ada_2022;

with Abohlib.Infrastructure.Testing.Test_Framework;

package Test_Application_Errors is

   use Abohlib.Infrastructure.Testing.Test_Framework;

--   Test suite entry point
   function Run_All_Tests
     (Output : access Test_Output_Port'Class) return Test_Stats_Result.Result;

private

--   Individual test functions
   function Test_Use_Case_Error_Creation return Void_Result.Result;
   function Test_Use_Case_Error_Default_Messages return Void_Result.Result;
   function Test_Use_Case_Error_Custom_Messages return Void_Result.Result;
   function Test_Use_Case_Error_To_String return Void_Result.Result;
   function Test_Workflow_Error_Creation return Void_Result.Result;
   function Test_Workflow_Error_Default_Messages return Void_Result.Result;
   function Test_Workflow_Error_To_String return Void_Result.Result;
   function Test_Error_Kind_Coverage return Void_Result.Result;

end Test_Application_Errors;
