--   =============================================================================
--   Test_Infrastructure_Errors_Unit - Unit tests for Infrastructure Errors
--   =============================================================================
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--
--   Purpose:
--     Comprehensive unit tests for infrastructure layer error types including
--     database errors, file system errors, network errors, and external
--     service errors. Tests constructor functions, error formatting, and
--     edge cases for string limits and error handling.
--   =============================================================================

pragma Ada_2022;

with Abohlib.Infrastructure.Testing.Test_Framework;

package Test_Infrastructure_Errors_Unit is

   use Abohlib.Infrastructure.Testing.Test_Framework;

--   Test suite entry point
   function Run_All_Tests
     (Output : access Test_Output_Port'Class) return Test_Stats_Result.Result;

private

--   Database error tests
   function Test_Database_Error_Creation return Void_Result.Result;
   function Test_Database_Error_Default_Messages return Void_Result.Result;
   function Test_Database_Error_Custom_Messages return Void_Result.Result;
   function Test_Database_Error_To_String return Void_Result.Result;
   function Test_Database_Error_All_Kinds return Void_Result.Result;

--   File system error tests
   function Test_File_System_Error_Creation return Void_Result.Result;
   function Test_File_System_Error_Default_Messages return Void_Result.Result;
   function Test_File_System_Error_Custom_Messages return Void_Result.Result;
   function Test_File_System_Error_To_String return Void_Result.Result;
   function Test_File_System_Error_All_Kinds return Void_Result.Result;

--   Network error tests
   function Test_Network_Error_Creation return Void_Result.Result;
   function Test_Network_Error_Default_Messages return Void_Result.Result;
   function Test_Network_Error_Custom_Messages return Void_Result.Result;
   function Test_Network_Error_To_String return Void_Result.Result;
   function Test_Network_Error_All_Kinds return Void_Result.Result;

--   External service error tests
   function Test_External_Service_Error_Creation return Void_Result.Result;
   function Test_External_Service_Error_Default_Messages return Void_Result.Result;
   function Test_External_Service_Error_Custom_Messages return Void_Result.Result;
   function Test_External_Service_Error_To_String return Void_Result.Result;

--   Boundary and edge case tests
   function Test_Bounded_String_Limits return Void_Result.Result;
   function Test_Error_String_Truncation return Void_Result.Result;
   function Test_Empty_String_Handling return Void_Result.Result;

end Test_Infrastructure_Errors_Unit;
