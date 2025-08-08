--   =============================================================================
--   Test_File_Path - Unit tests for File_Path value object
--   =============================================================================
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--
--   Purpose:
--     Unit tests for the File_Path value object using our custom test framework.
--   =============================================================================

pragma Ada_2022;

with Abohlib.Infrastructure.Testing.Test_Framework;

package Test_File_Path is

   use Abohlib.Infrastructure.Testing.Test_Framework;

--   Test suite entry point
   function Run_All_Tests
     (Output : access Test_Output_Port'Class) return Test_Stats_Result.Result;

private

--   Individual test functions
   function Test_Create_Valid_Path return Void_Result.Result;
   function Test_File_Name return Void_Result.Result;
   function Test_Extension return Void_Result.Result;
   function Test_File_Stem return Void_Result.Result;
   function Test_Is_Absolute return Void_Result.Result;

end Test_File_Path;
