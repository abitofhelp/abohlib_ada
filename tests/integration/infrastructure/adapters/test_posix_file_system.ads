--   =============================================================================
--   Test_POSIX_File_System - Integration tests for POSIX File System Adapter
--   =============================================================================
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--
--   Purpose:
--     Integration tests for the POSIX_File_System_Provider implementation.
--     These tests interact with the actual file system.
--   =============================================================================

pragma Ada_2022;

with Abohlib.Infrastructure.Testing.Test_Framework;

package Test_POSIX_File_System is

   use Abohlib.Infrastructure.Testing.Test_Framework;

--   Test suite entry point
   function Run_All_Tests
     (Output : access Test_Output_Port'Class) return Test_Stats_Result.Result;

private

--   Test file existence checks
   function Test_File_Exists return Void_Result.Result;

--   Test directory checks
   function Test_Directory_Operations return Void_Result.Result;

--   Test path manipulation
   function Test_Path_Operations return Void_Result.Result;

--   Test file size retrieval
   function Test_File_Size return Void_Result.Result;

--   Test path normalization
   function Test_Path_Normalization return Void_Result.Result;

--   Test error handling
   function Test_Error_Conditions return Void_Result.Result;

--   Test with temporary files
   function Test_With_Temp_Files return Void_Result.Result;

--   Test path composition
   function Test_Path_Composition return Void_Result.Result;

--   Test extension handling
   function Test_Extension_Operations return Void_Result.Result;

--   Test absolute path conversion
   function Test_Absolute_Path return Void_Result.Result;

end Test_POSIX_File_System;
