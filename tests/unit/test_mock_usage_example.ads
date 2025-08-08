--   =============================================================================
--   Test_Mock_Usage_Example - Example of Using Mock Objects
--   =============================================================================
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--
--   Purpose:
--     Demonstrates how to use the mock implementations for testing domain
--     services that depend on ports.
--   =============================================================================

pragma Ada_2022;

with Abohlib.Infrastructure.Testing.Test_Framework;

package Test_Mock_Usage_Example is

   use Abohlib.Infrastructure.Testing.Test_Framework;

--   Test suite entry point
   function Run_All_Tests
     (Output : access Test_Output_Port'Class) return Test_Stats_Result.Result;

private

--   Test using file system mock
   function Test_File_System_Mock_Usage return Void_Result.Result;

--   Test using system info mock
   function Test_System_Info_Mock_Usage return Void_Result.Result;

--   Test mock failure simulation
   function Test_Mock_Failure_Simulation return Void_Result.Result;

--   Test mock call tracking
   function Test_Mock_Call_Tracking return Void_Result.Result;

end Test_Mock_Usage_Example;
