--   =============================================================================
--   Test_POSIX_System_Info - Integration tests for POSIX System Info Adapter
--   =============================================================================
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--
--   Purpose:
--     Integration tests for the POSIX_System_Info_Provider implementation.
--     These tests interact with the actual operating system.
--   =============================================================================

pragma Ada_2022;

with Abohlib.Infrastructure.Testing.Test_Framework;

package Test_POSIX_System_Info is

   use Abohlib.Infrastructure.Testing.Test_Framework;

--   Test suite entry point
   function Run_All_Tests
     (Output : access Test_Output_Port'Class) return Test_Stats_Result.Result;

private

--   Test memory information retrieval
   function Test_Memory_Info return Void_Result.Result;

--   Test CPU information retrieval
   function Test_CPU_Info return Void_Result.Result;

--   Test OS identification
   function Test_OS_Info return Void_Result.Result;

--   Test architecture detection
   function Test_Architecture_Info return Void_Result.Result;

--   Test consistency of values
   function Test_Value_Consistency return Void_Result.Result;

--   Test reasonable value ranges
   function Test_Value_Ranges return Void_Result.Result;

--   Test multiple calls return same values
   function Test_Call_Stability return Void_Result.Result;

--   Test performance of calls
   function Test_Performance return Void_Result.Result;

end Test_POSIX_System_Info;
