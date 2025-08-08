--   =============================================================================
--   Test_Buffer_Manager_V2 - Unit tests for Concurrent Buffer Manager (Result API)
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--   =============================================================================

pragma Ada_2022;

with Abohlib.Infrastructure.Testing.Test_Framework;

package Test_Buffer_Manager is

   use Abohlib.Infrastructure.Testing.Test_Framework;

--   Main test runner function
   function Run_All_Tests
     (Output : access Test_Output_Port'Class) return Test_Stats_Result.Result;

end Test_Buffer_Manager;