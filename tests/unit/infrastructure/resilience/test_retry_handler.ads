--   =============================================================================
--   Test_Retry_Handler - Retry Handler Unit Tests
--   =============================================================================
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--   =============================================================================

pragma Ada_2022;

with Abohlib.Infrastructure.Testing.Test_Framework;

package Test_Retry_Handler is

   use Abohlib.Infrastructure.Testing.Test_Framework;

--   Run all retry handler tests
   function Run_All_Tests
     (Output : access Test_Output_Port'Class) return Test_Stats_Result.Result;

end Test_Retry_Handler;