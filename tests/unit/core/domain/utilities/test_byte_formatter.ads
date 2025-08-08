--   =============================================================================
--   Test_Byte_Formatter - Unit tests for byte formatting utilities
--   =============================================================================
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--
--   Purpose:
--     Unit tests for the Byte_Formatter utility using our custom test framework.
--   =============================================================================

pragma Ada_2022;

with Abohlib.Infrastructure.Testing.Test_Framework;

package Test_Byte_Formatter is

   use Abohlib.Infrastructure.Testing.Test_Framework;

   function Run_All_Tests
     (Output : access Test_Output_Port'Class) return Test_Stats_Result.Result;

end Test_Byte_Formatter;