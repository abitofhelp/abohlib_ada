--   =============================================================================
--   Test_Bytes_Arithmetic - Bytes Types Arithmetic Operations Unit Tests
--   =============================================================================
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--
--   Purpose:
--     Comprehensive tests for arithmetic operations on all bytes types.
--     Validates correctness, type safety, and performance characteristics.
--   =============================================================================

pragma Ada_2022;

with Abohlib.Infrastructure.Testing.Test_Framework;

package Test_Bytes_Arithmetic is

   use Abohlib.Infrastructure.Testing.Test_Framework;

   function Run_All_Tests
     (Output : access Test_Output_Port'Class) return Test_Stats_Result.Result;

end Test_Bytes_Arithmetic;