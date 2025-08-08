--   =============================================================================
--   Test_Generic_Stage - Test Specification
--   =============================================================================
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--   =============================================================================

pragma Ada_2022;

with Abohlib.Infrastructure.Testing.Test_Framework;
use Abohlib.Infrastructure.Testing.Test_Framework;

package Test_Pipeline_Generic_Stage is

--   Run all tests
   function Run_All_Tests
     (Output : access Test_Output_Port'Class) return Test_Stats_Result.Result;

end Test_Pipeline_Generic_Stage;