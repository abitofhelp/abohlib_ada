--   =============================================================================
--   Test_Safe_Call - Unit tests for Safe_Call wrapper
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--   =============================================================================

pragma Ada_2022;

with Abohlib.Infrastructure.Testing.Test_Framework;

package Test_Safe_Call is

   function Run_All_Tests
     (Output : access Abohlib.Infrastructure.Testing.Test_Framework.Test_Output_Port'Class)
      return Abohlib.Infrastructure.Testing.Test_Framework.Test_Stats_Result.Result;

end Test_Safe_Call;