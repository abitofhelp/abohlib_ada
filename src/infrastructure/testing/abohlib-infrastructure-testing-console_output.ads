--  =============================================================================
--  Abohlib.Infrastructure.Testing.Console_Output - Console Test Output Adapter
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Purpose:
--    Provides a console-based implementation of the Test_Output_Port for
--    displaying test results to standard output.
--  =============================================================================

pragma Ada_2022;

with Abohlib.Infrastructure.Testing.Test_Framework;

package Abohlib.Infrastructure.Testing.Console_Output is

   use Abohlib.Infrastructure.Testing.Test_Framework;

   --  Console implementation of Test_Output_Port
   type Console_Test_Output is new Test_Output_Port with null record;

   overriding
   procedure Write_Line (Self : Console_Test_Output; Message : String);

   overriding
   procedure Write_Error (Self : Console_Test_Output; Message : String);

   --  Singleton instance for convenience
   Console_Output : aliased Console_Test_Output;

end Abohlib.Infrastructure.Testing.Console_Output;
