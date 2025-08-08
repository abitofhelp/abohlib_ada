--  =============================================================================
--  Abohlib.Infrastructure.Testing.Console_Output - Implementation
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

with Ada.Text_IO; use Ada.Text_IO;

package body Abohlib.Infrastructure.Testing.Console_Output is

   --  =========================================================================
   --  Write_Line
   --  =========================================================================

   overriding
   procedure Write_Line (Self : Console_Test_Output; Message : String) is
      pragma Unreferenced (Self);
   begin
      Put_Line (Message);
   end Write_Line;

   --  =========================================================================
   --  Write_Error
   --  =========================================================================

   overriding
   procedure Write_Error (Self : Console_Test_Output; Message : String) is
      pragma Unreferenced (Self);
   begin
      Put_Line (Standard_Error, "ERROR: " & Message);
   end Write_Error;

end Abohlib.Infrastructure.Testing.Console_Output;
