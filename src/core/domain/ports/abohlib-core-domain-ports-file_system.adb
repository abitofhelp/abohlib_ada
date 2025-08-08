--  =============================================================================
--  Abohlib.Core.Domain.Ports.File_System - Implementation
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

package body Abohlib.Core.Domain.Ports.File_System is

   function Make_File_System_Error
     (Kind    : File_System_Error_Kind;
      Message : String;
      Path    : String := "";
      Context : String := "") return File_System_Error is
   begin
      return
        (Kind    => Kind,
         Message => To_Unbounded_String (Message),
         Path    => To_Unbounded_String (Path),
         Context => To_Unbounded_String (Context));
   end Make_File_System_Error;

end Abohlib.Core.Domain.Ports.File_System;
