--  =============================================================================
--  Abohlib.Core.Domain.Ports.System_Info - Implementation
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

package body Abohlib.Core.Domain.Ports.System_Info is

   function Make_System_Info_Error
     (Kind : System_Info_Error_Kind; Message : String; Context : String := "")
      return System_Info_Error is
   begin
      return
        (Kind    => Kind,
         Message => To_Unbounded_String (Message),
         Context => To_Unbounded_String (Context));
   end Make_System_Info_Error;

end Abohlib.Core.Domain.Ports.System_Info;
