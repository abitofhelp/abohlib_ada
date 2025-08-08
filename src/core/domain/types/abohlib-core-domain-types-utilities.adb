--  =============================================================================
--  Abohlib.Core.Domain.Types.Utilities - Implementation
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

package body Abohlib.Core.Domain.Types.Utilities is

   --  ==========================================================================
   --  Safe Conversion Implementation
   --  ==========================================================================
   
   function Safe_Convert (Value : Source_Type) return Target_Type is
   begin
      return Target_Type (Value);
   end Safe_Convert;

end Abohlib.Core.Domain.Types.Utilities;