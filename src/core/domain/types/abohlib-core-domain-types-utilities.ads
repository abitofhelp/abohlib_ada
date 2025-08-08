--  =============================================================================
--  Abohlib.Core.Domain.Types.Utilities - Type Conversion Utilities
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Purpose:
--    Provides generic utility functions for safe type conversions between
--    different numeric types with compile-time range checking.
--
--  Design:
--    - Generic safe conversion with preconditions
--    - Compile-time range validation where possible
--    - Runtime safety through contracts
--  =============================================================================

pragma Ada_2022;

package Abohlib.Core.Domain.Types.Utilities is

   --  ==========================================================================
   --  Safe Conversion Function
   --  ==========================================================================
   
   --  Safe conversion with range checking
   generic
      type Source_Type is range <>;
      type Target_Type is range <>;
   function Safe_Convert (Value : Source_Type) return Target_Type
   with Pre => Value >= Source_Type (Target_Type'First) and
               Value <= Source_Type (Target_Type'Last);

end Abohlib.Core.Domain.Types.Utilities;