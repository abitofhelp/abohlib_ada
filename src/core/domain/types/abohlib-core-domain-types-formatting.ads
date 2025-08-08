--  =============================================================================
--  Abohlib.Core.Domain.Types.Formatting - Display and Formatting Strong Types
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Purpose:
--    Provides strong types for formatting and display operations including
--    decimal precision, numeric bases, and hash lengths.
--
--  Design:
--    - Types for controlling output precision
--    - Numeric base constraints for conversions
--    - Hash length types for cryptographic operations
--    - General numeric types (ratios, multipliers)
--  =============================================================================

pragma Ada_2022;

package Abohlib.Core.Domain.Types.Formatting is

   --  ==========================================================================
   --  Formatting Control Types
   --  ==========================================================================
   
   --  Decimal precision for formatting functions
   subtype Decimal_Precision_Type is Natural range 0 .. 10;
   
   --  Hash and encoding types
   type Hash_Length_Type is new Positive;
   type Numeric_Base_Type is new Positive range 2 .. 36;
   
   --  ==========================================================================
   --  General Numeric Types
   --  ==========================================================================
   
   type Ratio_Type is new Float range 0.0 .. 1.0;

end Abohlib.Core.Domain.Types.Formatting;