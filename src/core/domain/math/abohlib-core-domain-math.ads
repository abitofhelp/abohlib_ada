--  =============================================================================
--  Abohlib.Core.Domain.Math - Common Mathematical Calculations
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Purpose:
--    Provides common mathematical calculations and types that are used across
--    multiple domains. This module contains generic math operations that don't
--    belong to a specific domain type.
--
--  Design:
--    - Generic percentage calculations for various numeric types
--    - Ratio and percentage type conversions
--    - Type-safe mathematical abstractions with contracts
--    - Utility functions for common math operations
--
--  Usage Examples:
--    -- Calculate percentage from integers
--    Percent : Percentage_Type := Calculate_Percentage (75, 100);  -- Returns 75.0
--    
--    -- Convert between ratios and percentages
--    Ratio : Ratio_Type := Percentage_To_Ratio (50.0);  -- Returns 0.5
--    Pct : Percentage_Type := Ratio_To_Percentage (0.25);  -- Returns 25.0
--    
--    -- Use the generic clamp function
--    function Clamp_Float is new Clamp (Float);
--    Clamped : Float := Clamp_Float (120.0, 0.0, 100.0);  -- Returns 100.0
--
--  Key Types:
--    - Percentage_Type: Fixed-point type (0.0 to 100.0) with 0.01 precision
--    - Ratio_Type: Float-based type (0.0 to Float'Last) where 1.0 = 100%
--  =============================================================================

pragma Ada_2022;

package Abohlib.Core.Domain.Math is
   pragma Pure;

   --  ==========================================================================
   --  Mathematical Types
   --  ==========================================================================
   
   --  Strong type for percentages (0.0 to 100.0 with 0.01 precision)
   type Percentage_Type is delta 0.01 range 0.0 .. 100.0;
   
   --  Strong type for ratios (0.0 to Float'Last, where 1.0 = 100%)
   type Ratio_Type is new Float range 0.0 .. Float'Last
      with Default_Value => 1.0;
   
   --  ==========================================================================
   --  Percentage Calculations
   --  ==========================================================================
   
   --  Calculate percentage from Natural values
   function Calculate_Percentage
     (Part  : Natural;
      Whole : Natural) return Percentage_Type
   with
     Pre => Whole > 0,
     Post => Calculate_Percentage'Result >= 0.0 and then
             Calculate_Percentage'Result <= 100.0,
     Inline;
   
   --  Calculate percentage from Long_Long_Integer values
   function Calculate_Percentage
     (Part  : Long_Long_Integer;
      Whole : Long_Long_Integer) return Percentage_Type
   with
     Pre => Whole > 0 and then Part >= 0,
     Post => Calculate_Percentage'Result >= 0.0 and then
             Calculate_Percentage'Result <= 100.0,
     Inline;
   
   --  Calculate percentage from Float values
   function Calculate_Percentage
     (Part  : Float;
      Whole : Float) return Percentage_Type
   with
     Pre => Whole > 0.0 and then Part >= 0.0,
     Post => Calculate_Percentage'Result >= 0.0 and then
             Calculate_Percentage'Result <= 100.0,
     Inline;
   
   --  ==========================================================================
   --  Ratio Calculations
   --  ==========================================================================
   
   --  Convert percentage to ratio (e.g., 75% -> 0.75)
   function Percentage_To_Ratio (Percentage : Percentage_Type) return Ratio_Type is
     (Ratio_Type (Float (Percentage) / 100.0))
   with
     Post => Percentage_To_Ratio'Result >= 0.0 and then
             Percentage_To_Ratio'Result <= 1.0,
     Inline;
   
   --  Convert ratio to percentage (e.g., 0.75 -> 75%)
   function Ratio_To_Percentage (Ratio : Ratio_Type) return Percentage_Type is
     (if Ratio <= 1.0 
      then Percentage_Type (Float (Ratio) * 100.0)
      else 100.0)
   with
     Inline;
   
   --  Calculate ratio from Natural values
   function Calculate_Ratio
     (Numerator   : Natural;
      Denominator : Natural) return Ratio_Type
   with
     Pre => Denominator > 0,
     Post => Calculate_Ratio'Result >= 0.0,
     Inline;
   
   --  ==========================================================================
   --  Utility Functions
   --  ==========================================================================
   
   --  Clamp a value between min and max
   generic
      type T is digits <>;
   function Clamp (Value, Min, Max : T) return T
   with
     Pre => Min <= Max,
     Post => Clamp'Result >= Min and then Clamp'Result <= Max,
     Inline;
   
   --  Check if two floating point values are approximately equal
   function Approximately_Equal
     (Left, Right : Float;
      Epsilon     : Float := 0.0001) return Boolean is
     (abs (Left - Right) <= Epsilon)
   with
     Pre => Epsilon > 0.0,
     Inline;

end Abohlib.Core.Domain.Math;