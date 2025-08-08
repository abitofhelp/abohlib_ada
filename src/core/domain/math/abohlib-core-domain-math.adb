--  =============================================================================
--  Abohlib.Core.Domain.Math - Implementation
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Implementation Notes:
--    This package provides safe mathematical operations with proper handling
--    of edge cases like division by zero and overflow conditions.
--  =============================================================================

pragma Ada_2022;

package body Abohlib.Core.Domain.Math is

   --  ==========================================================================
   --  Percentage Calculations
   --  ==========================================================================
   
   --  Calculate percentage from Natural values
   --  Algorithm:
   --    1. Convert integers to floats to avoid integer division truncation
   --    2. Calculate percentage: (part / whole) * 100
   --    3. Clamp result to [0.0, 100.0] range for safety
   --  Note: Division by zero is handled by precondition in specification
   function Calculate_Percentage
     (Part  : Natural;
      Whole : Natural) return Percentage_Type
   is
      Part_Float  : constant Float := Float (Part);
      Whole_Float : constant Float := Float (Whole);
      Result      : constant Float := (Part_Float * 100.0) / Whole_Float;
   begin
      --  Clamp to valid percentage range
      --  This handles cases where Part > Whole (returns 100.0)
      --  and potential floating-point errors
      if Result > 100.0 then
         return 100.0;
      elsif Result < 0.0 then
         return 0.0;  -- Should not occur with Natural inputs
      else
         return Percentage_Type (Result);
      end if;
   end Calculate_Percentage;
   
   --  Calculate percentage from Long_Long_Integer values
   --  This overload handles larger values (e.g., file sizes in bytes)
   --  Precision Note: Converting very large Long_Long_Integer to Float
   --  may lose precision, but percentage calculations typically don't
   --  require exact precision for large values
   function Calculate_Percentage
     (Part  : Long_Long_Integer;
      Whole : Long_Long_Integer) return Percentage_Type
   is
      Part_Float  : constant Float := Float (Part);
      Whole_Float : constant Float := Float (Whole);
      Result      : constant Float := (Part_Float * 100.0) / Whole_Float;
   begin
      --  Clamp to valid percentage range
      --  Handles negative values (when Part or Whole is negative)
      if Result > 100.0 then
         return 100.0;
      elsif Result < 0.0 then
         return 0.0;
      else
         return Percentage_Type (Result);
      end if;
   end Calculate_Percentage;
   
   --  Calculate percentage from Float values
   --  Direct float calculation without conversion overhead
   --  Useful for calculations already in floating-point domain
   function Calculate_Percentage
     (Part  : Float;
      Whole : Float) return Percentage_Type
   is
      Result : constant Float := (Part * 100.0) / Whole;
   begin
      --  Clamp to valid percentage range
      --  Handles edge cases:
      --  - Part > Whole (e.g., 120% becomes 100%)
      --  - Negative inputs (become 0%)
      --  - NaN/Infinity from float operations
      if Result > 100.0 then
         return 100.0;
      elsif Result < 0.0 then
         return 0.0;
      else
         return Percentage_Type (Result);
      end if;
   end Calculate_Percentage;
   
   --  ==========================================================================
   --  Ratio Calculations
   --  ==========================================================================
   
   --  Calculate ratio from Natural values
   --  Returns the quotient as a floating-point ratio
   --  Common uses:
   --    - Aspect ratios (width/height)
   --    - Compression ratios (original/compressed)
   --    - Performance metrics (operations/second)
   function Calculate_Ratio
     (Numerator   : Natural;
      Denominator : Natural) return Ratio_Type
   is
      Num_Float : constant Float := Float (Numerator);
      Den_Float : constant Float := Float (Denominator);
   begin
      --  Direct division - precondition ensures Denominator > 0
      return Ratio_Type (Num_Float / Den_Float);
   end Calculate_Ratio;
   
   --  ==========================================================================
   --  Generic Utility Functions
   --  ==========================================================================
   
   --  Generic clamp function
   --  Constrains a value to a specified range [Min, Max]
   --  Algorithm:
   --    1. If Value < Min, return Min (lower bound)
   --    2. If Value > Max, return Max (upper bound)  
   --    3. Otherwise, return Value (within bounds)
   --  Performance: O(1) with at most 2 comparisons
   --  Thread Safety: Pure function, safe for concurrent use
   function Clamp (Value, Min, Max : T) return T is
   begin
      if Value < Min then
         return Min;
      elsif Value > Max then
         return Max;
      else
         return Value;
      end if;
   end Clamp;

end Abohlib.Core.Domain.Math;