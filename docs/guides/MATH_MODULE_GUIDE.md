# Math Module Guide

## Overview

The `Abohlib.Core.Domain.Math` module provides common mathematical calculations that are used throughout the library. This module is designed to be a central location for generic math operations that don't belong to a specific domain type.

## Key Concepts

### Why a Separate Math Module?

When building applications, you often need to perform calculations that aren't specific to any one domain. For example:
- Calculating what percentage 75 is of 100
- Converting between ratios and percentages
- Checking if two floating-point numbers are approximately equal

Rather than duplicating these calculations throughout the codebase, we centralize them in the Math module. This approach:
- Reduces code duplication
- Ensures consistent calculation methods
- Makes testing easier
- Provides a single place to look for common math operations

### Type Safety with Strong Types

The module uses strong types to prevent common errors:

```ada
type Percentage_Type is delta 0.01 range 0.0 .. 100.0;
type Ratio_Type is new Float range 0.0 .. Float'Last;
```

These types help prevent mistakes like:
- Mixing up percentages and ratios (75% vs 0.75)
- Using invalid percentage values (like 150% when that doesn't make sense)
- Losing precision in percentage calculations

## Common Usage Patterns

### Calculating Percentages

The module provides overloaded `Calculate_Percentage` functions for different numeric types:

```ada
-- Example 1: Calculate percentage of files processed
Files_Processed : Natural := 75;
Total_Files : Natural := 100;
Progress : Percentage_Type := Calculate_Percentage (Files_Processed, Total_Files);
-- Result: 75.0%

-- Example 2: Calculate percentage of bytes downloaded
Bytes_Downloaded : Long_Long_Integer := 500_000_000;  -- 500 MB
Total_Bytes : Long_Long_Integer := 1_000_000_000;    -- 1 GB
Download_Progress : Percentage_Type := Calculate_Percentage (Bytes_Downloaded, Total_Bytes);
-- Result: 50.0%

-- Example 3: Calculate percentage with floating point precision
Exact_Value : Float := 33.333;
Total_Value : Float := 100.0;
Exact_Percentage : Percentage_Type := Calculate_Percentage (Exact_Value, Total_Value);
-- Result: 33.33% (rounded to nearest 0.01)
```

### Working with Ratios

Sometimes you need to work with ratios instead of percentages:

```ada
-- Convert percentage to ratio for calculations
Discount_Percentage : Percentage_Type := 25.0;  -- 25% off
Discount_Ratio : Ratio_Type := Percentage_To_Ratio (Discount_Percentage);
-- Result: 0.25

-- Apply the discount
Original_Price : Float := 100.0;
Discounted_Price : Float := Original_Price * (1.0 - Float (Discount_Ratio));
-- Result: 75.0

-- Convert ratio back to percentage for display
Completion_Ratio : Ratio_Type := 0.85;  -- 85% complete
Completion_Percentage : Percentage_Type := Ratio_To_Percentage (Completion_Ratio);
-- Result: 85.0%
```

### Using Utility Functions

The module includes helpful utility functions:

```ada
-- Example 1: Clamp values to a valid range
function Clamp_Float is new Clamp (Float);

Temperature : Float := 120.0;  -- Too hot!
Safe_Temperature : Float := Clamp_Float (Temperature, 0.0, 100.0);
-- Result: 100.0 (clamped to maximum)

-- Example 2: Check if floating point values are approximately equal
Expected : Float := 1.0;
Calculated : Float := 0.9999;
Are_Equal : Boolean := Approximately_Equal (Expected, Calculated, 0.001);
-- Result: True (difference is less than epsilon)
```

## Integration with Other Modules

### Performance Module Integration

The Performance module uses Math types for calculations:

```ada
with Abohlib.Core.Domain.Math;
with Abohlib.Core.Domain.Types.Performance;

-- Calculate throughput
Bytes_Processed : SI_Bytes_Type := From_GB (2);  -- 2 GB
Time_Taken : Duration := 10.0;  -- 10 seconds
Throughput : MB_Per_Second_Type := Calculate_MB_Per_Second (Bytes_Processed, Time_Taken);

-- Calculate percentage of theoretical maximum
Max_Throughput : MB_Per_Second_Type := 250.0;  -- Theoretical max
Efficiency : Percentage_Type := Calculate_Percentage (
    Float (Throughput), 
    Float (Max_Throughput)
);
```

### Bytes Module Integration

When working with file sizes and byte calculations:

```ada
with Abohlib.Core.Domain.Types.Bytes;
use Abohlib.Core.Domain.Types.Bytes;

-- Calculate compression percentage
Original_Size : SI_Bytes_Type := From_MB (100);
Compressed_Size : SI_Bytes_Type := From_MB (30);
Compression_Percent : Percentage_Type := Calculate_Percentage (
    Long_Long_Integer (Original_Size - Compressed_Size),
    Long_Long_Integer (Original_Size)
);
-- Result: 70.0% (space saved)
```

## Best Practices

### 1. Choose the Right Function Overload

The module provides multiple overloads for flexibility:
- Use `Natural` for counts and small integers
- Use `Long_Long_Integer` for file sizes and large values
- Use `Float` when you need fractional precision

### 2. Handle Edge Cases

The functions handle edge cases automatically:
```ada
-- Percentage > 100% is clamped to 100%
Over_Percent : Percentage_Type := Calculate_Percentage (150, 100);
-- Result: 100.0

-- Zero denominator is caught by precondition
-- This would raise an exception:
-- Bad_Percent : Percentage_Type := Calculate_Percentage (50, 0);
```

### 3. Use Strong Types

Take advantage of the type safety:
```ada
-- This prevents mixing up ratios and percentages
Ratio : Ratio_Type := 0.75;
Percent : Percentage_Type := Ratio_To_Percentage (Ratio);

-- The types make it clear what each value represents
-- You can't accidentally use a ratio where a percentage is expected
```

### 4. Precision Considerations

Remember that `Percentage_Type` has 0.01 precision:
```ada
-- Values are rounded to nearest 0.01
One_Third : Percentage_Type := Calculate_Percentage (1, 3);
-- Result: 33.33 (not 33.333...)
```

## Common Patterns

### Progress Tracking

```ada
procedure Update_Progress (Completed : Natural; Total : Natural) is
   Progress : constant Percentage_Type := Calculate_Percentage (Completed, Total);
begin
   Put_Line ("Progress: " & Progress'Image & "%");
   
   if Progress >= 90.0 then
      Put_Line ("Almost done!");
   elsif Progress >= 50.0 then
      Put_Line ("Halfway there!");
   end if;
end Update_Progress;
```

### Performance Metrics

```ada
procedure Report_Efficiency (Actual : Float; Theoretical : Float) is
   Efficiency : constant Percentage_Type := Calculate_Percentage (Actual, Theoretical);
   Loss : constant Percentage_Type := 100.0 - Efficiency;
begin
   Put_Line ("Operating at " & Efficiency'Image & "% efficiency");
   Put_Line ("Performance loss: " & Loss'Image & "%");
end Report_Efficiency;
```

### Data Validation

```ada
function Is_Within_Tolerance (Value, Expected : Float; 
                             Tolerance_Percent : Percentage_Type) return Boolean is
   Tolerance_Ratio : constant Ratio_Type := Percentage_To_Ratio (Tolerance_Percent);
   Max_Difference : constant Float := Expected * Float (Tolerance_Ratio);
begin
   return Approximately_Equal (Value, Expected, Max_Difference);
end Is_Within_Tolerance;
```

## Error Handling

The module uses preconditions to catch errors early:

```ada
-- These preconditions prevent runtime errors:
-- Calculate_Percentage requires Whole > 0
-- Approximately_Equal requires Epsilon > 0

-- If preconditions are violated, you'll get a clear error
-- rather than incorrect results or crashes
```

When using these functions, ensure your inputs meet the preconditions or add appropriate checks:

```ada
if Total_Items > 0 then
   Progress := Calculate_Percentage (Completed_Items, Total_Items);
else
   Progress := 0.0;  -- or 100.0, depending on your logic
end if;
```

## Summary

The Math module provides essential mathematical operations in a type-safe, reusable way. By centralizing these calculations, we ensure consistency across the codebase and make it easier to find and use common mathematical operations. The strong typing helps prevent errors, while the comprehensive set of overloads provides flexibility for different use cases.