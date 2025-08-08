# Performance Calculations Guide

## Overview

The Performance module provides specialized types and functions for measuring and calculating performance metrics in your applications. This guide covers the helper functions added for common performance calculations.

## Key Functions

### Calculate_MB_Per_Second

This function calculates data transfer rates in megabytes per second. It has two overloads to support different use cases:

#### Using SI_Bytes_Type

When working with strongly-typed byte values:

```ada
with Abohlib.Core.Domain.Types.Bytes; use Abohlib.Core.Domain.Types.Bytes;
with Abohlib.Core.Domain.Types.Performance; use Abohlib.Core.Domain.Types.Performance;

-- Example: File download speed
File_Size : SI_Bytes_Type := From_MB (150);  -- 150 MB file
Download_Time : Duration := 30.0;  -- 30 seconds

Download_Speed : MB_Per_Second_Type := 
    Calculate_MB_Per_Second (File_Size, Download_Time);
-- Result: 5.0 MB/s
```

#### Using Long_Long_Integer

When working with file positions or legacy code that uses Long_Long_Integer:

```ada
-- Example: Processing throughput
Total_Bytes_Processed : Long_Long_Integer := 0;

-- Process data...
for Chunk in 1 .. 1000 loop
   Process_Chunk;
   Total_Bytes_Processed := Total_Bytes_Processed + Chunk_Size;
end loop;

End_Time : Time := Clock;
Processing_Time : Duration := End_Time - Start_Time;

Throughput : MB_Per_Second_Type := 
    Calculate_MB_Per_Second (Total_Bytes_Processed, Processing_Time);
```

### Why Two Overloads?

The Long_Long_Integer overload exists because:
1. Many file operations return sizes as Long_Long_Integer
2. File position types are often based on Long_Long_Integer
3. It avoids unnecessary conversions in performance-critical code

## Integration with Other Modules

### Working with Math Module

The Performance module now uses the Math module's `Percentage_Type` for all percentage calculations. This ensures consistency across the codebase:

```ada
with Abohlib.Core.Domain.Math; use Abohlib.Core.Domain.Math;
with Abohlib.Core.Domain.Types.Performance; use Abohlib.Core.Domain.Types.Performance;

-- The Performance module re-exports Percentage_Type from Math
-- subtype Percentage_Type is Abohlib.Core.Domain.Math.Percentage_Type;

-- For generic percentage calculations, use Math module directly
Progress_Percent : Percentage_Type := Abohlib.Core.Domain.Math.Calculate_Percentage (
    Completed_Tasks,  -- Natural
    Total_Tasks       -- Natural
);

-- For byte-specific percentages, Performance module provides a wrapper
-- that delegates to Math module internally
Data_Percent : Percentage_Type := Calculate_Percentage (
    Bytes_Processed,  -- SI_Bytes_Type
    Total_Bytes       -- SI_Bytes_Type
);

-- Both return the same Percentage_Type from Math module
```

### Why the Integration?

Previously, the Performance module had its own percentage calculations. Now:
- All percentage types come from the Math module
- This prevents duplicate code
- Ensures consistent behavior across the library
- Makes it easier to find all math-related operations

### New Float Conversion Functions

The Performance module now includes explicit conversion functions for working with Float values:

```ada
-- Speed type conversions
Speed : MB_Per_Second_Type := 125.5;
Speed_As_Float : Float := MB_Speed_To_Float (Speed);  -- 125.5
New_Speed : MB_Per_Second_Type := Float_To_MB_Speed (89.3);

-- Processing speed conversions  
Processing_Rate : Processing_Speed_Type := 1500.0;  -- items/sec
Rate_Float : Float := Processing_Speed_To_Float (Processing_Rate);
Updated_Rate : Processing_Speed_Type := Float_To_Processing_Speed (Rate_Float * 1.1);

-- Compression ratio conversions
Original_Ratio : Compression_Ratio_Type := 0.65;  -- 65% of original
Ratio_Float : Float := Compression_Ratio_To_Float (Original_Ratio);
Adjusted_Ratio : Compression_Ratio_Type := Float_To_Compression_Ratio (Ratio_Float * 0.9);
```

These functions follow the "Type Conversion Clarity Rule" from CLAUDE.md:
- Use explicit conversion functions when they provide semantic meaning
- Function names clearly indicate the conversion direction and types
- All functions include contracts for safety

### Complete Example: File Processing

Here's a complete example showing how to use these functions together:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar; use Ada.Calendar;
with Abohlib.Core.Domain.Types.Bytes; use Abohlib.Core.Domain.Types.Bytes;
with Abohlib.Core.Domain.Types.Performance; use Abohlib.Core.Domain.Types.Performance;
with Abohlib.Core.Domain.Math; use Abohlib.Core.Domain.Math;

procedure Process_Large_File is
   -- File information
   File_Size : constant SI_Bytes_Type := From_GB (2);  -- 2 GB file
   Bytes_Processed : SI_Bytes_Type := 0;
   
   -- Timing
   Start_Time : constant Time := Clock;
   Current_Time : Time;
   Elapsed : Duration;
   
   -- Chunk processing
   Chunk_Size : constant SI_Bytes_Type := From_MB (16);  -- 16 MB chunks
   
begin
   -- Process file in chunks
   while Bytes_Processed < File_Size loop
      -- Simulate processing
      delay 0.1;  -- Simulate work
      
      -- Update progress
      if Bytes_Processed + Chunk_Size <= File_Size then
         Bytes_Processed := Bytes_Processed + Chunk_Size;
      else
         Bytes_Processed := File_Size;  -- Last chunk
      end if;
      
      -- Calculate and display metrics
      Current_Time := Clock;
      Elapsed := Current_Time - Start_Time;
      
      if Elapsed > 0.0 then
         declare
            -- Calculate throughput
            Throughput : constant MB_Per_Second_Type := 
                Calculate_MB_Per_Second (Bytes_Processed, Elapsed);
            
            -- Calculate progress percentage
            Progress : constant Percentage_Type := 
                Calculate_Percentage (Bytes_Processed, File_Size);
            
            -- Estimate time remaining
            Bytes_Remaining : constant SI_Bytes_Type := 
                File_Size - Bytes_Processed;
            Time_Remaining : constant Duration := 
                Duration (Float (Bytes_Remaining) / Float (SI_MB) / Float (Throughput));
         begin
            Put_Line ("Progress: " & Progress'Image & "%");
            Put_Line ("Speed: " & Float (Throughput)'Image & " MB/s");
            Put_Line ("ETA: " & Duration'Image (Time_Remaining) & " seconds");
            New_Line;
         end;
      end if;
   end loop;
   
   Put_Line ("Processing complete!");
end Process_Large_File;
```

## Performance Tips

### 1. Choose the Right Overload

Use the overload that matches your data type to avoid conversions:

```ada
-- Good: Direct use with matching type
File_Size : Long_Long_Integer := Get_File_Size;
Speed := Calculate_MB_Per_Second (File_Size, Duration);

-- Less efficient: Unnecessary conversion
File_Size : Long_Long_Integer := Get_File_Size;
Speed := Calculate_MB_Per_Second (
    From_Long_Long_Integer (File_Size),  -- Extra conversion
    Duration
);
```

### 2. Reuse Calculated Values

Store frequently used calculations:

```ada
-- Instead of recalculating each time
for I in 1 .. 100 loop
   Show_Speed (Calculate_MB_Per_Second (Bytes, Time));
end loop;

-- Calculate once and reuse
Current_Speed : constant MB_Per_Second_Type := 
    Calculate_MB_Per_Second (Bytes, Time);
for I in 1 .. 100 loop
   Show_Speed (Current_Speed);
end loop;
```

### 3. Handle Zero Duration

The functions have preconditions to prevent division by zero:

```ada
-- This will raise an exception:
-- Speed := Calculate_MB_Per_Second (Bytes, 0.0);

-- Safe approach:
if Elapsed > 0.0 then
   Speed := Calculate_MB_Per_Second (Bytes, Elapsed);
else
   Speed := 0.0;  -- or handle as appropriate
end if;
```

## Common Patterns

### Network Transfer Monitoring

```ada
procedure Monitor_Download (URL : String; Expected_Size : SI_Bytes_Type) is
   Bytes_Received : SI_Bytes_Type := 0;
   Start : constant Time := Clock;
   
   procedure Update_Stats (New_Bytes : SI_Bytes_Type) is
      Elapsed : constant Duration := Clock - Start;
      Speed : MB_Per_Second_Type;
      Progress : Percentage_Type;
   begin
      Bytes_Received := Bytes_Received + New_Bytes;
      
      if Elapsed > 0.1 then  -- Update every 100ms minimum
         Speed := Calculate_MB_Per_Second (Bytes_Received, Elapsed);
         Progress := Calculate_Percentage (Bytes_Received, Expected_Size);
         
         Put (ASCII.CR & "Downloading: " & Progress'Image & "% at " & 
              Float (Speed)'Image & " MB/s");
      end if;
   end Update_Stats;
   
begin
   -- Download implementation would call Update_Stats
   null;
end Monitor_Download;
```

### Batch Processing Metrics

```ada
procedure Process_Batch (Items : Item_Array) is
   Start_Time : constant Time := Clock;
   Items_Processed : Natural := 0;
   Bytes_Processed : Long_Long_Integer := 0;
   
begin
   for Item of Items loop
      Process_Item (Item);
      Items_Processed := Items_Processed + 1;
      Bytes_Processed := Bytes_Processed + Long_Long_Integer (Item.Size);
      
      -- Report progress every 10 items
      if Items_Processed mod 10 = 0 then
         declare
            Elapsed : constant Duration := Clock - Start_Time;
            Item_Progress : constant Percentage_Type := 
                Calculate_Percentage (Items_Processed, Items'Length);
            Throughput : constant MB_Per_Second_Type := 
                Calculate_MB_Per_Second (Bytes_Processed, Elapsed);
         begin
            Put_Line ("Processed " & Items_Processed'Image & " items (" & 
                     Item_Progress'Image & "%) at " & 
                     Float (Throughput)'Image & " MB/s");
         end;
      end if;
   end loop;
end Process_Batch;
```

## Migration Guide

If you're updating existing code to use these helper functions:

### Before
```ada
-- Manual calculation
Throughput_MB : constant Float :=
   Float (File_Size) / Float (SI_MB) / Float (Duration_Seconds);
```

### After
```ada
-- Using helper function
Throughput_MB : constant MB_Per_Second_Type :=
   Calculate_MB_Per_Second (File_Size, Duration_Seconds);
```

### Benefits of Migration

1. **Type Safety**: Returns strongly-typed `MB_Per_Second_Type` instead of raw Float
2. **Clearer Intent**: Function name explicitly states what's being calculated
3. **Consistency**: Same calculation method used throughout the codebase
4. **Easier Testing**: Can mock or test the function independently
5. **Future-Proof**: If calculation needs to change, only one place to update

## Summary

The performance calculation helpers make it easier to work with common performance metrics in a type-safe way. By providing overloads for different numeric types, the functions integrate smoothly with existing code while encouraging the use of strong types for new development.