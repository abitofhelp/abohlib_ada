# Type Conversions Guide

**Version 1.0.0**

## Overview

Abohlib uses strong typing to prevent semantic errors at compile time. Each domain concept has its own type - you can't accidentally mix timeouts with retry delays, or decimal bytes with binary bytes. 

This guide explains:
- Why strong types make your code safer
- How to convert between types when needed
- Best practices for type conversions
- Common patterns and examples

## Why Strong Types?

Strong types prevent entire categories of bugs by making invalid states unrepresentable:

### Example: The Mars Climate Orbiter Bug
```ada
-- Without strong types (actual bug that crashed spacecraft):
function Calculate_Thrust (Force : Float) return Float is
begin
   -- Is Force in Newtons or Pound-force? No way to tell!
   return Force * 4.45;  -- Assumes pounds, but what if it's Newtons?
end;

-- With strong types (bug impossible):
type Newtons is new Float;
type Pound_Force is new Float;

function Calculate_Thrust (Force : Newtons) return Newtons is
begin
   -- Type system ensures correct units
   return Force * 4.45;
end;

-- Caller must be explicit about units:
Thrust := Calculate_Thrust (To_Newtons (Pounds));  -- Conversion required
```

### Real Example from Abohlib
```ada
-- Without strong types (confusing and error-prone):
procedure Configure_Retry (Timeout : Natural; 
                          Delay_Time : Natural;
                          Max_Attempts : Natural) is
begin
   -- Which is which? Easy to mix up!
   Configure_Retry (5000, 10, 100);  -- Timeout? Delay? Attempts?
end;

-- With strong types (self-documenting and safe):
procedure Configure_Retry (Timeout : Timeout_Ms_Type; 
                          Delay_Time : Retry_Delay_Ms_Type;
                          Max_Attempts : Retry_Count_Type) is
begin
   -- Clear and type-safe
   Configure_Retry (Timeout_Ms_Type (5000),      -- 5 second timeout
                   Retry_Delay_Ms_Type (100),    -- 100ms between retries  
                   Retry_Count_Type (10));       -- Max 10 attempts
end;
```

## Bytes Type Conversions

### Basic Conversions

The Bytes module provides conversions between SI_Bytes_Type and common numeric types:

```ada
with Abohlib.Core.Domain.Types.Bytes; use Abohlib.Core.Domain.Types.Bytes;

-- Natural to SI_Bytes_Type
Buffer_Size : Natural := 1024;
Typed_Size : SI_Bytes_Type := From_Natural (Buffer_Size);

-- SI_Bytes_Type to Natural (with safety check)
Bytes : SI_Bytes_Type := From_KB (500);  -- 500,000 bytes
Size : Natural := To_Natural (Bytes);    -- Checks it fits in Natural

-- Long_Long_Integer conversions (for large files)
File_Size : Long_Long_Integer := 5_000_000_000;  -- 5 GB
Typed_File_Size : SI_Bytes_Type := From_Long_Long_Integer (File_Size);

-- And back
Large_Bytes : SI_Bytes_Type := From_GB (10);
Size_LLI : Long_Long_Integer := To_Long_Long_Integer (Large_Bytes);
```

### Unit-Based Creation

Instead of manual multiplication, use the unit-based functions:

```ada
-- Instead of this:
Size1 : SI_Bytes_Type := SI_Bytes_Type (5) * SI_MB;

-- Use this (clearer and safer):
Size2 : SI_Bytes_Type := From_MB (5);

-- Available unit functions:
Small : SI_Bytes_Type := From_KB (256);   -- 256 KB
Medium : SI_Bytes_Type := From_MB (100);  -- 100 MB  
Large : SI_Bytes_Type := From_GB (2);     -- 2 GB
```

### SI vs IEC Bytes

Sometimes you need to convert between decimal (SI) and binary (IEC) units:

```ada
-- SI units (1 KB = 1,000 bytes)
SI_Size : SI_Bytes_Type := From_KB (1);  -- 1,000 bytes

-- Convert to IEC (1 KiB = 1,024 bytes)
IEC_Size : IEC_Bytes_Type := To_IEC_Bytes (SI_Size);  -- 1,000 bytes

-- Convert back
Back_To_SI : SI_Bytes_Type := To_SI_Bytes (IEC_Size);
```

## Time Type Conversions

### Duration to Time Types

Ada's built-in Duration type can be converted to strong time types:

```ada
with Abohlib.Core.Domain.Types.Time; use Abohlib.Core.Domain.Types.Time;

-- From Duration to Milliseconds
Elapsed : Duration := 2.5;  -- 2.5 seconds
Elapsed_Ms : Milliseconds_Type := To_Milliseconds (Elapsed);
-- Result: 2500 milliseconds

-- From Milliseconds back to Duration
Response_Time : Milliseconds_Type := 1500;  -- 1500 ms
Response_Duration : Duration := From_Milliseconds (Response_Time);
-- Result: 1.5 seconds

-- Converting specific timeout/delay types
Network_Timeout : Timeout_Ms_Type := 5000;  -- 5 second timeout
Timeout_Duration : Duration := To_Duration (Network_Timeout);
-- Result: 5.0 seconds

Retry_Delay : Delay_Ms_Type := 200;  -- 200 ms retry delay
Delay_Duration : Duration := To_Duration (Retry_Delay);
-- Result: 0.2 seconds
```

### Between Time Units

Convert between different time units:

```ada
-- Milliseconds to Seconds_Type
Response_Time : Milliseconds_Type := 1500;  -- 1500 ms
Response_Sec : Seconds_Type := To_Seconds (Response_Time);
-- Result: 1.5 seconds

-- Seconds_Type to Milliseconds
Timeout : Seconds_Type := 30.0;
Timeout_Ms : Milliseconds_Type := From_Seconds (Timeout);
-- Result: 30000 milliseconds

-- Direct conversion for calculations
Processing_Time : Seconds_Type := 2.5;
Processing_Ms : Milliseconds_Type := To_Milliseconds (Processing_Time);
-- Result: 2500 milliseconds
```

## Count Type Conversions

Count types are used for various kinds of counting:

```ada
with Abohlib.Core.Domain.Types.Counts; use Abohlib.Core.Domain.Types.Counts;

-- Creating from Natural
Items : Natural := 100;
Item_Count : Element_Count_Type := Element_Count_Type (Items);

-- Converting to Natural for display
Count : Worker_Count_Type := 8;
Put_Line ("Workers: " & Natural (Count)'Image);
```

## Math Module Integration

The Math module provides generic percentage and ratio calculations that work with multiple types:

```ada
with Abohlib.Core.Domain.Math; use Abohlib.Core.Domain.Math;

-- Calculate percentage using different numeric types
Task_Percent : Percentage_Type := Calculate_Percentage (75, 100);  -- Natural
Byte_Percent : Percentage_Type := Calculate_Percentage (
    Long_Long_Integer (Bytes_Used), 
    Long_Long_Integer (Total_Bytes)
);  -- Long_Long_Integer
Precise_Percent : Percentage_Type := Calculate_Percentage (33.7, 100.0);  -- Float

-- Convert between percentages and ratios
Success_Rate : Percentage_Type := 95.0;  -- 95%
Success_Ratio : Ratio_Type := Percentage_To_Ratio (Success_Rate);  -- 0.95

-- Use ratios for calculations
Discount_Ratio : Ratio_Type := 0.20;  -- 20% off
Discount_Percent : Percentage_Type := Ratio_To_Percentage (Discount_Ratio);  -- 20.0%
```

## Performance Type Conversions

The Performance module provides conversion functions for performance measurement types:

```ada
with Abohlib.Core.Domain.Types.Performance; use Abohlib.Core.Domain.Types.Performance;

-- Speed conversions
Speed : MB_Per_Second_Type := 125.5;
Speed_Float : Float := MB_Speed_To_Float (Speed);           -- 125.5
Back_To_Speed : MB_Per_Second_Type := Float_To_MB_Speed (Speed_Float);

-- Processing speed conversions
Proc_Speed : Processing_Speed_Type := 89.7;  -- items per second
Proc_Float : Float := Processing_Speed_To_Float (Proc_Speed);  -- 89.7
New_Proc_Speed : Processing_Speed_Type := Float_To_Processing_Speed (42.3);

-- Compression ratio conversions
Compression : Compression_Ratio_Type := 0.75;  -- 75% of original size
Comp_Float : Float := Compression_Ratio_To_Float (Compression);  -- 0.75
New_Compression : Compression_Ratio_Type := Float_To_Compression_Ratio (0.5);
```

### Type Conversion Clarity Rule

Following the updated CLAUDE.md guidelines, prefer explicit casts when the intent is clear:

```ada
-- Good: Clear direct type conversion
File_Size_Bytes : SI_Bytes_Type := SI_Bytes_Type (File_Size_Raw);

-- Good: Semantic conversion with units
Buffer_Size : SI_Bytes_Type := From_MB (256);

-- Avoid: Verbose without added clarity  
File_Size_Bytes : SI_Bytes_Type := From_Long_Long_Integer (File_Size_Raw);

-- Use conversion functions when they provide semantic meaning
Speed_Value : MB_Per_Second_Type := Float_To_MB_Speed (125.5);  -- Clear units
```

## Practical Examples

### File Processing

```ada
procedure Process_File (Path : String) is
   -- Get file size as Long_Long_Integer from system call
   Raw_Size : Long_Long_Integer := Get_File_Size (Path);
   
   -- Convert to typed value
   File_Size : SI_Bytes_Type := From_Long_Long_Integer (Raw_Size);
   
   -- Process in chunks
   Chunk_Size : constant SI_Bytes_Type := From_MB (1);  -- 1 MB chunks
   Chunks_Needed : Natural := Natural (File_Size / Chunk_Size);
   
   if File_Size mod Chunk_Size > 0 then
      Chunks_Needed := Chunks_Needed + 1;  -- Partial last chunk
   end if;
   
   Put_Line ("Processing " & To_Natural (File_Size / SI_MB)'Image & 
             " MB in " & Chunks_Needed'Image & " chunks");
end Process_File;
```

### Performance Monitoring

```ada
procedure Monitor_Performance is
   Start_Time : constant Time := Clock;
   Bytes_Processed : SI_Bytes_Type := 0;
   
   -- Do work...
   for I in 1 .. 1000 loop
      Process_Item (I);
      Bytes_Processed := Bytes_Processed + From_KB (64);
   end loop;
   
   -- Calculate metrics
   Elapsed : constant Duration := Clock - Start_Time;
   
   -- Multiple ways to calculate throughput
   -- Method 1: Using helper function with SI_Bytes_Type
   Throughput_1 : MB_Per_Second_Type := 
       Calculate_MB_Per_Second (Bytes_Processed, Elapsed);
   
   -- Method 2: Using helper function with Long_Long_Integer
   Throughput_2 : MB_Per_Second_Type := 
       Calculate_MB_Per_Second (To_Long_Long_Integer (Bytes_Processed), Elapsed);
   
   -- Both methods give the same result
   Put_Line ("Throughput: " & Float (Throughput_1)'Image & " MB/s");
end Monitor_Performance;
```

### Progress Tracking

```ada
procedure Download_With_Progress (URL : String; Expected_Size : Natural) is
   -- Convert expected size to typed value
   Total_Bytes : constant SI_Bytes_Type := From_Natural (Expected_Size);
   Downloaded : SI_Bytes_Type := 0;
   Start_Time : constant Time := Clock;
   
   procedure Update_Progress (New_Bytes : Natural) is
      -- Add new bytes using conversion
      Downloaded := Downloaded + From_Natural (New_Bytes);
      
      -- Calculate percentage using Performance module (wraps Math module)
      Progress : constant Percentage_Type := 
          Calculate_Percentage (Downloaded, Total_Bytes);
      
      -- Calculate download speed
      Elapsed : constant Duration := Clock - Start_Time;
      Speed : constant MB_Per_Second_Type := 
          Calculate_MB_Per_Second (Downloaded, Elapsed);
      
      -- Convert for display
      Downloaded_MB : constant Natural := To_Natural (Downloaded / SI_MB);
      
      Put_Line ("Downloaded " & Downloaded_MB'Image & " MB (" & 
                Float'Image (Float (Progress)) & "%) at " &
                Float'Image (Float (Speed)) & " MB/s");
   end Update_Progress;
   
begin
   -- Download implementation
   null;
end Download_With_Progress;
```

## Best Practices

### 1. Convert at Boundaries

Convert to strong types as early as possible and from strong types as late as possible:

```ada
-- Good: Convert immediately after getting external data
File_Size_Raw : Long_Long_Integer := System_Call_Get_Size;
File_Size : SI_Bytes_Type := From_Long_Long_Integer (File_Size_Raw);
-- ... use File_Size throughout your code ...

-- Bad: Keeping raw types and converting repeatedly
File_Size_Raw : Long_Long_Integer := System_Call_Get_Size;
-- ... later ...
If SI_Bytes_Type (File_Size_Raw) > From_MB (100) then  -- Converting each time
```

### 2. Use Unit Functions

Prefer unit-based creation functions over manual multiplication:

```ada
-- Good: Clear intent
Cache_Size : SI_Bytes_Type := From_MB (64);

-- Less clear: Manual calculation
Cache_Size : SI_Bytes_Type := SI_Bytes_Type (64) * SI_MB;
```

### 3. Check Constraints

Some conversions have preconditions to ensure safety:

```ada
-- To_Natural checks the value fits in Natural
Large_Value : SI_Bytes_Type := From_GB (5);
-- This would fail the precondition:
-- Natural_Value : Natural := To_Natural (Large_Value);  -- Too big!

-- Safe approach:
if Large_Value <= SI_Bytes_Type (Natural'Last) then
   Natural_Value : Natural := To_Natural (Large_Value);
else
   -- Handle the too-large case
   Put_Line ("Value too large for Natural type");
end if;
```

### 4. Document Units

When interfacing with external systems, document the expected units:

```ada
-- Good: Clear what units are expected
procedure Set_Timeout (Timeout_Ms : Natural) is  -- Milliseconds
   Timeout : Milliseconds_Type := Milliseconds_Type (Timeout_Ms);
begin
   -- ...
end Set_Timeout;

-- Less clear:
procedure Set_Timeout (Timeout : Natural) is  -- What unit?
```

## Common Pitfalls

### 1. Forgetting Conversions

The compiler will catch this, but it's a common error:

```ada
-- Won't compile: Can't mix types
Bytes : SI_Bytes_Type := From_MB (10);
Size : Natural := 5;
Total : SI_Bytes_Type := Bytes + Size;  -- Error!

-- Correct:
Total : SI_Bytes_Type := Bytes + From_Natural (Size);
```

### 2. Handling Very Large Values

The From_KB, From_MB, and From_GB functions now accept Long_Long_Integer parameters to handle very large values:

```ada
-- These all work correctly now:
Large_File : SI_Bytes_Type := From_GB (2000);    -- 2 TB file
Huge_File : SI_Bytes_Type := From_GB (5000);     -- 5 TB file
Max_File : SI_Bytes_Type := From_GB (9_223_372); -- 9.2 PB (max)

-- Maximum safe values:
-- From_KB: up to 9,223,372,036,854,775 KB (≈ 9.2 EB)
-- From_MB: up to 9,223,372,036,854 MB (≈ 9.2 PB)  
-- From_GB: up to 9,223,372,036 GB (≈ 9,223 TB or 9.2 PB)
```

### 3. Lossy Conversions

Be careful when converting to smaller types:

```ada
-- Potential data loss
Big_Bytes : SI_Bytes_Type := From_GB (10);  -- 10 billion
Small_Natural : Natural := To_Natural (Big_Bytes);  -- Will overflow!

-- Safe approach - check first:
if Big_Bytes <= SI_Bytes_Type (Natural'Last) then
   Small_Natural := To_Natural (Big_Bytes);
else
   -- Handle the overflow case
   raise Constraint_Error with "Value too large for Natural";
end if;
```

### 4. Unit Confusion

Make sure you're using the right unit functions:

```ada
-- Bug: Using KB when MB was intended  
Size : SI_Bytes_Type := From_KB (100);  -- 100 KB, not 100 MB!

-- Correct:
Size : SI_Bytes_Type := From_MB (100);  -- 100 MB
```

## Summary

Type conversions in Abohlib are designed to be:
- **Explicit**: No automatic conversions that could hide bugs
- **Safe**: Preconditions catch invalid conversions
- **Efficient**: Inline functions with no runtime overhead
- **Clear**: Function names indicate what's being converted

By using these conversion functions properly, you get the safety benefits of strong typing while maintaining clean, readable code.