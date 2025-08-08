--  =============================================================================
--  Abohlib.Core.Domain.Types.Performance - Performance Measurement Strong Types
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Purpose:
--    Provides strong types for performance measurements including throughput,
--    data transfer rates, processing speeds, and compression metrics.
--
--  Design:
--    - Base types for bytes per second measurements
--    - Derived types for MB/s, GB/s rates  
--    - Compression ratio calculations
--    - Type safety for performance calculations
--    - Integration with Math module for percentage calculations
--
--  Usage Examples:
--    -- Calculate transfer speed
--    Speed : MB_Per_Second_Type := Calculate_MB_Per_Second (10_000_000, 2.5);
--    -- Result: 4.0 MB/s (10MB in 2.5 seconds)
--    
--    -- Convert speed to Float for display
--    Display_Speed : Float := MB_Speed_To_Float (Speed);
--    -- Result: 4.0
--    
--    -- Create speed from Float value
--    New_Speed : MB_Per_Second_Type := Float_To_MB_Speed (125.5);
--    -- Result: 125.5 MB/s
--    
--    -- Calculate compression ratio
--    Ratio : Compression_Ratio_Type := 
--      Calculate_Compression_Ratio (Original_Size => 1000, Compressed_Size => 250);
--    -- Result: 0.25 (75% compression)
--    
--    -- Convert compression ratio for calculations
--    Ratio_Float : Float := Compression_Ratio_To_Float (Ratio);
--    New_Ratio : Compression_Ratio_Type := Float_To_Compression_Ratio (0.5);
--    
--    -- Convert to percentage saved
--    Saved : Percentage_Type := Compression_Percentage_Saved (Ratio);
--    -- Result: 75.0%
--    
--    -- Calculate throughput
--    Throughput : Throughput_Type := 
--      Calculate_Throughput (Bytes => 5_000_000, Time_Sec => 10.0);
--    -- Result: 500,000 bytes/second
--
--  Dependencies:
--    - Abohlib.Core.Domain.Types.Bytes for byte types
--    - Abohlib.Core.Domain.Types.Time for time types
--    - Abohlib.Core.Domain.Math for Percentage_Type
--  =============================================================================

pragma Ada_2022;

with Abohlib.Core.Domain.Types.Bytes;
with Abohlib.Core.Domain.Types.Time;
with Abohlib.Core.Domain.Math;

package Abohlib.Core.Domain.Types.Performance is

   --  Make necessary types visible
   use type Abohlib.Core.Domain.Types.Bytes.SI_Bytes_Type;
   use type Abohlib.Core.Domain.Types.Time.Seconds_Type;
   use type Abohlib.Core.Domain.Math.Percentage_Type;

   --  ==========================================================================
   --  Speed and Throughput Types
   --  ==========================================================================
   
   --  Strong types for throughput and data transfer rates
   type Bytes_Per_Second_Type is new Float range 0.0 .. Float'Last;
   type MB_Per_Second_Type is new Float range 0.0 .. Float'Last;
   type GB_Per_Second_Type is new Float range 0.0 .. Float'Last;
   
   --  General performance measurement types
   type Processing_Speed_Type is new Float range 0.0 .. Float'Last;
   type Transfer_Rate_Type is new Bytes_Per_Second_Type;
   type Throughput_Type is new Bytes_Per_Second_Type;
   
   --  ==========================================================================
   --  Conversion Functions
   --  ==========================================================================
   
   function To_MB_Per_Second (Bytes_Per_Sec : Bytes_Per_Second_Type) return MB_Per_Second_Type
   is (MB_Per_Second_Type (Bytes_Per_Sec / 1_000_000.0));
   pragma Inline (To_MB_Per_Second);
   
   function To_GB_Per_Second (MB_Per_Sec : MB_Per_Second_Type) return GB_Per_Second_Type
   is (GB_Per_Second_Type (MB_Per_Sec / 1_000.0));
   pragma Inline (To_GB_Per_Second);
   
   function Calculate_Throughput 
     (Bytes : Abohlib.Core.Domain.Types.Bytes.SI_Bytes_Type; 
      Time_Sec : Abohlib.Core.Domain.Types.Time.Seconds_Type) return Throughput_Type
   is (if Time_Sec > 0.0 
       then Throughput_Type (Float (Bytes) / Float (Time_Sec))
       else 0.0);
   pragma Inline (Calculate_Throughput);

   --  ==========================================================================
   --  Float Conversions for Performance Types
   --  ==========================================================================

   --  MB_Per_Second_Type conversions
   function MB_Speed_To_Float (Speed : MB_Per_Second_Type) return Float is
     (Float (Speed))
   with
     Post => MB_Speed_To_Float'Result = Float (Speed);
   pragma Inline (MB_Speed_To_Float);

   function Float_To_MB_Speed (Value : Float) return MB_Per_Second_Type is
     (MB_Per_Second_Type (Value))
   with
     Pre => Value >= 0.0,
     Post => Float_To_MB_Speed'Result = MB_Per_Second_Type (Value);
   pragma Inline (Float_To_MB_Speed);

   --  Processing_Speed_Type conversions  
   function Processing_Speed_To_Float (Speed : Processing_Speed_Type) return Float is
     (Float (Speed))
   with
     Post => Processing_Speed_To_Float'Result = Float (Speed);
   pragma Inline (Processing_Speed_To_Float);

   function Float_To_Processing_Speed (Value : Float) return Processing_Speed_Type is
     (Processing_Speed_Type (Value))
   with
     Pre => Value >= 0.0,
     Post => Float_To_Processing_Speed'Result = Processing_Speed_Type (Value);
   pragma Inline (Float_To_Processing_Speed);

   --  ==========================================================================
   --  Compression Types
   --  ==========================================================================
   
   --  Compression ratio (0.0 = fully compressed, 1.0 = no compression)
   --  Values > 1.0 indicate expansion (e.g., from encryption overhead)
   type Compression_Ratio_Type is new Float range 0.0 .. Float'Last
      with Default_Value => 1.0;
   
   --  Calculate compression ratio from original and compressed sizes
   function Calculate_Compression_Ratio
     (Original_Size   : Abohlib.Core.Domain.Types.Bytes.SI_Bytes_Type;
      Compressed_Size : Abohlib.Core.Domain.Types.Bytes.SI_Bytes_Type) return Compression_Ratio_Type
   is (if Original_Size > 0
       then Compression_Ratio_Type (Float (Compressed_Size) / Float (Original_Size))
       else Compression_Ratio_Type (1.0))
   with Inline,
        Pre => Original_Size >= 0 and then Compressed_Size >= 0;

   --  Compression_Ratio_Type conversions
   function Compression_Ratio_To_Float (Ratio : Compression_Ratio_Type) return Float is
     (Float (Ratio))
   with
     Post => Compression_Ratio_To_Float'Result = Float (Ratio);
   pragma Inline (Compression_Ratio_To_Float);

   function Float_To_Compression_Ratio (Value : Float) return Compression_Ratio_Type is
     (Compression_Ratio_Type (Value))
   with
     Pre => Value >= 0.0,
     Post => Float_To_Compression_Ratio'Result = Compression_Ratio_Type (Value);
   pragma Inline (Float_To_Compression_Ratio);
   
   --  ==========================================================================
   --  Re-export Percentage Type from Math module
   --  ==========================================================================
   
   --  Re-export for convenience and backward compatibility
   subtype Percentage_Type is Abohlib.Core.Domain.Math.Percentage_Type;
   
   --  Convert compression ratio to percentage space saved
   function Compression_Percentage_Saved
     (Ratio : Compression_Ratio_Type) return Percentage_Type
   is (if Ratio <= 1.0
       then Percentage_Type ((1.0 - Float (Ratio)) * 100.0)
       else 0.0)  -- No savings if expanded
   with Inline;
   
   --  ==========================================================================
   --  Arithmetic Operations for Performance Types
   --  ==========================================================================
   
   --  Operations for Bytes_Per_Second_Type
   overriding function "+" (Left, Right : Bytes_Per_Second_Type) return Bytes_Per_Second_Type is
     (Bytes_Per_Second_Type (Float (Left) + Float (Right)));
   pragma Inline ("+");
   
   overriding function "-" (Left, Right : Bytes_Per_Second_Type) return Bytes_Per_Second_Type is
     (Bytes_Per_Second_Type (Float (Left) - Float (Right)));
   pragma Inline ("-");
   
   function "*" (Left : Bytes_Per_Second_Type; Right : Float) return Bytes_Per_Second_Type is
     (Bytes_Per_Second_Type (Float (Left) * Right));
   pragma Inline ("*");
   
   function "/" (Left : Bytes_Per_Second_Type; Right : Float) return Bytes_Per_Second_Type is
     (Bytes_Per_Second_Type (Float (Left) / Right));
   pragma Inline ("/");
   
   overriding function "<" (Left, Right : Bytes_Per_Second_Type) return Boolean is
     (Float (Left) < Float (Right));
   pragma Inline ("<");
   
   overriding function ">" (Left, Right : Bytes_Per_Second_Type) return Boolean is
     (Float (Left) > Float (Right));
   pragma Inline (">");
   
   --  Operations for MB_Per_Second_Type
   overriding function "+" (Left, Right : MB_Per_Second_Type) return MB_Per_Second_Type is
     (MB_Per_Second_Type (Float (Left) + Float (Right)));
   pragma Inline ("+");
   
   overriding function "-" (Left, Right : MB_Per_Second_Type) return MB_Per_Second_Type is
     (MB_Per_Second_Type (Float (Left) - Float (Right)));
   pragma Inline ("-");
   
   function "*" (Left : MB_Per_Second_Type; Right : Float) return MB_Per_Second_Type is
     (MB_Per_Second_Type (Float (Left) * Right));
   pragma Inline ("*");
   
   function "/" (Left : MB_Per_Second_Type; Right : Float) return MB_Per_Second_Type is
     (MB_Per_Second_Type (Float (Left) / Right));
   pragma Inline ("/");
   
   overriding function "<" (Left, Right : MB_Per_Second_Type) return Boolean is
     (Float (Left) < Float (Right));
   pragma Inline ("<");
   
   overriding function ">" (Left, Right : MB_Per_Second_Type) return Boolean is
     (Float (Left) > Float (Right));
   pragma Inline (">");
   
   --  Operations for GB_Per_Second_Type
   overriding function "+" (Left, Right : GB_Per_Second_Type) return GB_Per_Second_Type is
     (GB_Per_Second_Type (Float (Left) + Float (Right)));
   pragma Inline ("+");
   
   overriding function "-" (Left, Right : GB_Per_Second_Type) return GB_Per_Second_Type is
     (GB_Per_Second_Type (Float (Left) - Float (Right)));
   pragma Inline ("-");
   
   function "*" (Left : GB_Per_Second_Type; Right : Float) return GB_Per_Second_Type is
     (GB_Per_Second_Type (Float (Left) * Right));
   pragma Inline ("*");
   
   function "/" (Left : GB_Per_Second_Type; Right : Float) return GB_Per_Second_Type is
     (GB_Per_Second_Type (Float (Left) / Right));
   pragma Inline ("/");
   
   overriding function "<" (Left, Right : GB_Per_Second_Type) return Boolean is
     (Float (Left) < Float (Right));
   pragma Inline ("<");
   
   overriding function ">" (Left, Right : GB_Per_Second_Type) return Boolean is
     (Float (Left) > Float (Right));
   pragma Inline (">");
   
   --  Operations for Throughput_Type
   overriding function "+" (Left, Right : Throughput_Type) return Throughput_Type is
     (Throughput_Type (Float (Left) + Float (Right)));
   pragma Inline ("+");
   
   overriding function "-" (Left, Right : Throughput_Type) return Throughput_Type is
     (Throughput_Type (Float (Left) - Float (Right)));
   pragma Inline ("-");
   
   function "*" (Left : Throughput_Type; Right : Float) return Throughput_Type is
     (Throughput_Type (Float (Left) * Right));
   pragma Inline ("*");
   
   function "/" (Left : Throughput_Type; Right : Float) return Throughput_Type is
     (Throughput_Type (Float (Left) / Right));
   pragma Inline ("/");
   
   overriding function "<" (Left, Right : Throughput_Type) return Boolean is
     (Float (Left) < Float (Right));
   pragma Inline ("<");
   
   overriding function ">" (Left, Right : Throughput_Type) return Boolean is
     (Float (Left) > Float (Right));
   pragma Inline (">");
   
   --  Percentage_Type inherits all necessary operators from its decimal type
   --  No custom operators needed

   --  ==========================================================================
   --  Calculation Helpers (Requested by pipelib)
   --  ==========================================================================
   
   --  Calculate MB/s from bytes and duration
   function Calculate_MB_Per_Second
     (Bytes    : Abohlib.Core.Domain.Types.Bytes.SI_Bytes_Type;
      Duration : Standard.Duration) return MB_Per_Second_Type
   with
     Pre => Duration > 0.0,
     Post => Calculate_MB_Per_Second'Result >= 0.0;
   
   --  Calculate MB/s from Long_Long_Integer bytes and duration (overload for pipelib)
   function Calculate_MB_Per_Second
     (Bytes    : Long_Long_Integer;
      Duration : Standard.Duration) return MB_Per_Second_Type
   with
     Pre => Bytes >= 0 and then Duration > 0.0,
     Post => Calculate_MB_Per_Second'Result >= 0.0;

   --  Calculate percentage from bytes (domain-specific wrapper)
   function Calculate_Percentage
     (Part  : Abohlib.Core.Domain.Types.Bytes.SI_Bytes_Type;
      Whole : Abohlib.Core.Domain.Types.Bytes.SI_Bytes_Type) return Percentage_Type
   with
     Pre => Whole > 0,
     Post => Calculate_Percentage'Result >= 0.0 and then
             Calculate_Percentage'Result <= 100.0;
   
   --  Note: Generic percentage calculations (Natural, Long_Long_Integer, Float)
   --  are available in Abohlib.Core.Domain.Math

   --  Note: Calculate_Compression_Ratio already exists above (line 73-81)
   --  It calculates compressed/original ratio (0.0 to 1.0 range)

end Abohlib.Core.Domain.Types.Performance;