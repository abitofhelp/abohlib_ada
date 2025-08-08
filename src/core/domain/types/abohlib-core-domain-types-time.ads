--  =============================================================================
--  Abohlib.Core.Domain.Types.Time - Time-Related Strong Types
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Purpose:
--    Provides strong types for time-related values to prevent mixing of
--    semantically different time concepts (timeouts, delays, durations, etc.)
--
--  Design:
--    - Milliseconds as base unit for most time types
--    - Arithmetic operations for common calculations
--    - Conversion functions between units
--    - Zero runtime overhead through expression functions
--  =============================================================================

pragma Ada_2022;

package Abohlib.Core.Domain.Types.Time is

   --  ==========================================================================
   --  Base Time Types
   --  ==========================================================================
   
   --  Strong types for time units
   type Milliseconds_Type is new Natural;
   type Seconds_Type is new Duration
     with Dynamic_Predicate => Seconds_Type >= 0.0;
   type Minutes_Type is new Duration
     with Dynamic_Predicate => Minutes_Type >= 0.0;
   type Hours_Type is new Duration
     with Dynamic_Predicate => Hours_Type >= 0.0;
   
   --  ==========================================================================
   --  Specific Time Uses
   --  ==========================================================================
   
   subtype Positive_Milliseconds is Milliseconds_Type 
     with Dynamic_Predicate => Positive_Milliseconds > 0;
   subtype Non_Negative_Milliseconds is Milliseconds_Type;
     
   type Timeout_Ms_Type is new Positive_Milliseconds;
   type Delay_Ms_Type is new Non_Negative_Milliseconds;
   
   subtype Positive_Delay is Delay_Ms_Type
     with Dynamic_Predicate => Positive_Delay > 0;
   type Retry_Delay_Ms_Type is new Positive_Delay;
   
   --  Multiplier subtype for time calculations (computational constraint)
   subtype Multiplier_Type is Float range 0.0 .. Float'Last;
   
   --  ==========================================================================
   --  Arithmetic Operations
   --  ==========================================================================
   
   --  Operations for base milliseconds type
   overriding function "+" (Left, Right : Milliseconds_Type) return Milliseconds_Type is
     (Milliseconds_Type (Natural (Left) + Natural (Right)));
   pragma Inline ("+");
   
   overriding function "-" (Left, Right : Milliseconds_Type) return Milliseconds_Type is
     (Milliseconds_Type (Natural (Left) - Natural (Right)));
   pragma Inline ("-");
   
   function "*" (Left : Milliseconds_Type; Right : Natural) return Milliseconds_Type is
     (Milliseconds_Type (Natural (Left) * Right));
   pragma Inline ("*");
   
   function "/" (Left : Milliseconds_Type; Right : Positive) return Milliseconds_Type is
     (Milliseconds_Type (Natural (Left) / Right));
   pragma Inline ("/");
   
   --  Operations for timeout type
   function "+" (Left : Timeout_Ms_Type; Right : Delay_Ms_Type) return Timeout_Ms_Type is
     (Timeout_Ms_Type (Natural (Left) + Natural (Right)));
   pragma Inline ("+");
   
   --  Operations for retry delay type
   overriding function "+" (Left : Retry_Delay_Ms_Type; Right : Retry_Delay_Ms_Type) return Retry_Delay_Ms_Type is
     (Retry_Delay_Ms_Type (Natural (Left) + Natural (Right)));
   pragma Inline ("+");
   
   function "*" (Left : Retry_Delay_Ms_Type; Right : Multiplier_Type) return Retry_Delay_Ms_Type is
     (Retry_Delay_Ms_Type (Float (Left) * Right));
   pragma Inline ("*");
   
   --  ==========================================================================
   --  Conversion Functions
   --  ==========================================================================
   
   function To_Milliseconds (S : Seconds_Type) return Milliseconds_Type
   is (Milliseconds_Type (S * 1_000.0));
   pragma Inline (To_Milliseconds);
   
   function To_Seconds (Ms : Milliseconds_Type) return Seconds_Type
   is (Seconds_Type (Ms) / 1_000.0);
   pragma Inline (To_Seconds);
   
   function To_Duration (Ms : Milliseconds_Type) return Duration
   is (Duration (Ms) / 1_000.0);
   pragma Inline (To_Duration);
   
   --  ==========================================================================
   --  Additional Arithmetic Operations for Time Types
   --  ==========================================================================
   
   --  Additional operations for Timeout_Ms_Type
   overriding function "+" (Left, Right : Timeout_Ms_Type) return Timeout_Ms_Type is
     (Timeout_Ms_Type (Natural (Left) + Natural (Right)));
   pragma Inline ("+");
   
   function "-" (Left : Timeout_Ms_Type; Right : Delay_Ms_Type) return Timeout_Ms_Type is
     (Timeout_Ms_Type (Natural (Left) - Natural (Right)));
   pragma Inline ("-");
   
   function "*" (Left : Timeout_Ms_Type; Right : Natural) return Timeout_Ms_Type is
     (Timeout_Ms_Type (Natural (Left) * Right));
   pragma Inline ("*");
   
   function "/" (Left : Timeout_Ms_Type; Right : Positive) return Timeout_Ms_Type is
     (Timeout_Ms_Type (Natural (Left) / Right));
   pragma Inline ("/");
   
   overriding function "<" (Left, Right : Timeout_Ms_Type) return Boolean is
     (Natural (Left) < Natural (Right));
   pragma Inline ("<");
   
   overriding function ">" (Left, Right : Timeout_Ms_Type) return Boolean is
     (Natural (Left) > Natural (Right));
   pragma Inline (">");
   
   --  Additional operations for Delay_Ms_Type
   overriding function "+" (Left, Right : Delay_Ms_Type) return Delay_Ms_Type is
     (Delay_Ms_Type (Natural (Left) + Natural (Right)));
   pragma Inline ("+");
   
   overriding function "-" (Left, Right : Delay_Ms_Type) return Delay_Ms_Type is
     (Delay_Ms_Type (Natural (Left) - Natural (Right)));
   pragma Inline ("-");
   
   function "*" (Left : Delay_Ms_Type; Right : Natural) return Delay_Ms_Type is
     (Delay_Ms_Type (Natural (Left) * Right));
   pragma Inline ("*");
   
   function "/" (Left : Delay_Ms_Type; Right : Positive) return Delay_Ms_Type is
     (Delay_Ms_Type (Natural (Left) / Right));
   pragma Inline ("/");
   
   overriding function "<" (Left, Right : Delay_Ms_Type) return Boolean is
     (Natural (Left) < Natural (Right));
   pragma Inline ("<");
   
   overriding function ">" (Left, Right : Delay_Ms_Type) return Boolean is
     (Natural (Left) > Natural (Right));
   pragma Inline (">");
   
   --  Arithmetic for Seconds_Type, Minutes_Type, Hours_Type
   overriding function "+" (Left, Right : Seconds_Type) return Seconds_Type is
     (Seconds_Type (Duration (Left) + Duration (Right)));
   pragma Inline ("+");
   
   overriding function "-" (Left, Right : Seconds_Type) return Seconds_Type is
     (Seconds_Type (Duration (Left) - Duration (Right)));
   pragma Inline ("-");
   
   overriding function "*" (Left : Seconds_Type; Right : Natural) return Seconds_Type is
     (Seconds_Type (Duration (Left) * Duration (Right)));
   pragma Inline ("*");
   
   overriding function "+" (Left, Right : Minutes_Type) return Minutes_Type is
     (Minutes_Type (Duration (Left) + Duration (Right)));
   pragma Inline ("+");
   
   overriding function "+" (Left, Right : Hours_Type) return Hours_Type is
     (Hours_Type (Duration (Left) + Duration (Right)));
   pragma Inline ("+");
   
   --  ==========================================================================
   --  Position and Location Types (time-related)
   --  ==========================================================================
   
   --  Strong types for different position concepts
   type File_Position_Type is new Long_Long_Integer range 0 .. Long_Long_Integer'Last;
   type Timestamp_Type is new Long_Long_Integer;  -- Unix timestamp or ticks
   
   --  Arithmetic Operations for File_Position_Type
   function "+" (Left : File_Position_Type; Right : Natural) return File_Position_Type is
     (File_Position_Type (Long_Long_Integer (Left) + Long_Long_Integer (Right)));
   pragma Inline ("+");
   
   function "-" (Left : File_Position_Type; Right : Natural) return File_Position_Type is
     (File_Position_Type (Long_Long_Integer (Left) - Long_Long_Integer (Right)));
   pragma Inline ("-");
   
   overriding function "+" (Left, Right : File_Position_Type) return File_Position_Type is
     (File_Position_Type (Long_Long_Integer (Left) + Long_Long_Integer (Right)));
   pragma Inline ("+");
   
   overriding function "-" (Left, Right : File_Position_Type) return File_Position_Type is
     (File_Position_Type (Long_Long_Integer (Left) - Long_Long_Integer (Right)));
   pragma Inline ("-");
   
   overriding function "<" (Left, Right : File_Position_Type) return Boolean is
     (Long_Long_Integer (Left) < Long_Long_Integer (Right));
   pragma Inline ("<");
   
   overriding function ">" (Left, Right : File_Position_Type) return Boolean is
     (Long_Long_Integer (Left) > Long_Long_Integer (Right));
   pragma Inline (">");
   
   overriding function "<=" (Left, Right : File_Position_Type) return Boolean is
     (Long_Long_Integer (Left) <= Long_Long_Integer (Right));
   pragma Inline ("<=");
   
   overriding function ">=" (Left, Right : File_Position_Type) return Boolean is
     (Long_Long_Integer (Left) >= Long_Long_Integer (Right));
   pragma Inline (">=");
   
   --  Arithmetic Operations for Timestamp_Type
   function "+" (Left : Timestamp_Type; Right : Milliseconds_Type) return Timestamp_Type is
     (Timestamp_Type (Long_Long_Integer (Left) + Long_Long_Integer (Right)));
   pragma Inline ("+");
   
   function "-" (Left : Timestamp_Type; Right : Milliseconds_Type) return Timestamp_Type is
     (Timestamp_Type (Long_Long_Integer (Left) - Long_Long_Integer (Right)));
   pragma Inline ("-");
   
   overriding function "<" (Left, Right : Timestamp_Type) return Boolean is
     (Long_Long_Integer (Left) < Long_Long_Integer (Right));
   pragma Inline ("<");
   
   overriding function ">" (Left, Right : Timestamp_Type) return Boolean is
     (Long_Long_Integer (Left) > Long_Long_Integer (Right));
   pragma Inline (">");

   --  ==========================================================================
   --  Type Conversions (Requested by pipelib)
   --  ==========================================================================
   
   --  Duration ↔ Milliseconds_Type conversions
   function To_Milliseconds (D : Duration) return Milliseconds_Type is
     (Milliseconds_Type (D * 1_000.0))
   with
     Pre => D >= 0.0 and then D <= Duration (Natural'Last / 1_000),
     Post => To_Milliseconds'Result = Milliseconds_Type (D * 1_000.0);
   pragma Inline (To_Milliseconds);
   
   function From_Milliseconds (Ms : Milliseconds_Type) return Duration is
     (Duration (Float (Ms) / 1_000.0))
   with
     Post => From_Milliseconds'Result = Duration (Float (Ms) / 1_000.0);
   pragma Inline (From_Milliseconds);
   
   --  Additional conversion functions for common operations
   
   --  Convert Seconds_Type to Milliseconds_Type
   function From_Seconds (S : Seconds_Type) return Milliseconds_Type is
     (Milliseconds_Type (Duration (S) * 1_000.0))
   with
     Pre => Duration (S) <= Duration (Natural'Last / 1_000),
     Post => From_Seconds'Result = Milliseconds_Type (Duration (S) * 1_000.0);
   pragma Inline (From_Seconds);
   
   --  Convert Timeout_Ms_Type to Duration
   function To_Duration (Timeout : Timeout_Ms_Type) return Duration is
     (Duration (Float (Timeout) / 1_000.0))
   with
     Post => To_Duration'Result = Duration (Float (Timeout) / 1_000.0);
   pragma Inline (To_Duration);
   
   --  Convert Delay_Ms_Type to Duration
   function To_Duration (Delay_Ms : Delay_Ms_Type) return Duration is
     (Duration (Float (Delay_Ms) / 1_000.0))
   with
     Post => To_Duration'Result = Duration (Float (Delay_Ms) / 1_000.0);
   pragma Inline (To_Duration);

end Abohlib.Core.Domain.Types.Time;