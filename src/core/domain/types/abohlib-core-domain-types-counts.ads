--  =============================================================================
--  Abohlib.Core.Domain.Types.Counts - Count, Index, and Size Strong Types
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Purpose:
--    Provides strong types to distinguish between counting items, indexing
--    arrays, and measuring sizes. Prevents common errors like using counts
--    as indices or mixing different counting contexts.
--
--  Design:
--    - Separate types for counts vs indices vs sizes
--    - Domain-specific count types for different contexts
--    - Type safety prevents off-by-one errors
--    - Clear semantic meaning in APIs
--  =============================================================================

pragma Ada_2022;

package Abohlib.Core.Domain.Types.Counts is

   --  ==========================================================================
   --  Base Count, Index, and Size Types
   --  ==========================================================================
   
   --  Distinguish between counting items, indexing arrays, and measuring sizes
   type Element_Count_Type is new Natural;
   type Array_Index_Type is new Positive;
   type Array_Size_Type is new Positive;
   
   --  ==========================================================================
   --  Domain-Specific Count Types
   --  ==========================================================================
   
   --  Retry and resilience counts
   type Retry_Count_Type is new Positive
     with Static_Predicate => Retry_Count_Type in 1 .. 100;
   type Failure_Count_Type is new Natural;
   type Success_Count_Type is new Natural;
   
   --  Session and connection counts
   type Session_Count_Type is new Natural
     with Static_Predicate => Session_Count_Type <= 10_000;
   
   --  System resource counts
   type CPU_Count_Type is new Natural
     with Static_Predicate => CPU_Count_Type in 1 .. 1024;
   type Worker_Count_Type is new Positive
     with Static_Predicate => Worker_Count_Type in 1 .. 1024;
   
   --  Sequence numbers and identifiers
   type Sequence_Number_Type is new Natural;
   
   --  ==========================================================================
   --  Arithmetic Operations for Element_Count_Type
   --  ==========================================================================
   
   overriding function "+" (Left, Right : Element_Count_Type) return Element_Count_Type is
     (Element_Count_Type (Natural (Left) + Natural (Right)))
   with
     Pre => Natural (Left) <= Natural'Last - Natural (Right),
     Post => Natural ("+"'Result) = Natural (Left) + Natural (Right);
   pragma Inline ("+");
   
   overriding function "-" (Left, Right : Element_Count_Type) return Element_Count_Type is
     (Element_Count_Type (Natural (Left) - Natural (Right)))
   with
     Pre => Natural (Left) >= Natural (Right),
     Post => Natural ("-"'Result) = Natural (Left) - Natural (Right);
   pragma Inline ("-");
   
   function "+" (Left : Element_Count_Type; Right : Natural) return Element_Count_Type is
     (Element_Count_Type (Natural (Left) + Right))
   with
     Pre => Natural (Left) <= Natural'Last - Right,
     Post => Natural ("+"'Result) = Natural (Left) + Right;
   pragma Inline ("+");
   
   function "-" (Left : Element_Count_Type; Right : Natural) return Element_Count_Type is
     (Element_Count_Type (Natural (Left) - Right))
   with
     Pre => Natural (Left) >= Right,
     Post => Natural ("-"'Result) = Natural (Left) - Right;
   pragma Inline ("-");
   
   function "*" (Left : Element_Count_Type; Right : Natural) return Element_Count_Type is
     (Element_Count_Type (Natural (Left) * Right))
   with
     Pre => Right = 0 or else Natural (Left) <= Natural'Last / Right,
     Post => Natural ("*"'Result) = Natural (Left) * Right;
   pragma Inline ("*");
   
   function "*" (Left : Natural; Right : Element_Count_Type) return Element_Count_Type is
     (Element_Count_Type (Left * Natural (Right)));
   pragma Inline ("*");
   
   function "/" (Left : Element_Count_Type; Right : Positive) return Element_Count_Type is
     (Element_Count_Type (Natural (Left) / Right));
   pragma Inline ("/");
   
   overriding function "<" (Left, Right : Element_Count_Type) return Boolean is
     (Natural (Left) < Natural (Right));
   pragma Inline ("<");
   
   overriding function ">" (Left, Right : Element_Count_Type) return Boolean is
     (Natural (Left) > Natural (Right));
   pragma Inline (">");
   
   overriding function "<=" (Left, Right : Element_Count_Type) return Boolean is
     (Natural (Left) <= Natural (Right));
   pragma Inline ("<=");
   
   overriding function ">=" (Left, Right : Element_Count_Type) return Boolean is
     (Natural (Left) >= Natural (Right));
   pragma Inline (">=");
   
   --  ==========================================================================
   --  Arithmetic Operations for Array_Index_Type
   --  ==========================================================================
   
   function "+" (Left : Array_Index_Type; Right : Natural) return Array_Index_Type is
     (Array_Index_Type (Positive (Left) + Right));
   pragma Inline ("+");
   
   function "-" (Left : Array_Index_Type; Right : Natural) return Array_Index_Type is
     (Array_Index_Type (Positive (Left) - Right));
   pragma Inline ("-");
   
   overriding function "<" (Left, Right : Array_Index_Type) return Boolean is
     (Positive (Left) < Positive (Right));
   pragma Inline ("<");
   
   overriding function ">" (Left, Right : Array_Index_Type) return Boolean is
     (Positive (Left) > Positive (Right));
   pragma Inline (">");
   
   overriding function "<=" (Left, Right : Array_Index_Type) return Boolean is
     (Positive (Left) <= Positive (Right));
   pragma Inline ("<=");
   
   overriding function ">=" (Left, Right : Array_Index_Type) return Boolean is
     (Positive (Left) >= Positive (Right));
   pragma Inline (">=");
   
   --  ==========================================================================
   --  Arithmetic Operations for Worker_Count_Type
   --  ==========================================================================
   
   function "+" (Left : Worker_Count_Type; Right : Natural) return Worker_Count_Type is
     (Worker_Count_Type (Positive (Left) + Right));
   pragma Inline ("+");
   
   function "-" (Left : Worker_Count_Type; Right : Natural) return Worker_Count_Type is
     (Worker_Count_Type (Positive (Left) - Right));
   pragma Inline ("-");
   
   overriding function "<" (Left, Right : Worker_Count_Type) return Boolean is
     (Positive (Left) < Positive (Right));
   pragma Inline ("<");
   
   overriding function ">" (Left, Right : Worker_Count_Type) return Boolean is
     (Positive (Left) > Positive (Right));
   pragma Inline (">");
   
   overriding function "<=" (Left, Right : Worker_Count_Type) return Boolean is
     (Positive (Left) <= Positive (Right));
   pragma Inline ("<=");
   
   overriding function ">=" (Left, Right : Worker_Count_Type) return Boolean is
     (Positive (Left) >= Positive (Right));
   pragma Inline (">=");
   
   --  ==========================================================================
   --  Arithmetic Operations for Count Types (Retry, Failure, Success)
   --  ==========================================================================
   
   function "+" (Left : Retry_Count_Type; Right : Natural) return Retry_Count_Type is
     (Retry_Count_Type (Positive (Left) + Right))
   with
     Pre => Positive (Left) + Right >= 1,
     Post => "+"'Result >= 1;
   pragma Inline ("+");
   
   function "+" (Left : Failure_Count_Type; Right : Natural) return Failure_Count_Type is
     (Failure_Count_Type (Natural (Left) + Right));
   pragma Inline ("+");
   
   function "+" (Left : Success_Count_Type; Right : Natural) return Success_Count_Type is
     (Success_Count_Type (Natural (Left) + Right));
   pragma Inline ("+");
   
   --  Note: Using Natural for right operand to avoid conflicts with Positive subtype
   
   --  Comparison operations for count types
   overriding function "<" (Left, Right : Retry_Count_Type) return Boolean is
     (Positive (Left) < Positive (Right));
   pragma Inline ("<");
   
   overriding function "<" (Left, Right : Failure_Count_Type) return Boolean is
     (Natural (Left) < Natural (Right));
   pragma Inline ("<");
   
   overriding function "<" (Left, Right : Success_Count_Type) return Boolean is
     (Natural (Left) < Natural (Right));
   pragma Inline ("<");
   
   overriding function ">" (Left, Right : Retry_Count_Type) return Boolean is
     (Positive (Left) > Positive (Right));
   pragma Inline (">");
   
   overriding function ">" (Left, Right : Failure_Count_Type) return Boolean is
     (Natural (Left) > Natural (Right));
   pragma Inline (">");
   
   overriding function ">" (Left, Right : Success_Count_Type) return Boolean is
     (Natural (Left) > Natural (Right));
   pragma Inline (">");
   
   --  ==========================================================================
   --  Arithmetic Operations for Sequence_Number_Type
   --  ==========================================================================
   
   function "+" (Left : Sequence_Number_Type; Right : Natural) return Sequence_Number_Type is
     (Sequence_Number_Type (Natural (Left) + Right))
   with
     Pre => Natural (Left) + Right <= Natural'Last,
     Post => "+"'Result >= Left;
   pragma Inline ("+");
   
   --  Note: Using Natural for right operand to avoid conflicts with Positive subtype
   
   overriding function "<" (Left, Right : Sequence_Number_Type) return Boolean is
     (Natural (Left) < Natural (Right));
   pragma Inline ("<");
   
   overriding function ">" (Left, Right : Sequence_Number_Type) return Boolean is
     (Natural (Left) > Natural (Right));
   pragma Inline (">");
   
   overriding function "<=" (Left, Right : Sequence_Number_Type) return Boolean is
     (Natural (Left) <= Natural (Right));
   pragma Inline ("<=");
   
   overriding function ">=" (Left, Right : Sequence_Number_Type) return Boolean is
     (Natural (Left) >= Natural (Right));
   pragma Inline (">=");

end Abohlib.Core.Domain.Types.Counts;