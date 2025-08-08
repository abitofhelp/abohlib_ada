--  =============================================================================
--  Abohlib.Core.Domain.Types.Bytes - Byte and Memory Strong Types
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Purpose:
--    Provides strong types for byte-related values to prevent mixing of
--    SI (decimal) and IEC (binary) units, and different byte measurements.
--
--  Design:
--    - Separate types for SI (1000-based) and IEC (1024-based) units
--    - Arithmetic operations for common calculations
--    - Conversion functions between units
--    - Type safety prevents mixing incompatible units
--  =============================================================================

pragma Ada_2022;

package Abohlib.Core.Domain.Types.Bytes is

   --  ==========================================================================
   --  Base Byte Types
   --  ==========================================================================
   
   --  Base type for all byte measurements
   type Bytes_Type is new Long_Long_Integer
   with Dynamic_Predicate => Bytes_Type >= 0;
   
   --  Strong types for byte units (prevent mixing SI and IEC)
   type SI_Bytes_Type is new Bytes_Type;   -- Decimal (1000-based)
   type IEC_Bytes_Type is new Bytes_Type;  -- Binary (1024-based)
   
   --  Specific byte measurements
   type File_Size_Type is new Bytes_Type;
   subtype Limited_Buffer_Size is Positive range 1 .. 100_000_000;
   type Buffer_Size_Type is new Limited_Buffer_Size;
   type Memory_Size_Type is new Bytes_Type;
   
   --  ==========================================================================
   --  Arithmetic Operations for SI Bytes
   --  ==========================================================================
   
   overriding function "+" (Left, Right : SI_Bytes_Type) return SI_Bytes_Type is
     (SI_Bytes_Type (Long_Long_Integer (Left) + Long_Long_Integer (Right)));
   pragma Inline ("+");
   
   overriding function "-" (Left, Right : SI_Bytes_Type) return SI_Bytes_Type is
     (SI_Bytes_Type (Long_Long_Integer (Left) - Long_Long_Integer (Right)));
   pragma Inline ("-");
   
   function "*" (Left : SI_Bytes_Type; Right : Natural) return SI_Bytes_Type is
     (SI_Bytes_Type (Long_Long_Integer (Left) * Long_Long_Integer (Right)));
   pragma Inline ("*");
   
   function "/" (Left : SI_Bytes_Type; Right : Positive) return SI_Bytes_Type is
     (SI_Bytes_Type (Long_Long_Integer (Left) / Long_Long_Integer (Right)));
   pragma Inline ("/");
   
   --  Reverse multiplication
   function "*" (Left : Natural; Right : SI_Bytes_Type) return SI_Bytes_Type is
     (SI_Bytes_Type (Long_Long_Integer (Left) * Long_Long_Integer (Right)));
   pragma Inline ("*");
   
   --  Comparison operations
   overriding function "<" (Left, Right : SI_Bytes_Type) return Boolean is
     (Long_Long_Integer (Left) < Long_Long_Integer (Right));
   pragma Inline ("<");
   
   overriding function ">" (Left, Right : SI_Bytes_Type) return Boolean is
     (Long_Long_Integer (Left) > Long_Long_Integer (Right));
   pragma Inline (">");
   
   overriding function "<=" (Left, Right : SI_Bytes_Type) return Boolean is
     (Long_Long_Integer (Left) <= Long_Long_Integer (Right));
   pragma Inline ("<=");
   
   overriding function ">=" (Left, Right : SI_Bytes_Type) return Boolean is
     (Long_Long_Integer (Left) >= Long_Long_Integer (Right));
   pragma Inline (">=");
   
   --  ==========================================================================
   --  Arithmetic Operations for IEC Bytes
   --  ==========================================================================
   
   overriding function "+" (Left, Right : IEC_Bytes_Type) return IEC_Bytes_Type is
     (IEC_Bytes_Type (Long_Long_Integer (Left) + Long_Long_Integer (Right)));
   pragma Inline ("+");
   
   overriding function "-" (Left, Right : IEC_Bytes_Type) return IEC_Bytes_Type is
     (IEC_Bytes_Type (Long_Long_Integer (Left) - Long_Long_Integer (Right)));
   pragma Inline ("-");
   
   function "*" (Left : IEC_Bytes_Type; Right : Natural) return IEC_Bytes_Type is
     (IEC_Bytes_Type (Long_Long_Integer (Left) * Long_Long_Integer (Right)));
   pragma Inline ("*");
   
   function "/" (Left : IEC_Bytes_Type; Right : Positive) return IEC_Bytes_Type is
     (IEC_Bytes_Type (Long_Long_Integer (Left) / Long_Long_Integer (Right)));
   pragma Inline ("/");
   
   --  Reverse multiplication
   function "*" (Left : Natural; Right : IEC_Bytes_Type) return IEC_Bytes_Type is
     (IEC_Bytes_Type (Long_Long_Integer (Left) * Long_Long_Integer (Right)));
   pragma Inline ("*");
   
   --  Comparison operations
   overriding function "<" (Left, Right : IEC_Bytes_Type) return Boolean is
     (Long_Long_Integer (Left) < Long_Long_Integer (Right));
   pragma Inline ("<");
   
   overriding function ">" (Left, Right : IEC_Bytes_Type) return Boolean is
     (Long_Long_Integer (Left) > Long_Long_Integer (Right));
   pragma Inline (">");
   
   overriding function "<=" (Left, Right : IEC_Bytes_Type) return Boolean is
     (Long_Long_Integer (Left) <= Long_Long_Integer (Right));
   pragma Inline ("<=");
   
   overriding function ">=" (Left, Right : IEC_Bytes_Type) return Boolean is
     (Long_Long_Integer (Left) >= Long_Long_Integer (Right));
   pragma Inline (">=");
   
   --  ==========================================================================
   --  Arithmetic Operations for File_Size_Type
   --  ==========================================================================
   
   overriding function "+" (Left, Right : File_Size_Type) return File_Size_Type is
     (File_Size_Type (Long_Long_Integer (Left) + Long_Long_Integer (Right)));
   pragma Inline ("+");
   
   overriding function "-" (Left, Right : File_Size_Type) return File_Size_Type is
     (File_Size_Type (Long_Long_Integer (Left) - Long_Long_Integer (Right)));
   pragma Inline ("-");
   
   function "*" (Left : File_Size_Type; Right : Natural) return File_Size_Type is
     (File_Size_Type (Long_Long_Integer (Left) * Long_Long_Integer (Right)));
   pragma Inline ("*");
   
   function "*" (Left : Natural; Right : File_Size_Type) return File_Size_Type is
     (File_Size_Type (Long_Long_Integer (Left) * Long_Long_Integer (Right)));
   pragma Inline ("*");
   
   function "/" (Left : File_Size_Type; Right : Positive) return File_Size_Type is
     (File_Size_Type (Long_Long_Integer (Left) / Long_Long_Integer (Right)));
   pragma Inline ("/");
   
   function "mod" (Left : File_Size_Type; Right : Positive) return File_Size_Type is
     (File_Size_Type (Long_Long_Integer (Left) mod Long_Long_Integer (Right)));
   pragma Inline ("mod");
   
   overriding function "<" (Left, Right : File_Size_Type) return Boolean is
     (Long_Long_Integer (Left) < Long_Long_Integer (Right));
   pragma Inline ("<");
   
   overriding function ">" (Left, Right : File_Size_Type) return Boolean is
     (Long_Long_Integer (Left) > Long_Long_Integer (Right));
   pragma Inline (">");
   
   overriding function "<=" (Left, Right : File_Size_Type) return Boolean is
     (Long_Long_Integer (Left) <= Long_Long_Integer (Right));
   pragma Inline ("<=");
   
   overriding function ">=" (Left, Right : File_Size_Type) return Boolean is
     (Long_Long_Integer (Left) >= Long_Long_Integer (Right));
   pragma Inline (">=");
   
   --  ==========================================================================
   --  Arithmetic Operations for Memory_Size_Type
   --  ==========================================================================
   
   overriding function "+" (Left, Right : Memory_Size_Type) return Memory_Size_Type is
     (Memory_Size_Type (Long_Long_Integer (Left) + Long_Long_Integer (Right)));
   pragma Inline ("+");
   
   overriding function "-" (Left, Right : Memory_Size_Type) return Memory_Size_Type is
     (Memory_Size_Type (Long_Long_Integer (Left) - Long_Long_Integer (Right)));
   pragma Inline ("-");
   
   function "*" (Left : Memory_Size_Type; Right : Natural) return Memory_Size_Type is
     (Memory_Size_Type (Long_Long_Integer (Left) * Long_Long_Integer (Right)));
   pragma Inline ("*");
   
   function "*" (Left : Natural; Right : Memory_Size_Type) return Memory_Size_Type is
     (Memory_Size_Type (Long_Long_Integer (Left) * Long_Long_Integer (Right)));
   pragma Inline ("*");
   
   function "/" (Left : Memory_Size_Type; Right : Positive) return Memory_Size_Type is
     (Memory_Size_Type (Long_Long_Integer (Left) / Long_Long_Integer (Right)));
   pragma Inline ("/");
   
   function "mod" (Left : Memory_Size_Type; Right : Positive) return Memory_Size_Type is
     (Memory_Size_Type (Long_Long_Integer (Left) mod Long_Long_Integer (Right)));
   pragma Inline ("mod");
   
   overriding function "<" (Left, Right : Memory_Size_Type) return Boolean is
     (Long_Long_Integer (Left) < Long_Long_Integer (Right));
   pragma Inline ("<");
   
   overriding function ">" (Left, Right : Memory_Size_Type) return Boolean is
     (Long_Long_Integer (Left) > Long_Long_Integer (Right));
   pragma Inline (">");
   
   overriding function "<=" (Left, Right : Memory_Size_Type) return Boolean is
     (Long_Long_Integer (Left) <= Long_Long_Integer (Right));
   pragma Inline ("<=");
   
   overriding function ">=" (Left, Right : Memory_Size_Type) return Boolean is
     (Long_Long_Integer (Left) >= Long_Long_Integer (Right));
   pragma Inline (">=");
   
   --  ==========================================================================
   --  Arithmetic Operations for Buffer_Size_Type
   --  ==========================================================================
   
   overriding function "+" (Left, Right : Buffer_Size_Type) return Buffer_Size_Type is
     (Buffer_Size_Type (Integer (Left) + Integer (Right)));
   pragma Inline ("+");
   
   overriding function "-" (Left, Right : Buffer_Size_Type) return Buffer_Size_Type is
     (Buffer_Size_Type (Integer (Left) - Integer (Right)));
   pragma Inline ("-");
   
   function "*" (Left : Buffer_Size_Type; Right : Natural) return Buffer_Size_Type is
     (Buffer_Size_Type (Integer (Left) * Right));
   pragma Inline ("*");
   
   function "*" (Left : Natural; Right : Buffer_Size_Type) return Buffer_Size_Type is
     (Buffer_Size_Type (Left * Integer (Right)));
   pragma Inline ("*");
   
   function "/" (Left : Buffer_Size_Type; Right : Positive) return Buffer_Size_Type is
     (Buffer_Size_Type (Integer (Left) / Right));
   pragma Inline ("/");
   
   function "mod" (Left : Buffer_Size_Type; Right : Positive) return Buffer_Size_Type is
     (Buffer_Size_Type (Integer (Left) mod Right));
   pragma Inline ("mod");
   
   overriding function "<" (Left, Right : Buffer_Size_Type) return Boolean is
     (Integer (Left) < Integer (Right));
   pragma Inline ("<");
   
   overriding function ">" (Left, Right : Buffer_Size_Type) return Boolean is
     (Integer (Left) > Integer (Right));
   pragma Inline (">");
   
   overriding function "<=" (Left, Right : Buffer_Size_Type) return Boolean is
     (Integer (Left) <= Integer (Right));
   pragma Inline ("<=");
   
   overriding function ">=" (Left, Right : Buffer_Size_Type) return Boolean is
     (Integer (Left) >= Integer (Right));
   pragma Inline (">=");
   
   --  ==========================================================================
   --  Additional Operations for SI and IEC Bytes
   --  ==========================================================================
   
   function "mod" (Left : SI_Bytes_Type; Right : Positive) return SI_Bytes_Type is
     (SI_Bytes_Type (Long_Long_Integer (Left) mod Long_Long_Integer (Right)));
   pragma Inline ("mod");
   
   function "mod" (Left : IEC_Bytes_Type; Right : Positive) return IEC_Bytes_Type is
     (IEC_Bytes_Type (Long_Long_Integer (Left) mod Long_Long_Integer (Right)));
   pragma Inline ("mod");
   
   --  ==========================================================================
   --  Conversion Functions
   --  ==========================================================================
   
   --  SI conversions (decimal)
   function To_SI_KB (Bytes : SI_Bytes_Type) return Float
   is (Float (Bytes) / 1_000.0);
   pragma Inline (To_SI_KB);
   
   function To_SI_MB (Bytes : SI_Bytes_Type) return Float
   is (Float (Bytes) / 1_000_000.0);
   pragma Inline (To_SI_MB);
   
   --  IEC conversions (binary)
   function To_IEC_KiB (Bytes : IEC_Bytes_Type) return Float
   is (Float (Bytes) / 1_024.0);
   pragma Inline (To_IEC_KiB);
   
   function To_IEC_MiB (Bytes : IEC_Bytes_Type) return Float
   is (Float (Bytes) / 1_048_576.0);
   pragma Inline (To_IEC_MiB);

   --  ==========================================================================
   --  Type Conversions (Requested by pipelib)
   --  ==========================================================================
   
   --  SI_Bytes_Type ↔ Long_Long_Integer conversions
   function To_Long_Long_Integer (Bytes : SI_Bytes_Type) return Long_Long_Integer is
     (Long_Long_Integer (Bytes))
   with
     Post => To_Long_Long_Integer'Result = Long_Long_Integer (Bytes);
   pragma Inline (To_Long_Long_Integer);
   
   function From_Long_Long_Integer (Value : Long_Long_Integer) return SI_Bytes_Type is
     (SI_Bytes_Type (Value))
   with
     Pre => Value >= 0,
     Post => From_Long_Long_Integer'Result = SI_Bytes_Type (Value);
   pragma Inline (From_Long_Long_Integer);
   
   --  Natural ↔ SI_Bytes_Type conversions
   function From_Natural (Value : Natural) return SI_Bytes_Type is
     (SI_Bytes_Type (Value))
   with
     Post => From_Natural'Result = SI_Bytes_Type (Value);
   pragma Inline (From_Natural);
   
   function To_Natural (Bytes : SI_Bytes_Type) return Natural is
     (Natural (Bytes))
   with
     Pre => Long_Long_Integer (Bytes) <= Long_Long_Integer (Natural'Last),
     Post => To_Natural'Result = Natural (Bytes);
   pragma Inline (To_Natural);

   --  Additional conversion functions for common operations
   
   --  Convert between SI and IEC bytes
   function To_IEC_Bytes (SI : SI_Bytes_Type) return IEC_Bytes_Type is
     (IEC_Bytes_Type (SI))
   with
     Post => To_IEC_Bytes'Result = IEC_Bytes_Type (SI);
   pragma Inline (To_IEC_Bytes);
   
   function To_SI_Bytes (IEC : IEC_Bytes_Type) return SI_Bytes_Type is
     (SI_Bytes_Type (IEC))
   with
     Post => To_SI_Bytes'Result = SI_Bytes_Type (IEC);
   pragma Inline (To_SI_Bytes);
   
   --  Create SI_Bytes_Type from common units
   --  These functions handle very large values safely:
   --  - From_KB: up to 9,223,372,036,854,775 KB (≈ 9.2 EB)
   --  - From_MB: up to 9,223,372,036,854 MB (≈ 9.2 PB)
   --  - From_GB: up to 9,223,372,036 GB (≈ 9,223 TB or 9.2 PB)
   
   function From_KB (KB : Long_Long_Integer) return SI_Bytes_Type is
     (SI_Bytes_Type (KB) * SI_Bytes_Type (1_000))
   with
     Pre => KB >= 0 and then KB <= Long_Long_Integer'Last / 1_000,
     Post => From_KB'Result = SI_Bytes_Type (KB) * SI_Bytes_Type (1_000);
   pragma Inline (From_KB);
   
   function From_MB (MB : Long_Long_Integer) return SI_Bytes_Type is
     (SI_Bytes_Type (MB) * SI_Bytes_Type (1_000_000))
   with
     Pre => MB >= 0 and then MB <= Long_Long_Integer'Last / 1_000_000,
     Post => From_MB'Result = SI_Bytes_Type (MB) * SI_Bytes_Type (1_000_000);
   pragma Inline (From_MB);
   
   function From_GB (GB : Long_Long_Integer) return SI_Bytes_Type is
     (SI_Bytes_Type (GB) * SI_Bytes_Type (1_000_000_000))
   with
     Pre => GB >= 0 and then GB <= Long_Long_Integer'Last / 1_000_000_000,
     Post => From_GB'Result = SI_Bytes_Type (GB) * SI_Bytes_Type (1_000_000_000);
   pragma Inline (From_GB);

end Abohlib.Core.Domain.Types.Bytes;