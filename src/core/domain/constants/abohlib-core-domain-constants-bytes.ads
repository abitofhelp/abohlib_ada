--  =============================================================================
--  Abohlib.Constants.Bytes - Byte Size Constants
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Purpose:
--    Provides read-only byte size constants for use by domain value objects
--    and outer layers. These constants standardize byte calculations based on
--    international standards (SI and IEC).
--
--  Usage:
--    Import this package to access standardized byte size constants.
--    - SI constants (KB, MB, GB, etc.): Decimal-based (1 KB = 1,000 bytes)
--    - IEC constants (KiB, MiB, GiB, etc.): Binary-based (1 KiB = 1,024 bytes)
--    All constants are compile-time values ensuring zero runtime overhead.
--
--  Standards:
--    - SI: International System of Units (base 10)
--    - IEC: International Electrotechnical Commission 60027-2 (base 2)
--
--  Architectural Note:
--    These constants are placed in the domain layer because they are needed
--    by domain value objects (file sizes, chunk sizes, etc.). Following the
--    dependency rule, outer layers can access these constants:
--    - Application layer: For validation of uploads/downloads
--    - Infrastructure layer: For file system and network operations
--    - Presentation layer: For displaying file sizes to users
--  =============================================================================

pragma Ada_2022;

with Abohlib.Core.Domain.Types.Bytes; use Abohlib.Core.Domain.Types.Bytes;

package Abohlib.Core.Domain.Constants.Bytes is

   --  SI (base 10) constants - Decimal multipliers
   SI_BYTES_MULTIPLIER : constant := 1000;
   SI_KB : constant SI_Bytes_Type := SI_BYTES_MULTIPLIER;
   SI_MB : constant SI_Bytes_Type := SI_Bytes_Type(Long_Long_Integer(SI_KB) * SI_BYTES_MULTIPLIER);
   SI_GB : constant SI_Bytes_Type := SI_Bytes_Type(Long_Long_Integer(SI_MB) * SI_BYTES_MULTIPLIER);
   SI_TB : constant SI_Bytes_Type := SI_Bytes_Type(Long_Long_Integer(SI_GB) * SI_BYTES_MULTIPLIER);
   SI_PB : constant SI_Bytes_Type := SI_Bytes_Type(Long_Long_Integer(SI_TB) * SI_BYTES_MULTIPLIER);
   
   --  Long_Long_Integer versions for case statements and comparisons
   SI_KB_LLI : constant Long_Long_Integer := Long_Long_Integer (SI_KB);
   SI_MB_LLI : constant Long_Long_Integer := Long_Long_Integer (SI_MB);
   SI_GB_LLI : constant Long_Long_Integer := Long_Long_Integer (SI_GB);
   SI_TB_LLI : constant Long_Long_Integer := Long_Long_Integer (SI_TB);
   
   --  Conversion utilities using proper constants
   function To_MB_Float (Bytes : Long_Long_Integer) return Float is
     (Float (Bytes) / Float (SI_MB_LLI))
   with
     Pre => Bytes >= 0,
     Post => To_MB_Float'Result >= 0.0;
   pragma Inline (To_MB_Float);

   --  IEC (base 2) constants - Binary multipliers
   IEC_BYTES_MULTIPLIER : constant := 1024;
   IEC_KiB : constant IEC_Bytes_Type := IEC_BYTES_MULTIPLIER;
   IEC_MiB : constant IEC_Bytes_Type := IEC_Bytes_Type(Long_Long_Integer(IEC_KiB) * IEC_BYTES_MULTIPLIER);
   IEC_GiB : constant IEC_Bytes_Type := IEC_Bytes_Type(Long_Long_Integer(IEC_MiB) * IEC_BYTES_MULTIPLIER);
   IEC_TiB : constant IEC_Bytes_Type := IEC_Bytes_Type(Long_Long_Integer(IEC_GiB) * IEC_BYTES_MULTIPLIER);
   IEC_PiB : constant IEC_Bytes_Type := IEC_Bytes_Type(Long_Long_Integer(IEC_TiB) * IEC_BYTES_MULTIPLIER);

end Abohlib.Core.Domain.Constants.Bytes;
