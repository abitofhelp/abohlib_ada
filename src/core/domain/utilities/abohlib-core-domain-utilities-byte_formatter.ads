--  =============================================================================
--  Abohlib.Core.Domain.Utilities.Byte_Formatter - Byte Formatting Utilities
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Purpose:
--    Provides utilities for formatting byte values into human-readable strings
--    using SI units (B, KB, MB, GB, TB).
--
--  Dependencies:
--    - Abohlib.Core.Domain.Types.Bytes: For SI_Bytes_Type
--    - Abohlib.Core.Domain.Types.Formatting: For Decimal_Precision_Type
--  =============================================================================

pragma Ada_2022;

with Abohlib.Core.Domain.Types.Bytes;
with Abohlib.Core.Domain.Types.Formatting;

package Abohlib.Core.Domain.Utilities.Byte_Formatter is

   use type Abohlib.Core.Domain.Types.Bytes.SI_Bytes_Type;

   --  Format bytes to human readable string with SI units
   --  Examples: "1.23 MB", "456 KB", "789 B"
   function Format_Bytes (Bytes : Abohlib.Core.Domain.Types.Bytes.SI_Bytes_Type) return String
   with Pre => Bytes >= 0, Post => Format_Bytes'Result'Length > 0;

   --  Format bytes with configurable decimal places
   function Format_Bytes_With_Precision
     (Bytes : Abohlib.Core.Domain.Types.Bytes.SI_Bytes_Type; 
      Decimal_Places : Abohlib.Core.Domain.Types.Formatting.Decimal_Precision_Type := 2) return String
   with
     Pre => Bytes >= 0,
     Post => Format_Bytes_With_Precision'Result'Length > 0;
     
   --  Convenience overloads for Long_Long_Integer
   function Format_Bytes (Bytes : Long_Long_Integer) return String
   with Pre => Bytes >= 0, Post => Format_Bytes'Result'Length > 0;
   
   function Format_Bytes_With_Precision
     (Bytes : Long_Long_Integer; 
      Decimal_Places : Abohlib.Core.Domain.Types.Formatting.Decimal_Precision_Type := 2) return String
   with
     Pre => Bytes >= 0,
     Post => Format_Bytes_With_Precision'Result'Length > 0;

end Abohlib.Core.Domain.Utilities.Byte_Formatter;
