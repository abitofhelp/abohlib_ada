--  =============================================================================
--  Abohlib.Core.Domain.Utilities.Byte_Formatter - Implementation
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

with Abohlib.Core.Domain.Constants.Bytes;
with Ada.Strings.Fixed;

package body Abohlib.Core.Domain.Utilities.Byte_Formatter is

   use Abohlib.Core.Domain.Constants.Bytes;
   use Abohlib.Core.Domain.Types.Bytes;
   use Abohlib.Core.Domain.Types.Formatting;
   use Ada.Strings.Fixed;

   --  Constants
   Decimal_Display_Threshold : constant := 0.005;  -- Threshold for displaying decimal places

   --  Format bytes to human readable string
   function Format_Bytes (Bytes : Abohlib.Core.Domain.Types.Bytes.SI_Bytes_Type) return String is
   begin
      return Format_Bytes_With_Precision (Bytes, 2);
   end Format_Bytes;
   
   --  Convenience overload for Long_Long_Integer
   function Format_Bytes (Bytes : Long_Long_Integer) return String is
   begin
      return Format_Bytes (SI_Bytes_Type (Bytes));
   end Format_Bytes;

   --  Format bytes with configurable decimal places
   function Format_Bytes_With_Precision
     (Bytes : Abohlib.Core.Domain.Types.Bytes.SI_Bytes_Type; 
      Decimal_Places : Abohlib.Core.Domain.Types.Formatting.Decimal_Precision_Type := 2) return String
   is

      function Format_Value
        (Value : Float; Unit : String; Precision : Decimal_Precision_Type) return String
      is
         --  Truncate to get whole part (don't round)
         Whole    : constant Long_Long_Integer :=
           Long_Long_Integer (Float'Floor (Value));
         Fraction : constant Float := Value - Float (Whole);

         function Get_Decimal_String
           (Frac : Float; Prec : Decimal_Precision_Type) return String
         is
            Scaled : constant Natural := Natural (Frac * Float (10 ** Natural(Prec)));
            Str    : constant String :=
              Trim (Natural'Image (Scaled), Ada.Strings.Left);
         begin
            --  Pad with leading zeros if needed
            if Str'Length < Natural(Prec) then
               return (1 .. Natural(Prec) - Str'Length => '0') & Str;
            else
               return Str (Str'First .. Str'First + Natural(Prec) - 1);
            end if;
         end Get_Decimal_String;

      begin
         if Precision = 0 or else Fraction < Decimal_Display_Threshold then
            return Trim (Whole'Image, Ada.Strings.Left) & " " & Unit;
         else
            return
              Trim (Whole'Image, Ada.Strings.Left)
              & "."
              & Get_Decimal_String (Fraction, Precision)
              & " "
              & Unit;
         end if;
      end Format_Value;

   begin
      if Bytes >= SI_TB then
         return
           Format_Value (Float (Bytes) / Float(SI_TB), "TB", Decimal_Places);
      elsif Bytes >= SI_GB then
         return
           Format_Value (Float (Bytes) / Float(SI_GB), "GB", Decimal_Places);
      elsif Bytes >= SI_MB then
         return
           Format_Value (Float (Bytes) / Float(SI_MB), "MB", Decimal_Places);
      elsif Bytes >= SI_KB then
         return
           Format_Value (Float (Bytes) / Float(SI_KB), "KB", Decimal_Places);
      else
         return Trim (Bytes'Image, Ada.Strings.Left) & " B";
      end if;
   end Format_Bytes_With_Precision;
   
   --  Convenience overload for Long_Long_Integer
   function Format_Bytes_With_Precision
     (Bytes : Long_Long_Integer; 
      Decimal_Places : Abohlib.Core.Domain.Types.Formatting.Decimal_Precision_Type := 2) return String
   is
   begin
      return Format_Bytes_With_Precision (SI_Bytes_Type (Bytes), Decimal_Places);
   end Format_Bytes_With_Precision;

end Abohlib.Core.Domain.Utilities.Byte_Formatter;
