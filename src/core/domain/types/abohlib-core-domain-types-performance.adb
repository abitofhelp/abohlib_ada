--  =============================================================================
--  Abohlib.Core.Domain.Types.Performance - Implementation
--  =============================================================================

pragma Ada_2022;

with Abohlib.Core.Domain.Constants.Bytes;

package body Abohlib.Core.Domain.Types.Performance is

   use Abohlib.Core.Domain.Constants.Bytes;

   --  ==========================================================================
   --  Calculate MB/s from bytes and duration
   --  ==========================================================================
   
   function Calculate_MB_Per_Second
     (Bytes    : Abohlib.Core.Domain.Types.Bytes.SI_Bytes_Type;
      Duration : Standard.Duration) return MB_Per_Second_Type
   is
      Bytes_Float : constant Float := Float (Bytes);
      MB_Float    : constant Float := Float (SI_MB);
      Seconds     : constant Float := Float (Duration);
   begin
      return MB_Per_Second_Type (Bytes_Float / MB_Float / Seconds);
   end Calculate_MB_Per_Second;
   
   --  Overload for Long_Long_Integer
   function Calculate_MB_Per_Second
     (Bytes    : Long_Long_Integer;
      Duration : Standard.Duration) return MB_Per_Second_Type
   is
      Bytes_Float : constant Float := Float (Bytes);
      MB_Float    : constant Float := Float (SI_MB);
      Seconds     : constant Float := Float (Duration);
   begin
      return MB_Per_Second_Type (Bytes_Float / MB_Float / Seconds);
   end Calculate_MB_Per_Second;

   --  ==========================================================================
   --  Calculate percentage from part and whole
   --  ==========================================================================
   
   function Calculate_Percentage
     (Part  : Abohlib.Core.Domain.Types.Bytes.SI_Bytes_Type;
      Whole : Abohlib.Core.Domain.Types.Bytes.SI_Bytes_Type) return Percentage_Type
   is
      Part_LL  : constant Long_Long_Integer := Long_Long_Integer (Part);
      Whole_LL : constant Long_Long_Integer := Long_Long_Integer (Whole);
   begin
      --  Delegate to the generic Math function
      return Abohlib.Core.Domain.Math.Calculate_Percentage (Part_LL, Whole_LL);
   end Calculate_Percentage;

end Abohlib.Core.Domain.Types.Performance;