--   =============================================================================
--   Test_Bytes_Arithmetic - Bytes Types Arithmetic Operations Unit Tests
--   =============================================================================
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--   =============================================================================

pragma Ada_2022;
pragma Warnings (Off, "subprogram body has no previous spec");

with Abohlib.Core.Domain.Types.Bytes;

package body Test_Bytes_Arithmetic is

   use Abohlib.Core.Domain.Types.Bytes;

--   ==========================================================================
--   Test Cases for SI_Bytes_Type
--   ==========================================================================

   function Test_SI_Bytes_Addition
     (Output : access Test_Output_Port'Class) return Boolean is
      Left   : constant SI_Bytes_Type := 1000;
      Right  : constant SI_Bytes_Type := 2000;
      Result : constant SI_Bytes_Type := Left + Right;
   begin
      if Result /= 3000 then
         Output.Write_Line ("FAIL: SI_Bytes addition - Expected 3000, got" &
                           SI_Bytes_Type'Image (Result));
         return False;
      end if;
      return True;
   end Test_SI_Bytes_Addition;

   function Test_SI_Bytes_Multiplication
     (Output : access Test_Output_Port'Class) return Boolean is
      Value  : constant SI_Bytes_Type := 1000;
      Factor : constant Natural := 5;
      Result : constant SI_Bytes_Type := Value * Factor;
      Result_Reverse : constant SI_Bytes_Type := Factor * Value;
   begin
      if Result /= 5000 then
         Output.Write_Line ("FAIL: SI_Bytes multiplication - Expected 5000, got" &
                           SI_Bytes_Type'Image (Result));
         return False;
      end if;

      if Result_Reverse /= 5000 then
         Output.Write_Line ("FAIL: SI_Bytes reverse multiplication - Expected 5000, got" &
                           SI_Bytes_Type'Image (Result_Reverse));
         return False;
      end if;
      return True;
   end Test_SI_Bytes_Multiplication;

   function Test_SI_Bytes_Comparison
     (Output : access Test_Output_Port'Class) return Boolean is
      Small  : constant SI_Bytes_Type := 1000;
      Large  : constant SI_Bytes_Type := 2000;
   begin
      if not (Small < Large) then
         Output.Write_Line ("FAIL: SI_Bytes comparison - Small should be < Large");
         return False;
      end if;

      if not (Large > Small) then
         Output.Write_Line ("FAIL: SI_Bytes comparison - Large should be > Small");
         return False;
      end if;

      if not (Small <= Large) then
         Output.Write_Line ("FAIL: SI_Bytes comparison - Small should be <= Large");
         return False;
      end if;

      if not (Large >= Small) then
         Output.Write_Line ("FAIL: SI_Bytes comparison - Large should be >= Small");
         return False;
      end if;
      return True;
   end Test_SI_Bytes_Comparison;

   function Test_SI_Bytes_Modulo
     (Output : access Test_Output_Port'Class) return Boolean is
      Value  : constant SI_Bytes_Type := 1000;
      Divisor : constant Positive := 300;
      Result : constant SI_Bytes_Type := Value mod Divisor;
   begin
      if Result /= 100 then
         Output.Write_Line ("FAIL: SI_Bytes modulo - Expected 100, got" &
                           SI_Bytes_Type'Image (Result));
         return False;
      end if;
      return True;
   end Test_SI_Bytes_Modulo;

--   ==========================================================================
--   Test Cases for File_Size_Type
--   ==========================================================================

   function Test_File_Size_Addition
     (Output : access Test_Output_Port'Class) return Boolean is
      Size1  : constant File_Size_Type := 1024;
      Size2  : constant File_Size_Type := 2048;
      Result : constant File_Size_Type := Size1 + Size2;
   begin
      if Result /= 3072 then
         Output.Write_Line ("FAIL: File_Size addition - Expected 3072, got" &
                           File_Size_Type'Image (Result));
         return False;
      end if;
      return True;
   end Test_File_Size_Addition;

   function Test_File_Size_Multiplication
     (Output : access Test_Output_Port'Class) return Boolean is
      Size   : constant File_Size_Type := 1024;
      Factor : constant Natural := 4;
      Result : constant File_Size_Type := Size * Factor;
      Result_Reverse : constant File_Size_Type := Factor * Size;
   begin
      if Result /= 4096 then
         Output.Write_Line ("FAIL: File_Size multiplication - Expected 4096, got" &
                           File_Size_Type'Image (Result));
         return False;
      end if;

      if Result_Reverse /= 4096 then
         Output.Write_Line ("FAIL: File_Size reverse multiplication - Expected 4096, got" &
                           File_Size_Type'Image (Result_Reverse));
         return False;
      end if;
      return True;
   end Test_File_Size_Multiplication;

   function Test_File_Size_Comparison
     (Output : access Test_Output_Port'Class) return Boolean is
      Small  : constant File_Size_Type := 1024;
      Large  : constant File_Size_Type := 2048;
   begin
      if not (Small < Large) then
         Output.Write_Line ("FAIL: File_Size comparison - Small should be < Large");
         return False;
      end if;

      if not (Large >= Small) then
         Output.Write_Line ("FAIL: File_Size comparison - Large should be >= Small");
         return False;
      end if;
      return True;
   end Test_File_Size_Comparison;

--   ==========================================================================
--   Test Cases for Buffer_Size_Type
--   ==========================================================================

   function Test_Buffer_Size_Addition
     (Output : access Test_Output_Port'Class) return Boolean is
      Size1  : constant Buffer_Size_Type := 512;
      Size2  : constant Buffer_Size_Type := 256;
      Result : constant Buffer_Size_Type := Size1 + Size2;
   begin
      if Result /= 768 then
         Output.Write_Line ("FAIL: Buffer_Size addition - Expected 768, got" &
                           Buffer_Size_Type'Image (Result));
         return False;
      end if;
      return True;
   end Test_Buffer_Size_Addition;

   function Test_Buffer_Size_Multiplication
     (Output : access Test_Output_Port'Class) return Boolean is
      Size   : constant Buffer_Size_Type := 256;
      Factor : constant Natural := 3;
      Result : constant Buffer_Size_Type := Size * Factor;
      Result_Reverse : constant Buffer_Size_Type := Factor * Size;
   begin
      if Result /= 768 then
         Output.Write_Line ("FAIL: Buffer_Size multiplication - Expected 768, got" &
                           Buffer_Size_Type'Image (Result));
         return False;
      end if;

      if Result_Reverse /= 768 then
         Output.Write_Line ("FAIL: Buffer_Size reverse multiplication - Expected 768, got" &
                           Buffer_Size_Type'Image (Result_Reverse));
         return False;
      end if;
      return True;
   end Test_Buffer_Size_Multiplication;

--   ==========================================================================
--   Test Cases for Type Safety (should not compile if uncommented)
--   ==========================================================================

--   function Test_Type_Safety_Violations
--     (Output : access Test_Output_Port'Class) return Boolean is
--      SI_Val : constant SI_Bytes_Type := 1000;
--      IEC_Val : constant IEC_Bytes_Type := 1024;
--      --  Result : constant SI_Bytes_Type := SI_Val + IEC_Val;  -- Should not compile
--   begin
--      return True;
--   end Test_Type_Safety_Violations;

--   ==========================================================================
--   Test Runner
--   ==========================================================================

   function Run_All_Tests
     (Output : access Test_Output_Port'Class) return Test_Stats_Result.Result is
      Stats : Test_Stats := (0, 0, 0);
   begin
      Output.Write_Line ("=== Bytes Arithmetic Operations Tests ===");

--   SI_Bytes_Type tests
      Run_Test ("SI_Bytes Addition", Test_SI_Bytes_Addition'Access, Output, Stats);
      Run_Test ("SI_Bytes Multiplication", Test_SI_Bytes_Multiplication'Access, Output, Stats);
      Run_Test ("SI_Bytes Comparison", Test_SI_Bytes_Comparison'Access, Output, Stats);
      Run_Test ("SI_Bytes Modulo", Test_SI_Bytes_Modulo'Access, Output, Stats);

--   File_Size_Type tests
      Run_Test ("File_Size Addition", Test_File_Size_Addition'Access, Output, Stats);
      Run_Test ("File_Size Multiplication", Test_File_Size_Multiplication'Access, Output, Stats);
      Run_Test ("File_Size Comparison", Test_File_Size_Comparison'Access, Output, Stats);

--   Buffer_Size_Type tests
      Run_Test ("Buffer_Size Addition", Test_Buffer_Size_Addition'Access, Output, Stats);
      Run_Test ("Buffer_Size Multiplication", Test_Buffer_Size_Multiplication'Access, Output, Stats);

      Print_Test_Summary ("Bytes Arithmetic Tests", Stats, Output);

      return Test_Stats_Result.Ok (Stats);
   end Run_All_Tests;

end Test_Bytes_Arithmetic;

pragma Warnings (On, "subprogram body has no previous spec");
