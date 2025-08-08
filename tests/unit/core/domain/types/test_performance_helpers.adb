--   =============================================================================
--   Test_Performance_Helpers - Tests for Performance Helper Functions
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--   =============================================================================

pragma Ada_2022;
pragma Warnings (Off, "subprogram body has no previous spec");

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Abohlib.Core.Domain.Types.Performance; use Abohlib.Core.Domain.Types.Performance;
with Abohlib.Core.Domain.Types.Bytes; use Abohlib.Core.Domain.Types.Bytes;
with Abohlib.Core.Domain.Constants.Bytes; use Abohlib.Core.Domain.Constants.Bytes;
with Abohlib.Core.Domain.Math; use Abohlib.Core.Domain.Math;
with Abohlib.Core.Domain.Result;
with Abohlib.Core.Testing;

procedure Test_Performance_Helpers is

   use Abohlib.Core.Testing;

   package Test_Result is new Abohlib.Core.Domain.Result.Result_Package
     (Ok_Type => Boolean, Err_Type => Unbounded_String);

--   Test output implementation
   type Test_Output_Port is new Output_Port_Interface with null record;

   overriding procedure Log_Message
     (Self : Test_Output_Port; Message : String) is
   begin
      Put_Line (Message);
   end Log_Message;

   Output : aliased Test_Output_Port;

--   =========================================================================
--   Calculate_MB_Per_Second Tests
--   =========================================================================

   function Test_MB_Per_Second_SI_Bytes
     (Output : access Test_Output_Port'Class) return Boolean is
      Bytes    : constant SI_Bytes_Type := From_Natural (100) * SI_MB;  -- 100 MB
      Duration : constant Standard.Duration := 2.5;  -- 2.5 seconds
      Result   : constant MB_Per_Second_Type := Calculate_MB_Per_Second (Bytes, Duration);
      Expected : constant Float := 40.0;  -- 100 MB / 2.5 s = 40 MB/s
   begin
      Assert (abs (Float (Result) - Expected) < 0.01,
              "MB/s calculation failed: expected " & Expected'Image &
              ", got " & Float (Result)'Image);
      return True;
   end Test_MB_Per_Second_SI_Bytes;

   function Test_MB_Per_Second_Long_Long_Integer
     (Output : access Test_Output_Port'Class) return Boolean is
      Bytes    : constant Long_Long_Integer := 100_000_000;  -- 100 MB
      Duration : constant Standard.Duration := 2.5;  -- 2.5 seconds
      Result   : constant MB_Per_Second_Type := Calculate_MB_Per_Second (Bytes, Duration);
      Expected : constant Float := 40.0;  -- 100 MB / 2.5 s = 40 MB/s
   begin
      Assert (abs (Float (Result) - Expected) < 0.01,
              "MB/s calculation (Long_Long_Integer) failed");
      return True;
   end Test_MB_Per_Second_Long_Long_Integer;

   function Test_MB_Per_Second_Zero_Bytes
     (Output : access Test_Output_Port'Class) return Boolean is
      Bytes    : constant SI_Bytes_Type := 0;
      Duration : constant Standard.Duration := 1.0;
      Result   : constant MB_Per_Second_Type := Calculate_MB_Per_Second (Bytes, Duration);
   begin
      Assert (Float (Result) = 0.0, "Zero bytes should give 0 MB/s");
      return True;
   end Test_MB_Per_Second_Zero_Bytes;

   function Test_MB_Per_Second_Large_Values
     (Output : access Test_Output_Port'Class) return Boolean is
      Bytes    : constant Long_Long_Integer := 10_000_000_000;  -- 10 GB
      Duration : constant Standard.Duration := 10.0;  -- 10 seconds
      Result   : constant MB_Per_Second_Type := Calculate_MB_Per_Second (Bytes, Duration);
      Expected : constant Float := 1000.0;  -- 10 GB / 10 s = 1000 MB/s
   begin
      Assert (abs (Float (Result) - Expected) < 0.01,
              "Large value MB/s calculation failed");
      return True;
   end Test_MB_Per_Second_Large_Values;

   function Test_MB_Per_Second_Small_Duration
     (Output : access Test_Output_Port'Class) return Boolean is
      Bytes    : constant SI_Bytes_Type := From_MB (1);  -- 1 MB
      Duration : constant Standard.Duration := 0.001;  -- 1 ms
      Result   : constant MB_Per_Second_Type := Calculate_MB_Per_Second (Bytes, Duration);
      Expected : constant Float := 1000.0;  -- 1 MB / 0.001 s = 1000 MB/s
   begin
      Assert (abs (Float (Result) - Expected) < 0.01,
              "Small duration MB/s calculation failed");
      return True;
   end Test_MB_Per_Second_Small_Duration;

--   =========================================================================
--   Calculate_Percentage Tests
--   =========================================================================

   function Test_Percentage_SI_Bytes
     (Output : access Test_Output_Port'Class) return Boolean is
      Part     : constant SI_Bytes_Type := From_Natural (75) * SI_MB;  -- 75 MB
      Whole    : constant SI_Bytes_Type := From_Natural (100) * SI_MB; -- 100 MB
      Result   : constant Percentage_Type := Calculate_Percentage (Part, Whole);
      Expected : constant Float := 75.0;
   begin
      Assert (abs (Float (Result) - Expected) < 0.01,
              "Percentage calculation failed: expected " & Expected'Image &
              ", got " & Float (Result)'Image);
      return True;
   end Test_Percentage_SI_Bytes;

--   Note: Natural percentage calculation moved to Math module tests

   function Test_Percentage_Zero_Part
     (Output : access Test_Output_Port'Class) return Boolean is
      Part     : constant Natural := 0;
      Whole    : constant Natural := 100;
      Result   : constant Percentage_Type := Calculate_Percentage (Part, Whole);
   begin
      Assert (Float (Result) = 0.0, "Zero part should give 0%");
      return True;
   end Test_Percentage_Zero_Part;

   function Test_Percentage_Equal_Part_Whole
     (Output : access Test_Output_Port'Class) return Boolean is
      Part     : constant SI_Bytes_Type := From_GB (1);
      Whole    : constant SI_Bytes_Type := From_GB (1);
      Result   : constant Percentage_Type := Calculate_Percentage (Part, Whole);
   begin
      Assert (Float (Result) = 100.0, "Equal part and whole should give 100%");
      return True;
   end Test_Percentage_Equal_Part_Whole;

--   Note: Generic percentage tests moved to Math module

--   Note: Precision tests moved to Math module

--   =========================================================================
--   Edge Case Tests
--   =========================================================================

   function Test_MB_Per_Second_Very_Small_Values
     (Output : access Test_Output_Port'Class) return Boolean is
      Bytes    : constant SI_Bytes_Type := 100;  -- 100 bytes
      Duration : constant Standard.Duration := 1.0;
      Result   : constant MB_Per_Second_Type := Calculate_MB_Per_Second (Bytes, Duration);
      Expected : constant Float := 0.0001;  -- 100 bytes / 1 MB = 0.0001 MB/s
   begin
      Assert (abs (Float (Result) - Expected) < 0.00001,
              "Very small value MB/s calculation failed");
      return True;
   end Test_MB_Per_Second_Very_Small_Values;

--   Note: Small ratio tests moved to Math module

--   =========================================================================
--   Integration with Other Types
--   =========================================================================

   function Test_Integration_With_Compression_Ratio
     (Output : access Test_Output_Port'Class) return Boolean is
      Original    : constant SI_Bytes_Type := From_MB (100);
      Compressed  : constant SI_Bytes_Type := From_MB (30);

--  Calculate compression ratio
      Ratio : constant Compression_Ratio_Type :=
         Calculate_Compression_Ratio (Original, Compressed);

--  Calculate percentage saved
      Saved : constant Percentage_Type :=
         Compression_Percentage_Saved (Ratio);

      Expected_Ratio : constant Float := 0.3;  -- 30/100
      Expected_Saved : constant Float := 70.0;  -- 70% saved
   begin
      Assert (abs (Float (Ratio) - Expected_Ratio) < 0.01,
              "Compression ratio calculation failed");
      Assert (abs (Float (Saved) - Expected_Saved) < 0.01,
              "Compression percentage saved calculation failed");
      return True;
   end Test_Integration_With_Compression_Ratio;

--   Test suite
   Suite : constant Test_Suite := Create_Suite ("Performance Helper Functions");

begin
--  MB/s calculation tests
   Add_Test (Suite, "Calculate_MB_Per_Second with SI_Bytes_Type",
             Test_MB_Per_Second_SI_Bytes'Access);
   Add_Test (Suite, "Calculate_MB_Per_Second with Long_Long_Integer",
             Test_MB_Per_Second_Long_Long_Integer'Access);
   Add_Test (Suite, "MB/s with zero bytes", Test_MB_Per_Second_Zero_Bytes'Access);
   Add_Test (Suite, "MB/s with large values", Test_MB_Per_Second_Large_Values'Access);
   Add_Test (Suite, "MB/s with small duration", Test_MB_Per_Second_Small_Duration'Access);
   Add_Test (Suite, "MB/s with very small values", Test_MB_Per_Second_Very_Small_Values'Access);

--  Percentage calculation tests
   Add_Test (Suite, "Calculate_Percentage with SI_Bytes_Type",
             Test_Percentage_SI_Bytes'Access);
--   Natural percentage tests moved to Math module
   Add_Test (Suite, "Percentage with zero part", Test_Percentage_Zero_Part'Access);
   Add_Test (Suite, "Percentage with equal part and whole",
             Test_Percentage_Equal_Part_Whole'Access);
--   Generic percentage tests moved to Math module

--  Integration tests
   Add_Test (Suite, "Integration with compression ratio",
             Test_Integration_With_Compression_Ratio'Access);

--  Run tests
   Run_Suite (Suite, Output'Access);
end Test_Performance_Helpers;

pragma Warnings (On, "subprogram body has no previous spec");
