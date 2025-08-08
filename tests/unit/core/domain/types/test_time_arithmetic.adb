--   =============================================================================
--   Test_Time_Arithmetic - Time Types Arithmetic Operations Unit Tests
--   =============================================================================
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--   =============================================================================

pragma Ada_2022;
pragma Warnings (Off, "subprogram body has no previous spec");

with Abohlib.Core.Domain.Types.Time;

package body Test_Time_Arithmetic is

   use Abohlib.Core.Domain.Types.Time;

--   ==========================================================================
--   Test Cases for Milliseconds_Type
--   ==========================================================================

   function Test_Milliseconds_Addition
     (Output : access Test_Output_Port'Class) return Boolean is
      Time1  : constant Milliseconds_Type := 1000;
      Time2  : constant Milliseconds_Type := 500;
      Result : constant Milliseconds_Type := Time1 + Time2;
   begin
      if Result /= 1500 then
         Output.Write_Line ("FAIL: Milliseconds addition - Expected 1500, got" &
                           Milliseconds_Type'Image (Result));
         return False;
      end if;
      return True;
   end Test_Milliseconds_Addition;

   function Test_Milliseconds_Multiplication
     (Output : access Test_Output_Port'Class) return Boolean is
      Time   : constant Milliseconds_Type := 100;
      Factor : constant Natural := 5;
      Result : constant Milliseconds_Type := Time * Factor;
   begin
      if Result /= 500 then
         Output.Write_Line ("FAIL: Milliseconds multiplication - Expected 500, got" &
                           Milliseconds_Type'Image (Result));
         return False;
      end if;
      return True;
   end Test_Milliseconds_Multiplication;

--   ==========================================================================
--   Test Cases for Timeout_Ms_Type
--   ==========================================================================

   function Test_Timeout_Addition
     (Output : access Test_Output_Port'Class) return Boolean is
      Timeout1 : constant Timeout_Ms_Type := 5000;
      Timeout2 : constant Timeout_Ms_Type := 2000;
      Result   : constant Timeout_Ms_Type := Timeout1 + Timeout2;
   begin
      if Result /= 7000 then
         Output.Write_Line ("FAIL: Timeout addition - Expected 7000, got" &
                           Timeout_Ms_Type'Image (Result));
         return False;
      end if;
      return True;
   end Test_Timeout_Addition;

   function Test_Timeout_With_Delay
     (Output : access Test_Output_Port'Class) return Boolean is
      Timeout : constant Timeout_Ms_Type := 5000;
      Delay_Time : constant Delay_Ms_Type := 1000;
      Result_Add : constant Timeout_Ms_Type := Timeout + Delay_Time;
      Result_Sub : constant Timeout_Ms_Type := Timeout - Delay_Time;
   begin
      if Result_Add /= 6000 then
         Output.Write_Line ("FAIL: Timeout + Delay - Expected 6000, got" &
                           Timeout_Ms_Type'Image (Result_Add));
         return False;
      end if;

      if Result_Sub /= 4000 then
         Output.Write_Line ("FAIL: Timeout - Delay - Expected 4000, got" &
                           Timeout_Ms_Type'Image (Result_Sub));
         return False;
      end if;
      return True;
   end Test_Timeout_With_Delay;

   function Test_Timeout_Comparison
     (Output : access Test_Output_Port'Class) return Boolean is
      Short : constant Timeout_Ms_Type := 1000;
      Long  : constant Timeout_Ms_Type := 5000;
   begin
      if not (Short < Long) then
         Output.Write_Line ("FAIL: Timeout comparison - Short should be < Long");
         return False;
      end if;

      if not (Long > Short) then
         Output.Write_Line ("FAIL: Timeout comparison - Long should be > Short");
         return False;
      end if;
      return True;
   end Test_Timeout_Comparison;

--   ==========================================================================
--   Test Cases for Delay_Ms_Type
--   ==========================================================================

   function Test_Delay_Addition
     (Output : access Test_Output_Port'Class) return Boolean is
      Delay1 : constant Delay_Ms_Type := 100;
      Delay2 : constant Delay_Ms_Type := 200;
      Result : constant Delay_Ms_Type := Delay1 + Delay2;
   begin
      if Result /= 300 then
         Output.Write_Line ("FAIL: Delay addition - Expected 300, got" &
                           Delay_Ms_Type'Image (Result));
         return False;
      end if;
      return True;
   end Test_Delay_Addition;

   function Test_Delay_Multiplication
     (Output : access Test_Output_Port'Class) return Boolean is
      Delay_Time : constant Delay_Ms_Type := 50;
      Factor     : constant Natural := 4;
      Result     : constant Delay_Ms_Type := Delay_Time * Factor;
   begin
      if Result /= 200 then
         Output.Write_Line ("FAIL: Delay multiplication - Expected 200, got" &
                           Delay_Ms_Type'Image (Result));
         return False;
      end if;
      return True;
   end Test_Delay_Multiplication;

   function Test_Delay_Comparison
     (Output : access Test_Output_Port'Class) return Boolean is
      Quick : constant Delay_Ms_Type := 50;
      Slow  : constant Delay_Ms_Type := 500;
   begin
      if not (Quick < Slow) then
         Output.Write_Line ("FAIL: Delay comparison - Quick should be < Slow");
         return False;
      end if;

      if not (Slow > Quick) then
         Output.Write_Line ("FAIL: Delay comparison - Slow should be > Quick");
         return False;
      end if;
      return True;
   end Test_Delay_Comparison;

--   ==========================================================================
--   Test Cases for Retry_Delay_Ms_Type
--   ==========================================================================

   function Test_Retry_Delay_Addition
     (Output : access Test_Output_Port'Class) return Boolean is
      Retry1 : constant Retry_Delay_Ms_Type := 1000;
      Retry2 : constant Retry_Delay_Ms_Type := 2000;
      Result : constant Retry_Delay_Ms_Type := Retry1 + Retry2;
   begin
      if Result /= 3000 then
         Output.Write_Line ("FAIL: Retry_Delay addition - Expected 3000, got" &
                           Retry_Delay_Ms_Type'Image (Result));
         return False;
      end if;
      return True;
   end Test_Retry_Delay_Addition;

   function Test_Retry_Delay_Multiplication
     (Output : access Test_Output_Port'Class) return Boolean is
      Base_Delay : constant Retry_Delay_Ms_Type := 100;
      Multiplier : constant Multiplier_Type := 2.5;
      Result     : constant Retry_Delay_Ms_Type := Base_Delay * Multiplier;
   begin
      if Result /= 250 then
         Output.Write_Line ("FAIL: Retry_Delay multiplication - Expected 250, got" &
                           Retry_Delay_Ms_Type'Image (Result));
         return False;
      end if;
      return True;
   end Test_Retry_Delay_Multiplication;

--   ==========================================================================
--   Test Cases for Seconds_Type, Minutes_Type, Hours_Type
--   ==========================================================================

   function Test_Seconds_Addition
     (Output : access Test_Output_Port'Class) return Boolean is
      Sec1   : constant Seconds_Type := 1.5;
      Sec2   : constant Seconds_Type := 2.5;
      Result : constant Seconds_Type := Sec1 + Sec2;
   begin
      if Result /= 4.0 then
         Output.Write_Line ("FAIL: Seconds addition - Expected 4.0, got" &
                           Seconds_Type'Image (Result));
         return False;
      end if;
      return True;
   end Test_Seconds_Addition;

   function Test_Seconds_Multiplication
     (Output : access Test_Output_Port'Class) return Boolean is
      Sec    : constant Seconds_Type := 2.0;
      Factor : constant Natural := 3;
      Result : constant Seconds_Type := Sec * Factor;
   begin
      if Result /= 6.0 then
         Output.Write_Line ("FAIL: Seconds multiplication - Expected 6.0, got" &
                           Seconds_Type'Image (Result));
         return False;
      end if;
      return True;
   end Test_Seconds_Multiplication;

--   ==========================================================================
--   Test Cases for File_Position_Type
--   ==========================================================================

   function Test_File_Position_Addition
     (Output : access Test_Output_Port'Class) return Boolean is
      Pos1   : constant File_Position_Type := 1024;
      Pos2   : constant File_Position_Type := 512;
      Offset : constant Natural := 256;
      Result1 : constant File_Position_Type := Pos1 + Pos2;
      Result2 : constant File_Position_Type := Pos1 + Offset;
   begin
      if Result1 /= 1536 then
         Output.Write_Line ("FAIL: File_Position + File_Position - Expected 1536, got" &
                           File_Position_Type'Image (Result1));
         return False;
      end if;

      if Result2 /= 1280 then
         Output.Write_Line ("FAIL: File_Position + Natural - Expected 1280, got" &
                           File_Position_Type'Image (Result2));
         return False;
      end if;
      return True;
   end Test_File_Position_Addition;

   function Test_File_Position_Comparison
     (Output : access Test_Output_Port'Class) return Boolean is
      Start  : constant File_Position_Type := 0;
      Middle : constant File_Position_Type := 1024;
      End_Pos : constant File_Position_Type := 2048;
   begin
      if not (Start < Middle) then
         Output.Write_Line ("FAIL: File_Position comparison - Start should be < Middle");
         return False;
      end if;

      if not (End_Pos > Middle) then
         Output.Write_Line ("FAIL: File_Position comparison - End should be > Middle");
         return False;
      end if;

      if not (Start <= Middle) then
         Output.Write_Line ("FAIL: File_Position comparison - Start should be <= Middle");
         return False;
      end if;

      if not (End_Pos >= Middle) then
         Output.Write_Line ("FAIL: File_Position comparison - End should be >= Middle");
         return False;
      end if;
      return True;
   end Test_File_Position_Comparison;

--   ==========================================================================
--   Test Cases for Timestamp_Type
--   ==========================================================================

   function Test_Timestamp_Addition
     (Output : access Test_Output_Port'Class) return Boolean is
      Base_Time : constant Timestamp_Type := 1609459200000; -- Jan 1, 2021 in ms
      Offset    : constant Milliseconds_Type := 3600000;    -- 1 hour in ms
      Result    : constant Timestamp_Type := Base_Time + Offset;
   begin
      if Result /= 1609462800000 then
         Output.Write_Line ("FAIL: Timestamp + Milliseconds - Expected 1609462800000, got" &
                           Timestamp_Type'Image (Result));
         return False;
      end if;
      return True;
   end Test_Timestamp_Addition;

   function Test_Timestamp_Comparison
     (Output : access Test_Output_Port'Class) return Boolean is
      Earlier : constant Timestamp_Type := 1609459200000;
      Later   : constant Timestamp_Type := 1609462800000;
   begin
      if not (Earlier < Later) then
         Output.Write_Line ("FAIL: Timestamp comparison - Earlier should be < Later");
         return False;
      end if;

      if not (Later > Earlier) then
         Output.Write_Line ("FAIL: Timestamp comparison - Later should be > Earlier");
         return False;
      end if;
      return True;
   end Test_Timestamp_Comparison;

--   ==========================================================================
--   Test Runner
--   ==========================================================================

   function Run_All_Tests
     (Output : access Test_Output_Port'Class) return Test_Stats_Result.Result is
      Stats : Test_Stats := (0, 0, 0);
   begin
      Output.Write_Line ("=== Time Arithmetic Operations Tests ===");

--   Milliseconds_Type tests
      Run_Test ("Milliseconds Addition", Test_Milliseconds_Addition'Access, Output, Stats);
      Run_Test ("Milliseconds Multiplication", Test_Milliseconds_Multiplication'Access, Output, Stats);

--   Timeout_Ms_Type tests
      Run_Test ("Timeout Addition", Test_Timeout_Addition'Access, Output, Stats);
      Run_Test ("Timeout with Delay", Test_Timeout_With_Delay'Access, Output, Stats);
      Run_Test ("Timeout Comparison", Test_Timeout_Comparison'Access, Output, Stats);

--   Delay_Ms_Type tests
      Run_Test ("Delay Addition", Test_Delay_Addition'Access, Output, Stats);
      Run_Test ("Delay Multiplication", Test_Delay_Multiplication'Access, Output, Stats);
      Run_Test ("Delay Comparison", Test_Delay_Comparison'Access, Output, Stats);

--   Retry_Delay_Ms_Type tests
      Run_Test ("Retry_Delay Addition", Test_Retry_Delay_Addition'Access, Output, Stats);
      Run_Test ("Retry_Delay Multiplication", Test_Retry_Delay_Multiplication'Access, Output, Stats);

--   Duration types tests
      Run_Test ("Seconds Addition", Test_Seconds_Addition'Access, Output, Stats);
      Run_Test ("Seconds Multiplication", Test_Seconds_Multiplication'Access, Output, Stats);

--   File_Position_Type tests
      Run_Test ("File_Position Addition", Test_File_Position_Addition'Access, Output, Stats);
      Run_Test ("File_Position Comparison", Test_File_Position_Comparison'Access, Output, Stats);

--   Timestamp_Type tests
      Run_Test ("Timestamp Addition", Test_Timestamp_Addition'Access, Output, Stats);
      Run_Test ("Timestamp Comparison", Test_Timestamp_Comparison'Access, Output, Stats);

      Print_Test_Summary ("Time Arithmetic Tests", Stats, Output);

      return Test_Stats_Result.Ok (Stats);
   end Run_All_Tests;

end Test_Time_Arithmetic;

pragma Warnings (On, "subprogram body has no previous spec");
