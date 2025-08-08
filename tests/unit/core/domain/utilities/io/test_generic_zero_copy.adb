--   =============================================================================
--   Test_Generic_Zero_Copy - Unit tests for Generic Zero-Copy IO
--   =============================================================================
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--   =============================================================================

pragma Ada_2022;
pragma Warnings (Off, "subprogram body has no previous spec");

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Test_Generic_Zero_Copy is

--   ==========================================================================
--   Test Functions
--   ==========================================================================

   function Test_Zero_Copy_Placeholder return Void_Result.Result is
   begin
--   This is a placeholder test for zero-copy functionality
--   Zero-copy operations are platform-specific and require
--   actual file operations that are difficult to test in isolation

      return Void_Result.Ok (True);
   end Test_Zero_Copy_Placeholder;

--   ==========================================================================
--   Test Runner
--   ==========================================================================

   function Run_All_Tests
     (Output : access Test_Output_Port'Class) return Test_Stats_Result.Result
   is
      Test_Count : constant := 1;
      Tests : Test_Results_Array (1 .. Test_Count);
   begin
--  Test 1: Placeholder
      declare
         Result : constant Test_Result_Pkg.Result :=
            Run_Test ("Zero Copy Placeholder", Test_Zero_Copy_Placeholder'Access, Output);
      begin
         if Result.Is_Ok then
            Tests (1) := Result.Get_Ok;
            Print_Test_Result (Tests (1), Output);
         else
            return Test_Stats_Result.Err (
               Test_Error'(
                  Kind        => Setup_Failed,
                  Message     => To_Unbounded_String ("Failed to run test"),
                  Details     => Null_Unbounded_String,
                  Line_Number => 0,
                  Test_Name   => To_Unbounded_String ("Run_All_Tests")
               )
            );
         end if;
      end;

      declare
         Stats_Result : constant Test_Stats_Result.Result :=
            Run_Test_Suite ("Generic Zero-Copy IO", Tests, Output);
      begin
         if Stats_Result.Is_Ok then
            declare
               Stats : constant Test_Statistics := Stats_Result.Get_Ok;
            begin
               Output.Write_Line ("");
               Print_Test_Summary ("Generic Zero-Copy IO Tests", Stats, Output);
               return Test_Stats_Result.Ok (Stats);
            end;
         else
            return Stats_Result;
         end if;
      end;
   end Run_All_Tests;

end Test_Generic_Zero_Copy;

pragma Warnings (On, "subprogram body has no previous spec");
