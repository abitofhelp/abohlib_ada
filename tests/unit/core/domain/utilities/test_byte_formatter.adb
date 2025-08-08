--   =============================================================================
--   Test_Byte_Formatter - Unit test implementation
--   =============================================================================
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--   =============================================================================

pragma Ada_2022;
pragma Warnings (Off, "subprogram body has no previous spec");

with Abohlib.Core.Domain.Utilities.Byte_Formatter;
use Abohlib.Core.Domain.Utilities.Byte_Formatter;
--   with Abohlib.Core.Domain.Constants.Bytes;
--   use Abohlib.Core.Domain.Constants.Bytes;
with Abohlib.Core.Domain.Types.Bytes;
use Abohlib.Core.Domain.Types.Bytes;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Test_Byte_Formatter is

   ----------------------
--  Test_Zero_Bytes --
   ----------------------

   function Test_Zero_Bytes return Void_Result.Result is
   begin
      if Format_Bytes (SI_Bytes_Type (0)) /= "0 B" then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Zero bytes should format as '0 B'"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Zero_Bytes")
         ));
      end if;
      return Void_Result.Ok (True);
   end Test_Zero_Bytes;

   --------------------
--  Run_All_Tests --
   --------------------

   function Run_All_Tests
     (Output : access Test_Output_Port'Class) return Test_Stats_Result.Result
   is
      Test_Count : constant := 1;
      Tests : Test_Results_Array (1 .. Test_Count);
   begin
      Output.Write_Line ("=== Running Byte Formatter Tests ===");
      Output.Write_Line ("");

--  Test 1: Zero bytes
      declare
         Result : constant Test_Result_Pkg.Result :=
            Run_Test ("Zero bytes formatting", Test_Zero_Bytes'Access, Output);
      begin
         if Result.Is_Ok then
            Tests (1) := Result.Get_Ok;
            Print_Test_Result (Tests (1), Output);
         else
--  Handle error case
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

--  Generate summary
      declare
         Stats_Result : constant Test_Stats_Result.Result :=
            Run_Test_Suite ("Byte Formatter Tests", Tests, Output);
      begin
         if Stats_Result.Is_Ok then
            declare
               Stats : constant Test_Statistics := Stats_Result.Get_Ok;
            begin
               Output.Write_Line ("");
               Print_Test_Summary ("Byte Formatter Tests", Stats, Output);
               return Test_Stats_Result.Ok (Stats);
            end;
         else
            return Stats_Result;
         end if;
      end;
   end Run_All_Tests;

end Test_Byte_Formatter;

pragma Warnings (On, "subprogram body has no previous spec");
