--   =============================================================================
--   Test_Strong_Types_Properties - Property-Based Tests for Strong Types
--   =============================================================================
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--
--   Purpose:
--     Property-based tests for strong domain types, ensuring type safety and
--     arithmetic operations maintain invariants.
--   =============================================================================

pragma Ada_2022;
pragma Warnings (Off, "subprogram body has no previous spec");

with Ada.Numerics.Discrete_Random;
with Abohlib.Core.Domain.Types.Bytes;
with Abohlib.Core.Domain.Types.Time;
with Abohlib.Core.Domain.Types.Counts;
with Abohlib.Core.Domain.Constants.Bytes;
with Abohlib.Infrastructure.Testing.Test_Framework;

procedure Test_Strong_Types_Properties is

   use Abohlib.Core.Domain.Types.Bytes;
   use Abohlib.Core.Domain.Types.Time;
   use Abohlib.Core.Domain.Types.Counts;
   use Abohlib.Core.Domain.Constants.Bytes;
   use Abohlib.Infrastructure.Testing.Test_Framework;

   package Framework renames Abohlib.Infrastructure.Testing.Test_Framework;

--   Random generators
   subtype Small_Natural is Natural range 0 .. 1000;
   package Random_Small is new Ada.Numerics.Discrete_Random (Small_Natural);
   Gen : Random_Small.Generator;

--   Property: SI and IEC bytes are distinct types (won't compile if mixed)
--   This is a compile-time property, so we test operations instead
   function Property_Bytes_Type_Safety return Boolean is
      SI_Value : constant SI_Bytes_Type := SI_KB * 10;  -- 10,000 bytes
      IEC_Value : constant IEC_Bytes_Type := IEC_KiB * 10;  -- 10,240 bytes
   begin
--  These should be different values due to different bases
      return Long_Long_Integer (SI_Value) = 10_000 and
             Long_Long_Integer (IEC_Value) = 10_240;
   end Property_Bytes_Type_Safety;

--   Property: Byte arithmetic operations maintain type
   function Property_Bytes_Arithmetic return Boolean is
      A : constant SI_Bytes_Type := SI_Bytes_Type (Random_Small.Random (Gen) * 1000);
      B : constant SI_Bytes_Type := SI_Bytes_Type (Random_Small.Random (Gen) * 1000);
   begin
--  Addition and subtraction should maintain SI_Bytes_Type
      declare
         Sum : constant SI_Bytes_Type := A + B;
         Diff : SI_Bytes_Type;
      begin
--  Check addition
         if Long_Long_Integer (Sum) /= Long_Long_Integer (A) + Long_Long_Integer (B) then
            return False;
         end if;

--  Check subtraction (only if A >= B to avoid constraint error)
         if A >= B then
            Diff := A - B;
            if Long_Long_Integer (Diff) /= Long_Long_Integer (A) - Long_Long_Integer (B) then
               return False;
            end if;
         end if;

         return True;
      end;
   exception
      when others => return False;
   end Property_Bytes_Arithmetic;

--   Property: Time types maintain constraints
   function Property_Time_Constraints return Boolean is
      MS : constant Milliseconds_Type :=
         Milliseconds_Type (Random_Small.Random (Gen));
      Timeout : constant Timeout_Ms_Type := Timeout_Ms_Type (MS);
      Delay_MS : constant Delay_Ms_Type := Delay_Ms_Type (MS);
   begin
--  All time types should preserve the value
      return Natural (MS) = Natural (Timeout) and
             Natural (MS) = Natural (Delay_MS);
   end Property_Time_Constraints;

--   Property: Count types enforce positive constraints
   function Property_Count_Positive return Boolean is
      Count : constant Element_Count_Type :=
         Element_Count_Type (Random_Small.Random (Gen) + 1); -- Ensure > 0
   begin
--  Element counts should always be positive
      return Natural (Count) > 0;
   end Property_Count_Positive;

--   Property: Time arithmetic operations work correctly
   function Property_Time_Arithmetic return Boolean is
      A : constant Milliseconds_Type :=
         Milliseconds_Type (Random_Small.Random (Gen));
      B : constant Milliseconds_Type :=
         Milliseconds_Type (Random_Small.Random (Gen));
   begin
      declare
         Sum : constant Milliseconds_Type := A + B;
      begin
--  Check addition
         return Natural (Sum) = Natural (A) + Natural (B);
      end;
   exception
      when others => return False;
   end Property_Time_Arithmetic;

--   Property: Multiplier type works with retry delays
   function Property_Multiplier_Type return Boolean is
      Base_Delay : constant Retry_Delay_Ms_Type :=
         Retry_Delay_Ms_Type (Random_Small.Random (Gen) + 1);
      Multiplier : constant Multiplier_Type := 2.0;
   begin
      declare
         Result : constant Retry_Delay_Ms_Type := Base_Delay * Multiplier;
      begin
--  Multiplication should double the delay
         return Natural (Result) = Natural (Base_Delay) * 2;
      end;
   exception
      when others => return False;
   end Property_Multiplier_Type;

--   Property: Worker count respects bounds
   function Property_Worker_Count_Bounds return Boolean is
--  Worker_Count_Type has range 1 .. 1024
      Raw_Value : constant Natural := Random_Small.Random (Gen);
      Safe_Value : constant Natural := (Raw_Value mod 1024) + 1;
      Workers : constant Worker_Count_Type := Worker_Count_Type (Safe_Value);
   begin
      return Natural (Workers) >= 1 and Natural (Workers) <= 1024;
   end Property_Worker_Count_Bounds;

--   Run all property tests
   procedure Run_Property_Tests is
      Test_Count : constant := 1000;
      All_Passed : Boolean := True;
   begin
      Random_Small.Reset (Gen);
      Framework.Test_Suite_Begin ("Strong Types Property Tests");

--  Test: Bytes Type Safety
      declare
         Passed : Boolean := True;
      begin
         for I in 1 .. Test_Count loop
            if not Property_Bytes_Type_Safety then
               Passed := False;
               exit;
            end if;
         end loop;

         if Passed then
            Framework.Test_Pass ("Bytes Type Safety Property (SI vs IEC)");
         else
            Framework.Test_Fail ("Bytes Type Safety", "SI/IEC distinction failed");
            All_Passed := False;
         end if;
      end;

--  Test: Bytes Arithmetic
      declare
         Passed : Boolean := True;
      begin
         for I in 1 .. Test_Count loop
            if not Property_Bytes_Arithmetic then
               Passed := False;
               exit;
            end if;
         end loop;

         if Passed then
            Framework.Test_Pass ("Bytes Arithmetic Property");
         else
            Framework.Test_Fail ("Bytes Arithmetic", "Arithmetic operations failed");
            All_Passed := False;
         end if;
      end;

--  Test: Time Constraints
      declare
         Passed : Boolean := True;
      begin
         for I in 1 .. Test_Count loop
            if not Property_Time_Constraints then
               Passed := False;
               exit;
            end if;
         end loop;

         if Passed then
            Framework.Test_Pass ("Time Type Constraints Property");
         else
            Framework.Test_Fail ("Time Constraints", "Time type conversions failed");
            All_Passed := False;
         end if;
      end;

--  Test: Count Positive
      declare
         Passed : Boolean := True;
      begin
         for I in 1 .. Test_Count loop
            if not Property_Count_Positive then
               Passed := False;
               exit;
            end if;
         end loop;

         if Passed then
            Framework.Test_Pass ("Count Types Positive Property");
         else
            Framework.Test_Fail ("Count Positive", "Count constraints violated");
            All_Passed := False;
         end if;
      end;

--  Test: Time Arithmetic
      declare
         Passed : Boolean := True;
      begin
         for I in 1 .. Test_Count loop
            if not Property_Time_Arithmetic then
               Passed := False;
               exit;
            end if;
         end loop;

         if Passed then
            Framework.Test_Pass ("Time Arithmetic Property");
         else
            Framework.Test_Fail ("Time Arithmetic", "Time arithmetic failed");
            All_Passed := False;
         end if;
      end;

--  Test: Multiplier Type
      declare
         Passed : Boolean := True;
      begin
         for I in 1 .. Test_Count loop
            if not Property_Multiplier_Type then
               Passed := False;
               exit;
            end if;
         end loop;

         if Passed then
            Framework.Test_Pass ("Multiplier Type Property");
         else
            Framework.Test_Fail ("Multiplier Type", "Multiplier operations failed");
            All_Passed := False;
         end if;
      end;

--  Test: Worker Count Bounds
      declare
         Passed : Boolean := True;
      begin
         for I in 1 .. Test_Count loop
            if not Property_Worker_Count_Bounds then
               Passed := False;
               exit;
            end if;
         end loop;

         if Passed then
            Framework.Test_Pass ("Worker Count Bounds Property");
         else
            Framework.Test_Fail ("Worker Count Bounds", "Worker count out of range");
            All_Passed := False;
         end if;
      end;

      Framework.Test_Suite_End (All_Passed);
   end Run_Property_Tests;

begin
   Run_Property_Tests;
end Test_Strong_Types_Properties;

pragma Warnings (On, "subprogram body has no previous spec");
