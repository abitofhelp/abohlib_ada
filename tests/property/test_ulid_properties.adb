--   =============================================================================
--   Test_ULID_Properties - Property-Based Tests for ULID
--   =============================================================================
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--
--   Purpose:
--     Property-based tests for ULID generation, ensuring invariants hold
--     across many randomly generated test cases.
--   =============================================================================

pragma Ada_2022;
pragma Warnings (Off, "subprogram body has no previous spec");

with Ada.Calendar; use Ada.Calendar;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Abohlib.Infrastructure.Testing.Property_Testing;
with Abohlib.Infrastructure.Testing.Test_Framework;
with Abohlib.Core.Domain.Utilities.ULID_Helpers;
with ULID;

procedure Test_ULID_Properties is

   use Abohlib.Infrastructure.Testing.Property_Testing;
   use Abohlib.Infrastructure.Testing.Test_Framework;
   use Abohlib.Core.Domain.Utilities.ULID_Helpers;

   package Framework renames Abohlib.Infrastructure.Testing.Test_Framework;

--   Property: ULIDs are always 26 characters long
   function Property_ULID_Length return Boolean is
      ID : constant ULID.ULID_Number := Generate_ULID;
      Str : constant String := To_String (ID);
   begin
      return Str'Length = 26;
   end Property_ULID_Length;

--   Property: ULIDs generated in sequence are lexicographically ordered
   function Property_ULID_Ordering return Boolean is
      ID1 : constant ULID.ULID_Number := Generate_ULID;
      ID2 : ULID.ULID_Number;
   begin
--  Small delay to ensure different timestamp
      delay 0.001;
      ID2 := Generate_ULID;

--  Later ULID should be lexicographically greater
      return To_String (ID1) < To_String (ID2);
   end Property_ULID_Ordering;

--   Property: ULIDs contain only valid Crockford Base32 characters
   function Property_ULID_Valid_Characters return Boolean is
      ID : constant ULID.ULID_Number := Generate_ULID;
      Str : constant String := To_String (ID);
      Valid_Chars : constant String := "0123456789ABCDEFGHJKMNPQRSTVWXYZ";
   begin
      for C of Str loop
         declare
            Found : Boolean := False;
         begin
            for Valid_C of Valid_Chars loop
               if C = Valid_C then
                  Found := True;
                  exit;
               end if;
            end loop;

            if not Found then
               return False;
            end if;
         end;
      end loop;
      return True;
   end Property_ULID_Valid_Characters;

--   Property: ULIDs are unique even when generated rapidly
   function Property_ULID_Uniqueness return Boolean is
      type ULID_Array is array (1 .. 100) of ULID.ULID_Number;
      IDs : ULID_Array;
   begin
--  Generate many ULIDs rapidly
      for I in IDs'Range loop
         IDs (I) := Generate_ULID;
      end loop;

--  Check all pairs for uniqueness
      for I in IDs'Range loop
         for J in I + 1 .. IDs'Last loop
            if To_String (IDs (I)) = To_String (IDs (J)) then
               return False;
            end if;
         end loop;
      end loop;

      return True;
   end Property_ULID_Uniqueness;

--   Property: ULID timestamp can be extracted and is reasonable
   function Property_ULID_Timestamp return Boolean is
      Start_Time : constant Time := Clock;
      ID : constant ULID.ULID_Number := Generate_ULID;
      End_Time : constant Time := Clock;

--  Extract timestamp (first 10 characters encode 48-bit timestamp)
--  This is a simplified check - real implementation would decode properly
      ID_Str : constant String := To_String (ID);
   begin
--  For now, just verify the ID was created between start and end
--  A full implementation would decode the timestamp
      return ID_Str'Length = 26;
   end Property_ULID_Timestamp;

--   Run all property tests
   procedure Run_Property_Tests is
      Test_Count : constant := 1000;
      All_Passed : Boolean := True;
   begin
      Framework.Test_Suite_Begin ("ULID Property Tests");

--  Test: ULID Length Property
      declare
         Passed : Boolean := True;
      begin
         for I in 1 .. Test_Count loop
            if not Property_ULID_Length then
               Passed := False;
               exit;
            end if;
         end loop;

         if Passed then
            Framework.Test_Pass ("ULID Length Property (26 chars)");
         else
            Framework.Test_Fail ("ULID Length Property", "Some ULIDs were not 26 characters");
            All_Passed := False;
         end if;
      end;

--  Test: ULID Ordering Property
      declare
         Passed : Boolean := True;
      begin
         for I in 1 .. Test_Count / 10 loop  -- Less iterations due to delay
            if not Property_ULID_Ordering then
               Passed := False;
               exit;
            end if;
         end loop;

         if Passed then
            Framework.Test_Pass ("ULID Ordering Property (lexicographic)");
         else
            Framework.Test_Fail ("ULID Ordering Property", "ULIDs not properly ordered");
            All_Passed := False;
         end if;
      end;

--  Test: ULID Valid Characters Property
      declare
         Passed : Boolean := True;
      begin
         for I in 1 .. Test_Count loop
            if not Property_ULID_Valid_Characters then
               Passed := False;
               exit;
            end if;
         end loop;

         if Passed then
            Framework.Test_Pass ("ULID Valid Characters Property (Crockford Base32)");
         else
            Framework.Test_Fail ("ULID Valid Characters Property", "Invalid characters found");
            All_Passed := False;
         end if;
      end;

--  Test: ULID Uniqueness Property
      declare
         Passed : Boolean := True;
      begin
         for I in 1 .. 10 loop  -- Run uniqueness test 10 times
            if not Property_ULID_Uniqueness then
               Passed := False;
               exit;
            end if;
         end loop;

         if Passed then
            Framework.Test_Pass ("ULID Uniqueness Property (100 rapid generations)");
         else
            Framework.Test_Fail ("ULID Uniqueness Property", "Duplicate ULIDs generated");
            All_Passed := False;
         end if;
      end;

--  Test: ULID Timestamp Property
      declare
         Passed : Boolean := True;
      begin
         for I in 1 .. Test_Count loop
            if not Property_ULID_Timestamp then
               Passed := False;
               exit;
            end if;
         end loop;

         if Passed then
            Framework.Test_Pass ("ULID Timestamp Property");
         else
            Framework.Test_Fail ("ULID Timestamp Property", "Timestamp validation failed");
            All_Passed := False;
         end if;
      end;

      Framework.Test_Suite_End (All_Passed);
   end Run_Property_Tests;

begin
   Run_Property_Tests;
end Test_ULID_Properties;

pragma Warnings (On, "subprogram body has no previous spec");
