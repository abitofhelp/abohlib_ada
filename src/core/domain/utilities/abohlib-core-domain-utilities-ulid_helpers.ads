--  =============================================================================
--  Abohlib.Core.Domain.Utilities.ULID_Helpers - ULID Utility Functions
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Purpose:
--    Provides reusable utility functions for working with ULIDs (Universally
--    Unique Lexicographically Sortable Identifiers) across projects.
--
--  Features:
--    - Timestamp extraction from ULIDs
--    - ULID validation
--    - Zero/Null ULID constant
--    - Thread-safe ULID generation with singleton generator
--    - ULID age calculation
--  =============================================================================

pragma Ada_2022;

with Ada.Calendar;
with Ada.Strings.Unbounded;
with ULID;
with Abohlib.Core.Domain.Result;

package Abohlib.Core.Domain.Utilities.ULID_Helpers is

   use Ada.Strings.Unbounded;

   --  Result types for safe operations
   package Time_Result is new
     Abohlib.Core.Domain.Result.Result_Package
       (Ok_Type  => Ada.Calendar.Time,
        Err_Type => Unbounded_String);

   package Duration_Result is new
     Abohlib.Core.Domain.Result.Result_Package
       (Ok_Type  => Duration,
        Err_Type => Unbounded_String);

   package Boolean_Result is new
     Abohlib.Core.Domain.Result.Result_Package
       (Ok_Type  => Boolean,
        Err_Type => Unbounded_String);

   package ULID_Result is new
     Abohlib.Core.Domain.Result.Result_Package
       (Ok_Type  => ULID.ULID_Number,
        Err_Type => Unbounded_String);

   --  Constants
   Null_ULID : constant ULID.ULID_Number := 0;
   ULID_String_Length : constant := 26;

   --  Extract timestamp from a ULID (returns Unix epoch if extraction fails)
   function Extract_Timestamp (U : ULID.ULID_Number) return Ada.Calendar.Time
   with Inline;

   --  Calculate age of a ULID (time since creation)
   function Get_Age (U : ULID.ULID_Number) return Duration
   with Post => Get_Age'Result >= 0.0;

   --  Check if a string is a valid ULID format
   function Is_Valid_ULID_String (S : String) return Boolean
   with
     Post =>
       (if S'Length = 0 then not Is_Valid_ULID_String'Result)
       and then (if S'Length /= ULID_String_Length
                 then not Is_Valid_ULID_String'Result)
       and then Is_Valid_ULID_String'Result
                = (S'Length = ULID_String_Length
                   and then (for all C of S
                             => C in '0' .. '9' | 'A' .. 'Z' | 'a' .. 'z'));

   --  Check if ULID is null/zero
   function Is_Null (U : ULID.ULID_Number) return Boolean
   with Inline;

   --  Protected singleton for thread-safe ULID generation
   protected ULID_Generator is
      --  Generate a new ULID
      procedure Generate (Result : out ULID.ULID_Number)
      with Post => not Is_Null (Result);

      --  Generate a new ULID as string
      procedure Generate_String (Result : out String; Last : out Natural)
      with
        Pre => Result'Length >= ULID_String_Length,
        Post =>
          Last = Result'First + ULID_String_Length - 1
          and then Is_Valid_ULID_String (Result (Result'First .. Last));

      --  Reset the generator (randomize)
      procedure Reset
      with Post => True;

      --  Reset with specific seed
      procedure Reset (Seed : Integer)
      with Post => True;

   private
      Generator : ULID.Random_Generator;
      Initialized : Boolean := False;
   end ULID_Generator;

   --  Safe versions returning Result types
   function Extract_Timestamp_Safe
     (U : ULID.ULID_Number) return Time_Result.Result;
   function Get_Age_Safe (U : ULID.ULID_Number) return Duration_Result.Result;
   function Is_Valid_ULID_String_Safe
     (S : String) return Boolean_Result.Result;
   function New_ULID_Safe return ULID_Result.Result;

   --  Convenience functions using the singleton generator
   function New_ULID return ULID.ULID_Number
   with Post => not Is_Null (New_ULID'Result);
   function New_ULID_String return String
   with
     Post =>
       New_ULID_String'Result'Length = ULID_String_Length
       and then Is_Valid_ULID_String (New_ULID_String'Result);

end Abohlib.Core.Domain.Utilities.ULID_Helpers;
