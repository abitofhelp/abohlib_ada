--  =============================================================================
--  Abohlib.Core.Domain.Value_Objects.Type_Safe_Generic_Id - Implementation
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

with Ada.Strings.Fixed;
--  Removed unused: with Ada.Characters.Handling;
with Abohlib.Core.Domain.Utilities.ULID_Helpers;

package body Abohlib.Core.Domain.Value_Objects.Type_Safe_Generic_Id is

   use Abohlib.Core.Domain.Utilities;

   use Ada.Strings.Fixed;
   --  Removed unused: use Ada.Characters.Handling;

   --  ==========================================================================
   --  Generic ID Type Implementation
   --  ==========================================================================

   package body Generic_ID_Type is

      --  Removed unused: use ID_Result;  -- For Result type functions
      --  Removed unused: use type ID_Error_Kind;  -- For error kind literals

      --  ----------------------------------------------------------------------
      --  Constructor functions for Result_Type
      --  ----------------------------------------------------------------------

      function Ok_Result (Value : ID) return Result_Type is
      begin
         return (ID_Result.Ok (Value) with null record);
      end Ok_Result;

      function Err_Result (Error : ID_Error) return Result_Type is
      begin
         return (ID_Result.Err (Error) with null record);
      end Err_Result;

      overriding
      function Is_Ok (R : Result_Type) return Boolean is
      begin
         return ID_Result.Is_Ok (ID_Result.Result (R));
      end Is_Ok;

      overriding
      function Is_Err (R : Result_Type) return Boolean is
      begin
         return ID_Result.Is_Err (ID_Result.Result (R));
      end Is_Err;

      overriding
      function Get_Ok (R : Result_Type) return ID is
      begin
         return ID_Result.Get_Ok (ID_Result.Result (R));
      end Get_Ok;

      overriding
      function Get_Err (R : Result_Type) return ID_Error is
      begin
         return ID_Result.Get_Err (ID_Result.Result (R));
      end Get_Err;

      --  ----------------------------------------------------------------------
      --  Default ID
      --  ----------------------------------------------------------------------

      function Default_ID return ID is
      begin
         return
           (ULID_Value => ULID_Helpers.Null_ULID,
            Has_Prefix => (Prefix'Length > 0));
      end Default_ID;

      --  ----------------------------------------------------------------------
      --  Constructors
      --  ----------------------------------------------------------------------

      function New_ID return ID is
      begin
         return
           (ULID_Value => ULID_Helpers.New_ULID,
            Has_Prefix => (Prefix'Length > 0));
      end New_ID;

      function From_String (S : String) return Result_Type is
         Trimmed   : constant String := Trim (S, Ada.Strings.Both);
         ULID_Part : String (1 .. ULID_Helpers.ULID_String_Length);
         Start_Pos : Natural;
      begin
         --  Check for empty string
         if Trimmed'Length = 0 then
            declare
               Err_Val : constant ID_Error :=
                 (Kind      => Invalid_Format,
                  Message   => To_Bounded_String ("Empty ID string"),
                  ID_String => To_Bounded_String (S));
            begin
               return Err_Result (Err_Val);
            end;
         end if;

         --  Handle prefix if expected
         if Prefix'Length > 0 then
            if Trimmed'Length < Prefix'Length then
               declare
                  Err_Val : constant ID_Error :=
                    (Kind      => Invalid_Prefix,
                     Message   =>
                       To_Bounded_String ("ID too short for prefix"),
                     ID_String => To_Bounded_String (S));
               begin
                  return Err_Result (Err_Val);
               end;
            end if;

            if Trimmed (Trimmed'First .. Trimmed'First + Prefix'Length - 1)
              /= Prefix
            then
               declare
                  Err_Val : constant ID_Error :=
                    (Kind      => Invalid_Prefix,
                     Message   =>
                       To_Bounded_String
                         ("Invalid prefix, expected: " & Prefix),
                     ID_String => To_Bounded_String (S));
               begin
                  return Err_Result (Err_Val);
               end;
            end if;

            Start_Pos := Trimmed'First + Prefix'Length;
         else
            Start_Pos := Trimmed'First;
         end if;

         --  Extract and validate ULID part
         if Trimmed'Last - Start_Pos + 1 /= ULID_Helpers.ULID_String_Length
         then
            declare
               Err_Val : constant ID_Error :=
                 (Kind      => Invalid_ULID,
                  Message   => To_Bounded_String ("Invalid ULID length"),
                  ID_String => To_Bounded_String (S));
            begin
               return Err_Result (Err_Val);
            end;
         end if;

         ULID_Part := Trimmed (Start_Pos .. Trimmed'Last);

         --  Parse ULID
         begin
            declare
               ULID_Value : constant ULID.ULID_Number :=
                 ULID.Decode (ULID_Part);
               New_ID     : constant ID :=
                 (ULID_Value => ULID_Value, Has_Prefix => (Prefix'Length > 0));
            begin
               return Ok_Result (New_ID);
            end;
         exception
            when others =>
               declare
                  Err_Val : constant ID_Error :=
                    (Kind      => Parse_Failed,
                     Message   => To_Bounded_String ("Failed to parse ULID"),
                     ID_String => To_Bounded_String (S));
               begin
                  return Err_Result (Err_Val);
               end;
         end;
      end From_String;

      function From_ULID (U : ULID.ULID_Number) return ID is
      begin
         return (ULID_Value => U, Has_Prefix => (Prefix'Length > 0));
      end From_ULID;

      function Parse (S : String) return Result_Type is
      begin
         --  Parse is an alias for From_String with the same validation
         return From_String (S);
      end Parse;

      --  ----------------------------------------------------------------------
      --  Accessors
      --  ----------------------------------------------------------------------

      function To_String (Self : ID) return String is
         ULID_Str : constant String := ULID.Encode (Self.ULID_Value);
      begin
         if Self.Has_Prefix then
            return Prefix & ULID_Str;
         else
            return ULID_Str;
         end if;
      end To_String;

      function To_ULID_String (Self : ID) return String is
      begin
         return ULID.Encode (Self.ULID_Value);
      end To_ULID_String;

      function To_ULID (Self : ID) return ULID.ULID_Number is
      begin
         return Self.ULID_Value;
      end To_ULID;

      --  ----------------------------------------------------------------------
      --  Properties
      --  ----------------------------------------------------------------------

      function Timestamp (Self : ID) return Ada.Calendar.Time is
      begin
         return ULID_Helpers.Extract_Timestamp (Self.ULID_Value);
      end Timestamp;

      function Has_Valid_Prefix (Self : ID) return Boolean is
      begin
         --  If no prefix is configured, it's always valid
         if Prefix'Length = 0 then
            return True;
         end if;

         --  Check that our internal state matches
         return Self.Has_Prefix;
      end Has_Valid_Prefix;

      function Age (Self : ID) return Duration is
      begin
         return ULID_Helpers.Get_Age (Self.ULID_Value);
      end Age;

      --  ----------------------------------------------------------------------
      --  Comparison
      --  ----------------------------------------------------------------------

      overriding
      function "=" (Left, Right : ID) return Boolean is
         use type ULID.ULID_Number;
      begin
         return Left.ULID_Value = Right.ULID_Value;
      end "=";

      function "<" (Left, Right : ID) return Boolean is
      begin
         --  ULID comparison is lexicographic, which preserves time ordering
         return ULID.Encode (Left.ULID_Value) < ULID.Encode (Right.ULID_Value);
      end "<";

      function "<=" (Left, Right : ID) return Boolean is
      begin
         return Left = Right or else Left < Right;
      end "<=";

      function ">" (Left, Right : ID) return Boolean is
      begin
         return not (Left <= Right);
      end ">";

      function ">=" (Left, Right : ID) return Boolean is
      begin
         return not (Left < Right);
      end ">=";

      --  ----------------------------------------------------------------------
      --  Validation
      --  ----------------------------------------------------------------------

      function Is_Valid_String (S : String) return Boolean is
         Parse_Result : constant Result_Type := From_String (S);
      begin
         return Is_Ok (Parse_Result);
      end Is_Valid_String;

      function Validate (Self : ID) return Result_Type is
      begin
         --  Check if it's a null ID
         if ULID_Helpers.Is_Null (Self.ULID_Value) then
            declare
               Err_Val : constant ID_Error :=
                 (Kind      => Invalid_Format,
                  Message   => To_Bounded_String ("Null ID is not valid"),
                  ID_String => To_Bounded_String (""));
            begin
               return Err_Result (Err_Val);
            end;
         end if;

         --  Check prefix consistency
         if Prefix'Length > 0 and then not Self.Has_Prefix then
            declare
               Err_Val : constant ID_Error :=
                 (Kind      => Invalid_Prefix,
                  Message   =>
                    To_Bounded_String ("ID missing required prefix"),
                  ID_String => To_Bounded_String (To_String (Self)));
            begin
               return Err_Result (Err_Val);
            end;
         end if;

         --  If we get here, the ID is valid
         return Ok_Result (Self);
      end Validate;

      --  ----------------------------------------------------------------------
      --  Utilities
      --  ----------------------------------------------------------------------

      function Generate_Batch (Count : Positive) return ID_Array is
         Result : ID_Array (1 .. Count);
      begin
         for I in Result'Range loop
            Result (I) := New_ID;
         end loop;
         return Result;
      end Generate_Batch;

      function Null_ID return ID is
      begin
         return
           (ULID_Value => ULID_Helpers.Null_ULID,
            Has_Prefix => (Prefix'Length > 0));
      end Null_ID;

      function Is_Null (Self : ID) return Boolean is
      begin
         return ULID_Helpers.Is_Null (Self.ULID_Value);
      end Is_Null;

      --  ----------------------------------------------------------------------
      --  Hashing Support
      --  ----------------------------------------------------------------------
      
      function Hash (Self : ID) return Ada.Containers.Hash_Type is
         use type Ada.Containers.Hash_Type;
         --  We hash the string representation to ensure consistency.
         --  This includes the prefix (if any) to ensure different ID types
         --  have different hash distributions even for the same ULID value.
         ID_Str : constant String := To_String (Self);
         Result : Ada.Containers.Hash_Type := 5381; -- DJB2 initial value
      begin
         --  DJB2 hash algorithm: hash = hash * 33 + char
         --  This algorithm was created by Daniel J. Bernstein and provides:
         --  - Good distribution across hash buckets
         --  - Simple and fast computation
         --  - Low collision rate for typical strings
         for C of ID_Str loop
            Result := Result * 33 + Ada.Containers.Hash_Type(Character'Pos (C));
         end loop;
         return Result;
      end Hash;

   end Generic_ID_Type;

end Abohlib.Core.Domain.Value_Objects.Type_Safe_Generic_Id;
