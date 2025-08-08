--   =============================================================================
--   Abohlib.Core.Domain.Services.Generic_SHA256_Hasher - Implementation
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--   =============================================================================

pragma Ada_2022;

with Ada.Unchecked_Deallocation;
with Ada.Exceptions;
--  Ada.Streams already visible through parent spec
with SHA2;

package body Abohlib.Core.Domain.Services.Generic_SHA256_Hasher is

   --  Constants for SHA256 operations
   Hex_Base : constant := 16;           -- Base for hexadecimal conversion

   --  Define the actual hash context type
   type Hash_Context_Type is limited record
      Context : SHA2.SHA_256.Context;
   end record;

   procedure Free is new
     Ada.Unchecked_Deallocation (Hash_Context_Type, Hash_Context_Access);

   -- -----------
   --  Create
   -- -----------

   function Create return SHA256_Hasher_Type is
   begin
      return Hasher : SHA256_Hasher_Type do
         Initialize (Hasher);
      end return;
   end Create;

   -- -----------
   --  Update
   -- -----------

   procedure Update (Hasher : in out SHA256_Hasher_Type; Data : Data_Type) is
      Stream_Data : constant Stream_Element_Array := To_Stream_Elements (Data);
   begin
      Update_Stream (Hasher, Stream_Data);
   end Update;

   -- -----------------
   --  Update_Stream
   -- -----------------

   procedure Update_Stream
     (Hasher : in out SHA256_Hasher_Type; Data : Stream_Element_Array) is
   begin
      --  SHA2 works directly with Stream_Element_Array, no conversion needed!
      SHA2.SHA_256.Update (Hasher.Context.Context, Data);

      --  Update statistics
      Hasher.Updates_Count := Hasher.Updates_Count + 1;
      Hasher.Bytes_Processed :=
        Hasher.Bytes_Processed + Long_Long_Integer (Data'Length);
   end Update_Stream;

   -- -----------------
   --  Finalize_Hash
   -- -----------------

   function Finalize_Hash
     (Hasher : in out SHA256_Hasher_Type) return Hash_Result.Result is
   begin
      declare
         Digest    : constant SHA2.SHA_256.Digest :=
           SHA2.SHA_256.Finalize (Hasher.Context.Context);
         Result    :
           String (1 .. SHA256_Hex_Length);
         Hex_Chars : constant String := "0123456789abcdef";
      begin
         --  Convert digest bytes to hex string
         for I in Digest'Range loop
            declare
               Byte        : constant Stream_Element := Digest (I);
               High_Nibble : constant Natural := Natural (Byte / Hex_Base) + 1;
               Low_Nibble  : constant Natural := Natural (Byte mod Hex_Base) + 1;
               Pos         : constant Natural :=
                 2 * Natural (I - Digest'First) + 1;
            begin
               Result (Pos) := Hex_Chars (High_Nibble);
               Result (Pos + 1) := Hex_Chars (Low_Nibble);
            end;
         end loop;

         Hasher.Final_Hash := To_Unbounded_String (Result);
         Hasher.Is_Finalized := True;
         return Hash_Result.Ok (Hasher.Final_Hash);
      end;
   exception
      when E : others =>
         return
           Hash_Result.Err
             (To_Unbounded_String
                ("Hash finalization failed: "
                 & Ada.Exceptions.Exception_Message (E)));
   end Finalize_Hash;

   -- -----------------
   --  Updates_Count
   -- -----------------

   function Updates_Count (Hasher : SHA256_Hasher_Type) return Natural
   is (Hasher.Updates_Count);

   -- -------------------
   --  Bytes_Processed
   -- -------------------

   function Bytes_Processed
     (Hasher : SHA256_Hasher_Type) return Long_Long_Integer
   is (Hasher.Bytes_Processed);

   -- ----------------
   --  Is_Finalized
   -- ----------------

   function Is_Finalized (Hasher : SHA256_Hasher_Type) return Boolean
   is (Hasher.Is_Finalized);

   -- ----------------
   --  Current_Hash
   -- ----------------

   function Current_Hash (Hasher : SHA256_Hasher_Type) return String
   is (if Hasher.Is_Finalized then To_String (Hasher.Final_Hash) else "");

   -- ---------
   --  Reset
   -- ---------

   procedure Reset (Hasher : in out SHA256_Hasher_Type) is
   begin
      Hasher.Context.Context := SHA2.SHA_256.Initialize;
      Hasher.Updates_Count := 0;
      Hasher.Bytes_Processed := 0;
      Hasher.Is_Finalized := False;
      Hasher.Final_Hash := Null_Unbounded_String;
   end Reset;

   -- ------------------
   --  Calculate_Hash
   -- ------------------

   function Calculate_Hash (Data : Data_Type) return String is
      Stream_Data : constant Stream_Element_Array := To_Stream_Elements (Data);
   begin
      return Calculate_Hash_Stream (Stream_Data);
   end Calculate_Hash;

   -- ------------------------
   --  Calculate_Hash_Stream
   -- ------------------------

   function Calculate_Hash_Stream (Data : Stream_Element_Array) return String
   is
      Context : SHA2.SHA_256.Context := SHA2.SHA_256.Initialize;
   begin
      --  SHA2 works directly with Stream_Element_Array, no conversion needed!
      SHA2.SHA_256.Update (Context, Data);

      --  Get the digest and convert to hex string
      declare
         Digest    : constant SHA2.SHA_256.Digest :=
           SHA2.SHA_256.Finalize (Context);
         Result    :
           String (1 .. SHA256_Hex_Length);
         Hex_Chars : constant String := "0123456789abcdef";
      begin
         --  Convert digest bytes to hex string
         for I in Digest'Range loop
            declare
               Byte        : constant Stream_Element := Digest (I);
               High_Nibble : constant Natural := Natural (Byte / Hex_Base) + 1;
               Low_Nibble  : constant Natural := Natural (Byte mod Hex_Base) + 1;
               Pos         : constant Natural :=
                 2 * Natural (I - Digest'First) + 1;
            begin
               Result (Pos) := Hex_Chars (High_Nibble);
               Result (Pos + 1) := Hex_Chars (Low_Nibble);
            end;
         end loop;
         return Result;
      end;
   end Calculate_Hash_Stream;

   -- ---------
   --  Image
   -- ---------

   function Image (Hasher : SHA256_Hasher_Type) return String is
   begin
      return
        "SHA256_Hasher[updates="
        & Hasher.Updates_Count'Image
        & ", bytes="
        & Hasher.Bytes_Processed'Image
        & ", finalized="
        & Hasher.Is_Finalized'Image
        & "]";
   end Image;

   -- ---------------
   --  Initialize
   -- ---------------

   overriding
   procedure Initialize (Hasher : in out SHA256_Hasher_Type) is
   begin
      Hasher.Context := new Hash_Context_Type;
      Hasher.Context.Context := SHA2.SHA_256.Initialize;
      Hasher.Updates_Count := 0;
      Hasher.Bytes_Processed := 0;
      Hasher.Is_Finalized := False;
      Hasher.Final_Hash := Null_Unbounded_String;
   end Initialize;

   -- -------------
   --  Finalize
   -- -------------

   overriding
   procedure Finalize (Hasher : in out SHA256_Hasher_Type) is
   begin
      if Hasher.Context /= null then
         Free (Hasher.Context);
      end if;
   end Finalize;

   -- -----------
   --  Adjust
   -- -----------

   overriding
   procedure Adjust (Hasher : in out SHA256_Hasher_Type) is
   begin
      if Hasher.Context /= null then
         declare
            Old_Context : constant Hash_Context_Access := Hasher.Context;
         begin
            Hasher.Context := new Hash_Context_Type;
            Hasher.Context.Context := Old_Context.Context;
         end;
      end if;
   end Adjust;

end Abohlib.Core.Domain.Services.Generic_SHA256_Hasher;
