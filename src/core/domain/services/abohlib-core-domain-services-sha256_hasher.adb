--   =============================================================================
--   Abohlib.Core.Domain.Services.SHA256_Hasher - Implementation
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--   =============================================================================

pragma Ada_2022;

package body Abohlib.Core.Domain.Services.SHA256_Hasher is

   -- ----------------
   --  Initialize
   -- ----------------

   overriding
   procedure Initialize (Hasher : in out SHA256_Hasher_Type) is
   begin
      Hasher.Implementation := Block_SHA256.Create;
   end Initialize;

   -- ------------
   --  Finalize
   -- ------------

   overriding
   procedure Finalize (Hasher : in out SHA256_Hasher_Type) is
   begin
      null;  -- Controlled type handles its own cleanup
   end Finalize;

   -- ----------
   --  Adjust
   -- ----------

   overriding
   procedure Adjust (Hasher : in out SHA256_Hasher_Type) is
   begin
      null;  -- Default deep copy is fine
   end Adjust;

   -- ----------
   --  Create
   -- ----------

   function Create return SHA256_Hasher_Type is
   begin
      return Result : SHA256_Hasher_Type do
         Result.Implementation := Block_SHA256.Create;
      end return;
   end Create;

   -- ----------
   --  Update
   -- ----------

   procedure Update
     (Hasher : in out SHA256_Hasher_Type; Data : Stream_Element_Array) is
   begin
      Block_SHA256.Update_Stream (Hasher.Implementation, Data);
   end Update;

   -- -----------------
   --  Finalize_Hash
   -- -----------------

   function Finalize_Hash
     (Hasher : in out SHA256_Hasher_Type) return Hash_Result.Result is
   begin
      declare
         Result : constant Block_SHA256.Hash_Result.Result :=
           Block_SHA256.Finalize_Hash (Hasher.Implementation);
      begin
         if Block_SHA256.Hash_Result.Is_Ok (Result) then
            return Hash_Result.Ok (Block_SHA256.Hash_Result.Get_Ok (Result));
         else
            return Hash_Result.Err (Block_SHA256.Hash_Result.Get_Err (Result));
         end if;
      end;
   end Finalize_Hash;

   -- -----------------
   --  Updates_Count
   -- -----------------

   function Updates_Count (Hasher : SHA256_Hasher_Type) return Natural is
   begin
      return Block_SHA256.Updates_Count (Hasher.Implementation);
   end Updates_Count;

   -- -------------------
   --  Bytes_Processed
   -- -------------------

   function Bytes_Processed
     (Hasher : SHA256_Hasher_Type) return Long_Long_Integer is
   begin
      return Block_SHA256.Bytes_Processed (Hasher.Implementation);
   end Bytes_Processed;

   -- -----------------
   --  Is_Finalized
   -- -----------------

   function Is_Finalized (Hasher : SHA256_Hasher_Type) return Boolean is
   begin
      return Block_SHA256.Is_Finalized (Hasher.Implementation);
   end Is_Finalized;

   -- ----------------
   --  Current_Hash
   -- ----------------

   function Current_Hash (Hasher : SHA256_Hasher_Type) return String is
   begin
      return Block_SHA256.Current_Hash (Hasher.Implementation);
   end Current_Hash;

   -- ---------
   --  Reset
   -- ---------

   procedure Reset (Hasher : in out SHA256_Hasher_Type) is
   begin
      Block_SHA256.Reset (Hasher.Implementation);
   end Reset;

   -- ---------
   --  Image
   -- ---------

   function Image (Hasher : SHA256_Hasher_Type) return String is
   begin
      return Block_SHA256.Image (Hasher.Implementation);
   end Image;

end Abohlib.Core.Domain.Services.SHA256_Hasher;
