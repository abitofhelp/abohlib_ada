--   =============================================================================
--   Abohlib.Core.Domain.Services.Generic_SHA256_Hasher - Generic SHA256 Hasher
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--
--   Generic SHA256 hasher that can process data in chunks and produce a final
--   hash. This is the default hasher for integrity verification across abohlib.
--   =============================================================================

pragma Ada_2022;

with Ada.Streams;           use Ada.Streams;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Finalization;
with Abohlib.Core.Domain.Result;
with Abohlib.Core.Domain.Constants.Cryptography;
use Abohlib.Core.Domain.Constants.Cryptography;

generic
   type Data_Type is private;
   with
     function To_Stream_Elements
       (Data : Data_Type) return Stream_Element_Array;
package Abohlib.Core.Domain.Services.Generic_SHA256_Hasher is

   --  Constants (from cryptographic constants package)
   SHA256_Hex_Length : constant := SHA256_HASH_SIZE_HEX;  -- SHA256 produces 32 bytes = 64 hex chars

   --  Type for the SHA256 hasher
   type SHA256_Hasher_Type is new Ada.Finalization.Controlled with private;

   --  Result types for error handling
   package Hash_Result is new
     Result.Result_Package
       (Ok_Type  => Unbounded_String,  -- SHA256 hash as hex string
        Err_Type => Unbounded_String);

   --  Constructor
   function Create return SHA256_Hasher_Type;
   --  Creates a new SHA256 hasher

   --  Update the hash with new data
   procedure Update (Hasher : in out SHA256_Hasher_Type; Data : Data_Type)
   with
     Pre => not Hasher.Is_Finalized,
     Post => Hasher.Bytes_Processed > Hasher.Bytes_Processed'Old;
   --  Updates the running hash with new data

   --  Alternative update for stream elements
   procedure Update_Stream
     (Hasher : in out SHA256_Hasher_Type; Data : Stream_Element_Array)
   with
     Pre => not Hasher.Is_Finalized,
     Post => Hasher.Bytes_Processed > Hasher.Bytes_Processed'Old;
   --  Updates the running hash with stream element data

   --  Get the final hash
   function Finalize_Hash
     (Hasher : in out SHA256_Hasher_Type) return Hash_Result.Result
   with Pre => not Hasher.Is_Finalized, Post => Hasher.Is_Finalized;
   --  Finalizes and returns the SHA256 hash

   --  Query methods
   function Updates_Count (Hasher : SHA256_Hasher_Type) return Natural
   with Inline;
   --  Returns the number of update calls

   function Bytes_Processed
     (Hasher : SHA256_Hasher_Type) return Long_Long_Integer
   with Inline;
   --  Returns the total number of bytes processed

   function Is_Finalized (Hasher : SHA256_Hasher_Type) return Boolean
   with Inline;
   --  Returns True if the hash has been finalized

   function Current_Hash (Hasher : SHA256_Hasher_Type) return String
   with
     Post =>
       (if not Hasher.Is_Finalized then Current_Hash'Result = ""
        else Current_Hash'Result'Length = SHA256_Hex_Length);
   --  Returns the current hash value (empty if not finalized)

   --  Reset the hasher for reuse
   procedure Reset (Hasher : in out SHA256_Hasher_Type)
   with
     Post =>
       not Hasher.Is_Finalized
       and then Hasher.Updates_Count = 0
       and then Hasher.Bytes_Processed = 0;
   --  Resets the hasher to initial state

   --  Utility: Direct hash calculation
   function Calculate_Hash (Data : Data_Type) return String
   with Post => Calculate_Hash'Result'Length = SHA256_Hex_Length;
   --  Calculates SHA256 hash of data in one shot

   function Calculate_Hash_Stream (Data : Stream_Element_Array) return String
   with Post => Calculate_Hash_Stream'Result'Length = 64;
   --  Calculates SHA256 hash of stream data in one shot

   --  String representation
   function Image (Hasher : SHA256_Hasher_Type) return String;

private
   --  Implementation-specific hash context
   type Hash_Context_Type;
   type Hash_Context_Access is access Hash_Context_Type;

   type SHA256_Hasher_Type is new Ada.Finalization.Controlled with record
      Context         : Hash_Context_Access;
      Updates_Count   : Natural := 0;
      Bytes_Processed : Long_Long_Integer := 0;
      Is_Finalized    : Boolean := False;
      Final_Hash      : Unbounded_String;
   end record;

   overriding
   procedure Initialize (Hasher : in out SHA256_Hasher_Type);
   overriding
   procedure Finalize (Hasher : in out SHA256_Hasher_Type);
   overriding
   procedure Adjust (Hasher : in out SHA256_Hasher_Type);

end Abohlib.Core.Domain.Services.Generic_SHA256_Hasher;
