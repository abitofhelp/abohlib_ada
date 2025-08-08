--   =============================================================================
--   Abohlib.Core.Domain.Services.SHA256_Hasher - SHA256 Hasher
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--
--   Non-generic SHA256 hasher that processes Stream_Element_Array data directly.
--   This is useful when you don't need the generic type conversion functionality.
--   =============================================================================

pragma Ada_2022;

with Ada.Streams;           use Ada.Streams;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Finalization;
with Abohlib.Core.Domain.Result;
with Abohlib.Core.Domain.Services.Generic_SHA256_Hasher;
with Abohlib.Core.Domain.Constants.Cryptography;
use Abohlib.Core.Domain.Constants.Cryptography;

package Abohlib.Core.Domain.Services.SHA256_Hasher is

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
   procedure Update
     (Hasher : in out SHA256_Hasher_Type; Data : Stream_Element_Array)
   with
     Pre => not Is_Finalized (Hasher),
     Post => Bytes_Processed (Hasher) > Bytes_Processed (Hasher)'Old;
   --  Updates the running hash with stream element data

   --  Get the final hash
   function Finalize_Hash
     (Hasher : in out SHA256_Hasher_Type) return Hash_Result.Result
   with Pre => not Is_Finalized (Hasher), Post => Is_Finalized (Hasher);
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
       (if not Is_Finalized (Hasher) then Current_Hash'Result = ""
        else Current_Hash'Result'Length = SHA256_Hex_Length);
   --  Returns the current hash value (empty if not finalized)

   --  Reset the hasher for reuse
   procedure Reset (Hasher : in out SHA256_Hasher_Type)
   with
     Post =>
       not Is_Finalized (Hasher)
       and then Updates_Count (Hasher) = 0
       and then Bytes_Processed (Hasher) = 0;
   --  Resets the hasher to initial state

   --  String representation
   function Image (Hasher : SHA256_Hasher_Type) return String;

   --  ==========================================================================
   --  Constants
   --  ==========================================================================
   
   --  SHA256 processing block size
   SHA256_Block_Size : constant := 4_096;  -- 4KB blocks for efficient hashing

private
   --  We need a definite subtype for the generic instantiation
   subtype Stream_Block is Stream_Element_Array (1 .. SHA256_Block_Size);

   --  Identity function for our block type
   function Block_To_Stream (Data : Stream_Block) return Stream_Element_Array
   is (Stream_Element_Array (Data));

   --  Import the generic hasher with our block type
   package Block_SHA256 is new
     Generic_SHA256_Hasher
       (Data_Type          => Stream_Block,
        To_Stream_Elements => Block_To_Stream);

   type SHA256_Hasher_Type is new Ada.Finalization.Controlled with record
      Implementation : Block_SHA256.SHA256_Hasher_Type;
   end record;

   overriding
   procedure Initialize (Hasher : in out SHA256_Hasher_Type);
   overriding
   procedure Finalize (Hasher : in out SHA256_Hasher_Type);
   overriding
   procedure Adjust (Hasher : in out SHA256_Hasher_Type);

end Abohlib.Core.Domain.Services.SHA256_Hasher;
