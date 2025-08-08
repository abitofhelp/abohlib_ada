--  =============================================================================
--  Abohlib.Core.Domain.Value_Objects.Type_Safe_Generic_Id - Type-Safe Generic ID Value Object
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Purpose:
--    Provides a generic, type-safe ID value object using phantom types to
--    prevent mixing IDs from different entity types at compile time.
--    Uses ULID (Universally Unique Lexicographically Sortable Identifier)
--    for the underlying ID generation.
--
--  Usage:
--    This generic package creates type-safe IDs for different entities:
--    - Compile-time prevention of ID mixing
--    - Zero runtime overhead (phantom types)
--    - Lexicographically sortable IDs
--    - Embedded timestamp for creation time
--
--  Example:
--    --  Define ID categories
--    type User_Category is null record;
--    type Order_Category is null record;
--
--    --  Instantiate ID types
--    package User_ID is new Abohlib.Generic_ID
--       (Category => User_Category,
--        Category_Name => "User",
--        Prefix => "usr_");
--
--    package Order_ID is new Abohlib.Generic_ID
--       (Category => Order_Category,
--        Category_Name => "Order",
--        Prefix => "ord_");
--
--    --  Use the IDs (type-safe!)
--    User_1 : User_ID.ID := User_ID.New_ID;
--    Order_1 : Order_ID.ID := Order_ID.New_ID;
--
--    --  This would be a compile error:
--    --  User_1 := Order_1;  -- Type mismatch!
--  =============================================================================

pragma Ada_2022;

with Ada.Strings.Bounded;
with Ada.Calendar;
with Ada.Containers;
with ULID;
with Abohlib.Core.Domain.Result;

package Abohlib.Core.Domain.Value_Objects.Type_Safe_Generic_Id is

   --  Maximum ID string length (ULID is 26 chars + optional prefix)
   Max_ID_Length : constant := 50;
   package ID_Strings is new
     Ada.Strings.Bounded.Generic_Bounded_Length (Max_ID_Length);
   use ID_Strings;

   --  Error types for ID operations
   type ID_Error_Kind is
     (Invalid_Format,
      Invalid_Prefix,
      Invalid_ULID,
      Parse_Failed,
      Timestamp_Extraction_Failed);

   type ID_Error is record
      Kind      : ID_Error_Kind;
      Message   : ID_Strings.Bounded_String;
      ID_String : ID_Strings.Bounded_String;
   end record;

   function Default_ID_Error return ID_Error
   is (Kind      => Invalid_Format,
       Message   => ID_Strings.Null_Bounded_String,
       ID_String => ID_Strings.Null_Bounded_String);

   --  ==========================================================================
   --  Generic ID Package
   --  ==========================================================================

   generic
      type Category is private;  -- Phantom type for compile-time safety
      Category_Name : String;    -- Human-readable category name
      Prefix : String := "";     -- Optional prefix for string representation
   package Generic_ID_Type is
      pragma Warnings (Off, Category);  -- Phantom type, intentionally unused

      --  The ID type (opaque)
      type ID is private;

      --  Result type (forward declaration)
      type Result_Type (<>) is tagged private;

      --  Constructor functions for Result_Type
      function Ok_Result (Value : ID) return Result_Type;
      function Err_Result (Error : ID_Error) return Result_Type;

      --  Result query functions
      function Is_Ok (R : Result_Type) return Boolean;
      function Is_Err (R : Result_Type) return Boolean;
      function Get_Ok (R : Result_Type) return ID;
      function Get_Err (R : Result_Type) return ID_Error;

      --  Default ID for Result type
      function Default_ID return ID;

      --  =======================================================================
      --  Constructors
      --  =======================================================================

      --  Generate a new unique ID
      function New_ID return ID;

      --  Create ID from string representation
      function From_String (S : String) return Result_Type;

      --  Create ID from ULID bytes
      function From_ULID (U : ULID.ULID_Number) return ID;

      --  Parse ID from various formats
      function Parse (S : String) return Result_Type;

      --  =======================================================================
      --  Accessors
      --  =======================================================================

      --  Convert to string representation (with prefix if configured)
      function To_String (Self : ID) return String;

      --  Get just the ULID part (without prefix)
      function To_ULID_String (Self : ID) return String;

      --  Get the underlying ULID
      function To_ULID (Self : ID) return ULID.ULID_Number;

      --  Get the category name
      function Get_Category_Name return String
      is (Category_Name);

      --  Get the prefix
      function Get_Prefix return String
      is (Prefix);

      --  =======================================================================
      --  Properties
      --  =======================================================================

      --  Extract timestamp from ID
      function Timestamp (Self : ID) return Ada.Calendar.Time;

      --  Check if ID has the expected prefix
      function Has_Valid_Prefix (Self : ID) return Boolean;

      --  Get age of ID (time since creation)
      function Age (Self : ID) return Duration;

      --  =======================================================================
      --  Comparison
      --  =======================================================================

      --  Equality
      overriding
      function "=" (Left, Right : ID) return Boolean;

      --  Ordering (based on ULID's lexicographic properties)
      function "<" (Left, Right : ID) return Boolean;
      function "<=" (Left, Right : ID) return Boolean;
      function ">" (Left, Right : ID) return Boolean;
      function ">=" (Left, Right : ID) return Boolean;

      --  =======================================================================
      --  Validation
      --  =======================================================================

      --  Check if a string is a valid ID
      function Is_Valid_String (S : String) return Boolean;

      --  Validate an ID
      function Validate (Self : ID) return Result_Type;

      --  =======================================================================
      --  Utilities
      --  =======================================================================

      --  Generate a batch of IDs (useful for bulk operations)
      type ID_Array is array (Positive range <>) of ID;

      function Generate_Batch (Count : Positive) return ID_Array
      with Pre => Count <= 1000;  -- Reasonable limit

      --  Create a "null" or "zero" ID (for optional fields)
      function Null_ID return ID;
      function Is_Null (Self : ID) return Boolean;

      --  =======================================================================
      --  Hashing Support
      --  =======================================================================
      
      --  Hash function for use with Ada.Containers (Hashed_Maps, Hashed_Sets)
      --
      --  Purpose:
      --    Converts an ID to a hash value for use in hash-based containers.
      --    This allows IDs to be used as keys in hashed maps or elements in
      --    hashed sets for efficient O(1) lookups.
      --
      --  Implementation:
      --    Uses the DJB2 algorithm (created by Daniel J. Bernstein), which
      --    provides good distribution and is simple to implement. The algorithm
      --    processes each character of the ID string representation.
      --
      --  Example:
      --    -- Create a hashed map with IDs as keys
      --    package User_Maps is new Ada.Containers.Hashed_Maps
      --      (Key_Type     => User_ID.ID,
      --       Element_Type => User_Record,
      --       Hash         => User_ID.Hash,
      --       Equivalent_Keys => User_ID."=");
      --
      --  Performance:
      --    O(n) where n is the length of the ID string (typically 26-35 chars)
      function Hash (Self : ID) return Ada.Containers.Hash_Type;

   private

      type ID is record
         ULID_Value : ULID.ULID_Number;
         Has_Prefix : Boolean := (Prefix'Length > 0);
      end record;

      --  Result type for this ID
      package ID_Result is new
        Abohlib.Core.Domain.Result.Result_Package
          (Ok_Type  => ID,
           Err_Type => ID_Error);

      type Result_Type is new ID_Result.Result with null record;

   end Generic_ID_Type;

end Abohlib.Core.Domain.Value_Objects.Type_Safe_Generic_Id;
