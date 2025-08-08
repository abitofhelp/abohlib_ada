--   =============================================================================
--   Test_Type_Safe_Generic_Id - Unit tests for Type-Safe Generic ID
--   =============================================================================
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--
--   Purpose:
--     Unit tests for the Type_Safe_Generic_Id generic package that provides
--     type-safe ID value objects with ULID generation.
--   =============================================================================

pragma Ada_2022;

with Abohlib.Infrastructure.Testing.Test_Framework;

package Test_Type_Safe_Generic_Id is

   use Abohlib.Infrastructure.Testing.Test_Framework;

--   Test suite entry point
   function Run_All_Tests
     (Output : access Test_Output_Port'Class) return Test_Stats_Result.Result;

private

--   Test ID creation with automatic ULID generation
   function Test_Create_Id return Void_Result.Result;

--   Test ID creation from string
   function Test_From_String return Void_Result.Result;

--   Test invalid ID string handling
   function Test_Invalid_Id_String return Void_Result.Result;

--   Test ID to string conversion
   function Test_To_String return Void_Result.Result;

--   Test ID equality comparison
   function Test_Id_Equality return Void_Result.Result;

--   Test ID ordering (for sorting)
   function Test_Id_Ordering return Void_Result.Result;

--   Test prefix validation
   function Test_Prefix_Validation return Void_Result.Result;

--   Test ID uniqueness
   function Test_Id_Uniqueness return Void_Result.Result;

--   Test null ID handling
   function Test_Null_Id return Void_Result.Result;

--   Test ID hashing
   function Test_Id_Hash return Void_Result.Result;

end Test_Type_Safe_Generic_Id;
