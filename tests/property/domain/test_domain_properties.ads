--   =============================================================================
--   Test_Domain_Properties - Property-Based Tests for Domain Invariants
--   =============================================================================
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--
--   Purpose:
--     Property-based tests for validating domain invariants and contracts
--     using automatically generated test data. Ensures domain rules hold
--     across all possible inputs and edge cases.
--
--   Coverage:
--     - ULID generation and validation properties
--     - File path security and normalization properties
--     - Aggregate root invariant properties
--     - Repository contract properties
--     - Mathematical properties (commutativity, associativity, etc.)
--   =============================================================================

pragma Ada_2022;

with Abohlib.Infrastructure.Testing.Test_Framework;
with Abohlib.Infrastructure.Testing.Property_Testing;

package Test_Domain_Properties is

   use Abohlib.Infrastructure.Testing.Test_Framework;
   use Abohlib.Infrastructure.Testing.Property_Testing;

--   Main test runner for all property-based tests
   function Run_All_Tests
     (Output : access Test_Output_Port'Class) return Test_Stats_Result.Result;

private

--   ==========================================================================
--   ULID Property Tests
--   ==========================================================================

   function Test_ULID_Generation_Properties return Void_Result.Result;
   function Test_ULID_Validation_Properties return Void_Result.Result;
   function Test_ULID_Ordering_Properties return Void_Result.Result;

--   ==========================================================================
--   File Path Property Tests
--   ==========================================================================

   function Test_File_Path_Security_Properties return Void_Result.Result;
   function Test_File_Path_Normalization_Properties return Void_Result.Result;
   function Test_File_Path_Validation_Properties return Void_Result.Result;

--   ==========================================================================
--   Error Handling Property Tests
--   ==========================================================================

   function Test_Result_Pattern_Properties return Void_Result.Result;
   function Test_Error_Construction_Properties return Void_Result.Result;

--   ==========================================================================
--   Mathematical Property Tests
--   ==========================================================================

   function Test_String_Concatenation_Properties return Void_Result.Result;
   function Test_Natural_Addition_Properties return Void_Result.Result;
   function Test_Boolean_Operations_Properties return Void_Result.Result;

--   ==========================================================================
--   Contract Validation Property Tests
--   ==========================================================================

   function Test_Aggregate_Invariant_Properties return Void_Result.Result;
   function Test_Repository_Contract_Properties return Void_Result.Result;

--   ==========================================================================
--   Integration Property Tests
--   ==========================================================================

   function Test_Cross_Layer_Properties return Void_Result.Result;
   function Test_Performance_Properties return Void_Result.Result;

end Test_Domain_Properties;
