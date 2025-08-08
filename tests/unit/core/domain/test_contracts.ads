--   =============================================================================
--   Test_Contracts - Contract Validation Unit Tests
--   =============================================================================
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--
--   Purpose:
--     Provides comprehensive tests for Design by Contract (DbC) implementations
--     across the domain layer. Tests validate that Pre/Post conditions work
--     correctly and provide meaningful error detection.
--
--   Coverage:
--     - ACID Repository contracts (transaction state management)
--     - Aggregate Root contracts (state reconstruction, change tracking)
--     - File Path contracts (path validation, extension checks)
--     - ULID contracts (string validation, generation safety)
--     - Error Construction contracts (bounded string safety)
--   =============================================================================

pragma Ada_2022;

with Abohlib.Infrastructure.Testing.Test_Framework;

package Test_Contracts is

   use Abohlib.Infrastructure.Testing.Test_Framework;

--   Main test runner for all contract tests
   function Run_All_Tests
     (Output : access Test_Output_Port'Class) return Test_Stats_Result.Result;

private

--   ==========================================================================
--   File Path Contract Tests
--   ==========================================================================

   function Test_File_Path_Creation_Contracts return Void_Result.Result;
   function Test_File_Path_Validation_Contracts return Void_Result.Result;
   function Test_File_Path_Manipulation_Contracts return Void_Result.Result;

--   ==========================================================================
--   ULID Contract Tests
--   ==========================================================================

   function Test_ULID_String_Validation_Contracts return Void_Result.Result;
   function Test_ULID_Generation_Contracts return Void_Result.Result;
   function Test_ULID_Protected_Generator_Contracts return Void_Result.Result;

--   ==========================================================================
--   Error Construction Contract Tests
--   ==========================================================================

   function Test_Error_Constructor_Length_Limits return Void_Result.Result;
   function Test_Error_Factory_Length_Limits return Void_Result.Result;
   function Test_Error_Constructor_Boundary_Conditions return Void_Result.Result;

--   ==========================================================================
--   ACID Repository Contract Tests
--   ==========================================================================

   function Test_Repository_Transaction_State_Contracts return Void_Result.Result;
   function Test_Repository_Save_Contracts return Void_Result.Result;
   function Test_Repository_Batch_Operation_Contracts return Void_Result.Result;

--   ==========================================================================
--   Aggregate Root Contract Tests
--   ==========================================================================

   function Test_Aggregate_State_Management_Contracts return Void_Result.Result;
   function Test_Aggregate_Event_History_Contracts return Void_Result.Result;
   function Test_Aggregate_Change_Tracking_Contracts return Void_Result.Result;

end Test_Contracts;
