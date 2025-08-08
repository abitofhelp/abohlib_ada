--   =============================================================================
--   Test_ACID_Repository - Unit tests for ACID Repository pattern
--   =============================================================================
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--
--   Purpose:
--     Unit tests for the ACID repository pattern implementation that ensures
--     Atomicity, Consistency, Isolation, and Durability for aggregate operations.
--   =============================================================================

pragma Ada_2022;

with Abohlib.Infrastructure.Testing.Test_Framework;

package Test_ACID_Repository is

   use Abohlib.Infrastructure.Testing.Test_Framework;

--   Test suite entry point
   function Run_All_Tests
     (Output : access Test_Output_Port'Class) return Test_Stats_Result.Result;

private

--   Individual test functions
   function Test_Save_New_Aggregate return Void_Result.Result;
   function Test_Update_Existing_Aggregate return Void_Result.Result;
   function Test_Delete_Aggregate return Void_Result.Result;
   function Test_Find_By_Id return Void_Result.Result;
   function Test_Transaction_Commit return Void_Result.Result;
   function Test_Transaction_Rollback return Void_Result.Result;
   function Test_Concurrent_Access return Void_Result.Result;
   function Test_Version_Conflict return Void_Result.Result;
   function Test_Isolation_Levels return Void_Result.Result;
   function Test_ACID_Properties return Void_Result.Result;

end Test_ACID_Repository;
