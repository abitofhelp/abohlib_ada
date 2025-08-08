--   =============================================================================
--   Test_Aggregate_Root - Unit tests for DDD Aggregate Root Pattern
--   =============================================================================
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--
--   Purpose:
--     Unit tests for the Aggregate_Root generic package.
--   =============================================================================

pragma Ada_2022;

with Abohlib.Infrastructure.Testing.Test_Framework;

package Test_Aggregate_Root is

   use Abohlib.Infrastructure.Testing.Test_Framework;

--   Test suite entry point
   function Run_All_Tests
     (Output : access Test_Output_Port'Class) return Test_Stats_Result.Result;

private

--   Test aggregate creation and initialization
   function Test_Aggregate_Creation return Void_Result.Result;

--   Test ID management
   function Test_Set_Id return Void_Result.Result;

--   Test event raising and tracking
   function Test_Raise_Event return Void_Result.Result;

--   Test marking events as committed
   function Test_Mark_Events_Committed return Void_Result.Result;

--   Test loading from event history
   function Test_Load_From_History return Void_Result.Result;

--   Test version management
   function Test_Version_Management return Void_Result.Result;

--   Test change tracking
   function Test_Change_Tracking return Void_Result.Result;

--   Test rollback functionality
   function Test_Rollback_Changes return Void_Result.Result;

--   Test aggregate validation
   function Test_Aggregate_Validation return Void_Result.Result;

--   Test concurrent modification handling
   function Test_Optimistic_Concurrency return Void_Result.Result;

end Test_Aggregate_Root;
