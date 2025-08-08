--   =============================================================================
--   Test_Logging_Unit - Logging Infrastructure Unit Tests
--   =============================================================================
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--
--   Purpose:
--     Comprehensive unit tests for the logging infrastructure including:
--     - Logger configuration and management
--     - Log level filtering and comparison
--     - Message formatting and timestamping
--     - Structured logging with context
--     - Built-in logger implementations
--     - Thread safety and state management
--     - Logger factory pattern
--   =============================================================================

pragma Ada_2022;

with Abohlib.Infrastructure.Testing.Test_Framework;

package Test_Logging_Unit is

   use Abohlib.Infrastructure.Testing.Test_Framework;

--   Main test runner for all logging tests
   function Run_All_Tests
     (Output : access Test_Output_Port'Class) return Test_Stats_Result.Result;

private

--   ==========================================================================
--   Logger State Management Tests
--   ==========================================================================

   function Test_Logger_Configuration return Void_Result.Result;
   function Test_Logger_State_Management return Void_Result.Result;
   function Test_Min_Level_Configuration return Void_Result.Result;

--   ==========================================================================
--   Log Level Tests
--   ==========================================================================

   function Test_Log_Level_Comparison return Void_Result.Result;
   function Test_Log_Level_Filtering return Void_Result.Result;
   function Test_Level_String_Conversion return Void_Result.Result;

--   ==========================================================================
--   Basic Logging Function Tests
--   ==========================================================================

   function Test_Basic_Logging_Functions return Void_Result.Result;
   function Test_Convenience_Logging_Functions return Void_Result.Result;
   function Test_Logging_With_Null_Logger return Void_Result.Result;

--   ==========================================================================
--   Message Formatting Tests
--   ==========================================================================

   function Test_Message_Formatting return Void_Result.Result;
   function Test_Timestamped_Message_Formatting return Void_Result.Result;
   function Test_Message_Format_Boundaries return Void_Result.Result;

--   ==========================================================================
--   Structured Logging Tests
--   ==========================================================================

   function Test_Context_Creation return Void_Result.Result;
   function Test_Context_Key_Value_Addition return Void_Result.Result;
   function Test_Context_Boundary_Conditions return Void_Result.Result;
   function Test_Context_Formatting return Void_Result.Result;
   function Test_Structured_Logging return Void_Result.Result;

--   ==========================================================================
--   Built-in Logger Tests
--   ==========================================================================

   function Test_Console_Logger return Void_Result.Result;
   function Test_Null_Logger return Void_Result.Result;
   function Test_Timestamped_Console_Logger return Void_Result.Result;

--   ==========================================================================
--   Logger Factory Tests
--   ==========================================================================

   function Test_Logger_Factory_Creation return Void_Result.Result;
   function Test_Logger_Factory_Types return Void_Result.Result;

--   ==========================================================================
--   Integration and State Tests
--   ==========================================================================

   function Test_Logger_Integration return Void_Result.Result;
   function Test_Thread_Safety_Simulation return Void_Result.Result;

end Test_Logging_Unit;
