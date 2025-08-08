--   =============================================================================
--   Test_Concurrent_Circuit_Breaker - Thread Safety Tests
--   =============================================================================
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--
--   Purpose:
--     Tests for the protected circuit breaker implementation to verify
--     thread safety, state consistency, and performance under concurrent load.
--   =============================================================================

pragma Ada_2022;

with Abohlib.Infrastructure.Testing.Test_Framework;

package Test_Concurrent_Circuit_Breaker is

   use Abohlib.Infrastructure.Testing.Test_Framework;

--   Test concurrent updates to circuit breaker
   function Test_Concurrent_Circuit_Updates return Void_Result.Result;

--   Test state transitions under concurrent load
   function Test_State_Transitions_Under_Load return Void_Result.Result;

--   Test event handler thread safety
   function Test_Event_Handler_Thread_Safety return Void_Result.Result;

--   Test protected operation contracts
   function Test_Protected_Operation_Contracts return Void_Result.Result;

--   Test performance of protected operations
   function Test_Protected_Object_Performance return Void_Result.Result;

--   Run all concurrent tests
   function Run_All_Tests
     (Output : access Test_Output_Port'Class) return Test_Stats_Result.Result;

end Test_Concurrent_Circuit_Breaker;