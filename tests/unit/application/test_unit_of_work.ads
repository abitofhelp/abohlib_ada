--   =============================================================================
--   Test_Unit_Of_Work - Unit of Work Pattern Unit Tests
--   =============================================================================
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--
--   Purpose:
--     This test suite validates the Unit of Work pattern, which maintains a
--     list of objects affected by a business transaction and coordinates
--     writing out changes.
--
--   What is the Unit of Work Pattern?
--     The Unit of Work pattern:
--     - Tracks changes to entities during a business transaction
--     - Ensures all changes are saved together (or not at all)
--     - Collects domain events to be published after successful commit
--     - Provides a transaction boundary for your business operations
--
--   Key Benefits:
--     - Consistency: All changes succeed or fail together
--     - Performance: Batches database operations
--     - Event Handling: Publishes events only after successful persistence
--     - Simplicity: Clear transaction boundaries in your code
--
--   Test Coverage:
--     These tests verify Unit of Work concepts including:
--     - Transaction lifecycle (begin, commit, rollback)
--     - Change tracking for entities
--     - Domain event collection and publishing
--     - Error handling and rollback scenarios
--   =============================================================================

pragma Ada_2022;

with Abohlib.Infrastructure.Testing.Test_Framework;

package Test_Unit_Of_Work is

   use Abohlib.Infrastructure.Testing.Test_Framework;

--   Run all unit of work tests
   --
--   Executes the complete test suite for Unit of Work pattern validation
--   and returns comprehensive test statistics.
   function Run_All_Tests
     (Output : access Test_Output_Port'Class) return Test_Stats_Result.Result;

end Test_Unit_Of_Work;