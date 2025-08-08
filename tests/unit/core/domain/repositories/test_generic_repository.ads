--   =============================================================================
--   Test_Generic_Repository - Generic Repository Pattern Unit Tests
--   =============================================================================
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--
--   Purpose:
--     This test suite validates the concepts of the Repository pattern, which
--     provides an abstraction layer between your domain logic and data access.
--
--   What is the Repository Pattern?
--     The Repository pattern encapsulates data access logic, making your code:
--     - Testable: Easy to mock data access for unit tests
--     - Maintainable: Changes to data storage don't affect business logic
--     - Flexible: Can switch between different data stores
--
--   Test Coverage:
--     These tests verify repository concepts including:
--     - Basic CRUD operations (Create, Read, Update, Delete)
--     - Transaction management and consistency
--     - Query operations and filtering
--     - Error handling for data access failures
--
--   Note:
--     This is a simplified test suite that validates the concepts rather than
--     a full implementation, as the actual Generic_Repository requires complex
--     entity type configurations.
--   =============================================================================

pragma Ada_2022;

with Abohlib.Infrastructure.Testing.Test_Framework;

package Test_Generic_Repository is

   use Abohlib.Infrastructure.Testing.Test_Framework;

--   Run all generic repository tests
   --
--   This function executes the complete test suite and returns statistics
--   about test execution including pass/fail counts and timing information.
   function Run_All_Tests
     (Output : access Test_Output_Port'Class) return Test_Stats_Result.Result;

end Test_Generic_Repository;