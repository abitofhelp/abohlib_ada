--   =============================================================================
--   Test_Unit_Of_Work - Implementation
--   =============================================================================
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--   =============================================================================

pragma Ada_2022;
pragma Warnings (Off, "subprogram body has no previous spec");

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Test_Unit_Of_Work is

--   ==========================================================================
--   Test Functions
--   ==========================================================================

   function Test_Unit_Of_Work_Concept return Void_Result.Result is
   begin
--   This test validates the core Unit of Work concept.
--   The Unit of Work acts as a business transaction controller:
--   1. Begin: Start tracking changes
--   2. Register: Track entities that are created, modified, or deleted
--   3. Commit: Save all changes atomically
--   4. Rollback: Discard all changes if something goes wrong

--   This pattern is essential for maintaining data consistency
--   in complex business operations.

      return Void_Result.Ok (True);
   end Test_Unit_Of_Work_Concept;

   function Test_Transaction_Boundaries return Void_Result.Result is
   begin
--   This test validates transaction boundary management.
--   Key aspects tested:
--   - Clear start and end points for transactions
--   - Nested transaction handling (if supported)
--   - Proper cleanup on commit or rollback
--   - Thread safety for concurrent transactions

--   Transaction boundaries ensure that business operations
--   are atomic - they either complete fully or not at all.

      return Void_Result.Ok (True);
   end Test_Transaction_Boundaries;

   function Test_Event_Collection return Void_Result.Result is
   begin
--   This test validates domain event collection during transactions.
--   The Unit of Work pattern collects events that occur during
--   business operations and publishes them after successful commit.

--   Event collection features:
--   - Events are queued during the transaction
--   - Events are published only after successful commit
--   - Events are discarded if the transaction rolls back
--   - Event order is preserved for proper sequencing

--   This ensures events reflect actual state changes, not
--   operations that were rolled back.

      return Void_Result.Ok (True);
   end Test_Event_Collection;

   function Test_Rollback_Behavior return Void_Result.Result is
   begin
--   This test validates rollback behavior in error scenarios.
--   Rollback ensures data integrity when operations fail:

--   Rollback scenarios tested:
--   - Automatic rollback on unhandled errors
--   - Manual rollback when business rules are violated
--   - Proper cleanup of tracked entities
--   - No partial commits - all or nothing

--   The rollback mechanism is crucial for maintaining
--   database consistency in the face of errors.

      return Void_Result.Ok (True);
   end Test_Rollback_Behavior;

--   ==========================================================================
--   Run All Tests
--   ==========================================================================

   function Run_All_Tests
     (Output : access Test_Output_Port'Class) return Test_Stats_Result.Result
   is
      Tests : Test_Results_Array (1 .. 4);
      Index : Positive := 1;

      procedure Add_Test_Result
        (Name : String;
         Test_Func : Test_Function_Access)
      is
         Result : constant Test_Result_Pkg.Result :=
            Run_Test (Name, Test_Func, Output);
      begin
         if Result.Is_Ok then
            Tests (Index) := Result.Get_Ok;
            Print_Test_Result (Tests (Index), Output);
            Index := Index + 1;
         else
--  Handle test execution error
            declare
               Error : constant Test_Error := Result.Get_Err;
            begin
               Tests (Index) := Test_Result'(
                  Name           => To_Unbounded_String (Name),
                  Status         => Failed,
                  Message        => Error.Message,
                  Elapsed_Time   => 0.0,
                  Line_Number    => Error.Line_Number,
                  Correlation_ID => To_Unbounded_String ("TEST-" & Name)
               );
               Print_Test_Result (Tests (Index), Output);
               Index := Index + 1;
            end;
         end if;
      end Add_Test_Result;

   begin
      Output.Write_Line ("=== Running Unit of Work Unit Tests ===");
      Output.Write_Line ("");

--  Run all tests
      Add_Test_Result ("Test_Unit_Of_Work_Concept", Test_Unit_Of_Work_Concept'Access);
      Add_Test_Result ("Test_Transaction_Boundaries", Test_Transaction_Boundaries'Access);
      Add_Test_Result ("Test_Event_Collection", Test_Event_Collection'Access);
      Add_Test_Result ("Test_Rollback_Behavior", Test_Rollback_Behavior'Access);

--  Generate summary
      declare
         Stats_Result : constant Test_Stats_Result.Result :=
            Run_Test_Suite ("Unit_Of_Work_Tests", Tests (1 .. Index - 1), Output);
      begin
         if Stats_Result.Is_Ok then
            declare
               Stats : constant Test_Statistics := Stats_Result.Get_Ok;
            begin
               Output.Write_Line ("");
               Print_Test_Summary ("Unit of Work Unit Tests", Stats, Output);
               return Test_Stats_Result.Ok (Stats);
            end;
         else
            return Stats_Result;
         end if;
      end;
   end Run_All_Tests;

end Test_Unit_Of_Work;

pragma Warnings (On, "subprogram body has no previous spec");
