--   =============================================================================
--   Test_All - Comprehensive Test Suite Runner
--   =============================================================================
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--
--   Purpose:
--     Main test runner that executes all unit and integration tests for abohlib.
--     Provides comprehensive test coverage reporting.
--   =============================================================================

pragma Ada_2022;
pragma Warnings (Off, "subprogram body has no previous spec");

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
--   with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions;
with Abohlib.Infrastructure.Testing.Test_Framework;
with Abohlib.Infrastructure.Testing.Console_Output;

--   Unit Tests
with Test_File_Path;
with Test_Type_Safe_Generic_Id;
with Test_Result;
with Test_Aggregate_Root;
with Test_Byte_Formatter;
with Test_Domain_Events;
--   with Test_Generic_Repository;  -- Replaced with V2
--   with Test_Unit_Of_Work;       -- Replaced with V2
with Test_Generic_Repository;
with Test_Unit_Of_Work;
with Test_Mock_Usage_Example;
with Test_Constrained_Strings;
with Test_File_Path_Service;
with Test_ULID_Helpers;
with Test_Pipeline_Generic_Stage;
with Test_Optional;
with Test_Safe_Call;

--   New Unit Tests for migrated components
with Test_Lock_Free_Ring_Buffer;
with Test_Lock_Free_Queue_Adapter;
with Test_Generic_Buffered;
with Test_Generic_Zero_Copy;
with Test_Buffer_Manager;
--   with Test_Cancellation_Source; -- Stack overflow issue to investigate
with Test_Signal_Handler;
with Test_Generic_Object_Pool;
with Test_Data;
with Test_Resources;
with Test_State_Machine;

--   Integration Tests
with Test_POSIX_File_System;
with Test_POSIX_System_Info;
--   with Test_Retry_Handler;     -- Replaced with V2
with Test_Retry_Handler;
--   with Test_Concurrent_Circuit_Breaker;

procedure Test_All is

   use Abohlib.Infrastructure.Testing.Test_Framework;
   use Abohlib.Infrastructure.Testing.Console_Output;

   Console : aliased Console_Test_Output;

   type Test_Category is (Unit_Tests, Integration_Tests, All_Tests);

   Category_To_Run : Test_Category := All_Tests;

   Total_Suites : Natural := 0;
   Total_Tests : Natural := 0;
   Total_Passed : Natural := 0;
   Total_Failed : Natural := 0;
   Total_Duration : Duration := 0.0;

--   Procedure specifications
   procedure Print_Header;
   procedure Print_Summary;
   procedure Run_Test_Suite
     (Name : String;
      Runner : access function (Output : access Test_Output_Port'Class)
                               return Test_Stats_Result.Result);
   procedure Run_Unit_Tests;
   procedure Run_Integration_Tests;
   procedure Parse_Arguments;

   procedure Print_Header is
   begin
      Put_Line ("=============================================================================");
      Put_Line ("                        ABOHLIB TEST SUITE RUNNER                            ");
      Put_Line ("=============================================================================");
      Put_Line ("Copyright (c) 2025 A Bit of Help, Inc.");
      Put_Line ("SPDX-License-Identifier: MIT");
      Put_Line ("=============================================================================");
      New_Line;
   end Print_Header;

   procedure Print_Summary is
      Success_Rate : constant Float :=
         (if Total_Tests > 0 then Float (Total_Passed) / Float (Total_Tests) * 100.0 else 0.0);
   begin
      New_Line;
      Put_Line ("=============================================================================");
      Put_Line ("                           OVERALL TEST SUMMARY                              ");
      Put_Line ("=============================================================================");
      Put_Line ("Test Suites Run:    " & Total_Suites'Image);
      Put_Line ("Total Tests:        " & Total_Tests'Image);
      Put_Line ("Tests Passed:       " & Total_Passed'Image);
      Put_Line ("Tests Failed:       " & Total_Failed'Image);
      Put_Line ("Success Rate:       " & Natural (Success_Rate)'Image & "%");
      Put_Line ("Total Duration:     " & Duration'Image (Total_Duration) & " seconds");
      Put_Line ("=============================================================================");

      if Total_Failed = 0 then
         Put_Line ("                         ALL TESTS PASSED                               ");
      else
         Put_Line ("                         SOME TESTS FAILED                              ");
      end if;
      Put_Line ("=============================================================================");
   end Print_Summary;

   procedure Run_Test_Suite
     (Name : String;
      Runner : access function (Output : access Test_Output_Port'Class)
                               return Test_Stats_Result.Result)
   is
      Result : constant Test_Stats_Result.Result := Runner (Console'Access);
   begin
      Total_Suites := Total_Suites + 1;

      if Result.Is_Ok then
         declare
            Stats : constant Test_Statistics := Result.Get_Ok;
         begin
            Total_Tests := Total_Tests + Stats.Total_Tests;
            Total_Passed := Total_Passed + Stats.Passed_Tests;
            Total_Failed := Total_Failed + Stats.Failed_Tests;
            Total_Duration := Total_Duration + Stats.Total_Duration;
         end;
      else
         Put_Line ("ERROR: Failed to run test suite: " & Name);
         Total_Failed := Total_Failed + 1;
      end if;

      New_Line;
   end Run_Test_Suite;

   procedure Run_Unit_Tests is
   begin
      Put_Line (">>> RUNNING UNIT TESTS <<<");
      New_Line;

--   Domain Layer Tests
      Run_Test_Suite ("File_Path", Test_File_Path.Run_All_Tests'Access);
      Run_Test_Suite ("Type_Safe_Generic_Id", Test_Type_Safe_Generic_Id.Run_All_Tests'Access);
      Run_Test_Suite ("Result<T,E>", Test_Result.Run_All_Tests'Access);
      Run_Test_Suite ("Aggregate_Root", Test_Aggregate_Root.Run_All_Tests'Access);
      Run_Test_Suite ("Byte_Formatter", Test_Byte_Formatter.Run_All_Tests'Access);
      Run_Test_Suite ("Domain_Events", Test_Domain_Events.Run_All_Tests'Access);
--   Run_Test_Suite ("Generic_Repository", Test_Generic_Repository.Run_All_Tests'Access);
--   Run_Test_Suite ("Unit_Of_Work", Test_Unit_Of_Work.Run_All_Tests'Access);
      Run_Test_Suite ("Generic_Repository", Test_Generic_Repository.Run_All_Tests'Access);
      Run_Test_Suite ("Unit_Of_Work", Test_Unit_Of_Work.Run_All_Tests'Access);
      Run_Test_Suite ("Constrained_Strings", Test_Constrained_Strings.Run_All_Tests'Access);
      Run_Test_Suite ("File_Path_Service", Test_File_Path_Service.Run_All_Tests'Access);
      Run_Test_Suite ("ULID_Helpers", Test_ULID_Helpers.Run_All_Tests'Access);
      Run_Test_Suite ("Pipeline_Generic_Stage", Test_Pipeline_Generic_Stage.Run_All_Tests'Access);
      Run_Test_Suite ("Optional", Test_Optional.Run_All_Tests'Access);
      Run_Test_Suite ("Safe Call Wrapper", Test_Safe_Call.Run_All_Tests'Access);

--   Concurrent Utilities Tests
      Run_Test_Suite ("Lock-Free Ring Buffer", Test_Lock_Free_Ring_Buffer.Run_All_Tests'Access);
      Run_Test_Suite ("Lock-Free Queue Adapter", Test_Lock_Free_Queue_Adapter.Run_All_Tests'Access);
      Run_Test_Suite ("Concurrent Buffer Manager", Test_Buffer_Manager.Run_All_Tests'Access);
--   Run_Test_Suite ("Cancellation Source", Test_Cancellation_Source.Run_All_Tests'Access); -- Stack overflow issue

--   I/O Utilities Tests
      Run_Test_Suite ("Generic Buffered IO", Test_Generic_Buffered.Run_All_Tests'Access);
      Run_Test_Suite ("Generic Zero-Copy IO", Test_Generic_Zero_Copy.Run_All_Tests'Access);

--   System Utilities Tests
      Run_Test_Suite ("Signal Handler", Test_Signal_Handler.Run_All_Tests'Access);

--   Generic Utilities Tests
      Run_Test_Suite ("Generic Object Pool", Test_Generic_Object_Pool.Run_All_Tests'Access);

--   Testing Framework Enhancement Tests
      Run_Test_Suite ("Test Data", Test_Data.Run_All_Tests'Access);
      Run_Test_Suite ("Test Resources", Test_Resources.Run_All_Tests'Access);

--   State Machine Tests
      Run_Test_Suite ("State Machine", Test_State_Machine.Run_All_Tests'Access);

--   Test Data Factory Tests
--   Run_Test_Suite ("Test Data Factory", Test_Test_Data_Factory.Run_All_Tests'Access);

--   Mock Tests
      Run_Test_Suite ("Mock_Usage_Example", Test_Mock_Usage_Example.Run_All_Tests'Access);
   end Run_Unit_Tests;

   procedure Run_Integration_Tests is
   begin
      Put_Line (">>> RUNNING INTEGRATION TESTS <<<");
      New_Line;

--   Infrastructure Adapter Tests
      Run_Test_Suite ("POSIX_File_System", Test_POSIX_File_System.Run_All_Tests'Access);
      Run_Test_Suite ("POSIX_System_Info", Test_POSIX_System_Info.Run_All_Tests'Access);

--   Resilience Tests
--   Run_Test_Suite ("Retry_Handler", Test_Retry_Handler.Run_All_Tests'Access);
      Run_Test_Suite ("Retry_Handler", Test_Retry_Handler.Run_All_Tests'Access);
--   Run_Test_Suite ("Concurrent_Circuit_Breaker", Test_Concurrent_Circuit_Breaker.Run_All_Tests'Access);
   end Run_Integration_Tests;

   Should_Exit : Boolean := False;

   procedure Parse_Arguments is
   begin
      if Ada.Command_Line.Argument_Count > 0 then
         declare
            Arg : constant String := Ada.Command_Line.Argument (1);
         begin
            if Arg = "--unit" or Arg = "-u" then
               Category_To_Run := Unit_Tests;
            elsif Arg = "--integration" or Arg = "-i" then
               Category_To_Run := Integration_Tests;
            elsif Arg = "--all" or Arg = "-a" then
               Category_To_Run := All_Tests;
            elsif Arg = "--help" or Arg = "-h" then
               Put_Line ("Usage: test_all [options]");
               Put_Line ("Options:");
               Put_Line ("  --unit, -u         Run only unit tests");
               Put_Line ("  --integration, -i  Run only integration tests");
               Put_Line ("  --all, -a          Run all tests (default)");
               Put_Line ("  --help, -h         Show this help message");
               Set_Exit_Status (Success);
               Should_Exit := True;
               return;
            else
               Put_Line ("Unknown option: " & Arg);
               Put_Line ("Use --help for usage information");
               Set_Exit_Status (Failure);
               Should_Exit := True;
               return;
            end if;
         end;
      end if;
   end Parse_Arguments;

begin
   Parse_Arguments;

--   Exit early if help was shown or invalid argument
   if Should_Exit then
      return;
   end if;

   Print_Header;

   case Category_To_Run is
      when Unit_Tests =>
         Run_Unit_Tests;

      when Integration_Tests =>
         Run_Integration_Tests;

      when All_Tests =>
         Run_Unit_Tests;
         Run_Integration_Tests;
   end case;

   Print_Summary;

--   Set exit status based on test results
   if Total_Failed = 0 then
      Set_Exit_Status (Success);
   else
      Set_Exit_Status (Failure);
   end if;

exception
   when E : others =>
      Put_Line ("FATAL ERROR: Unhandled exception in test runner");
      Put_Line ("Exception: " & Ada.Exceptions.Exception_Name (E));
      Put_Line ("Message: " & Ada.Exceptions.Exception_Message (E));
      Set_Exit_Status (Failure);
end Test_All;

pragma Warnings (On, "subprogram body has no previous spec");
