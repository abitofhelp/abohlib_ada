--   =============================================================================
--   Test_Domain_Events - Unit tests for Domain Events
--   =============================================================================
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--
--   Purpose:
--     Unit tests for the Domain Events infrastructure that supports event
--     sourcing and event-driven architecture patterns.
--   =============================================================================

pragma Ada_2022;

with Abohlib.Infrastructure.Testing.Test_Framework;

package Test_Domain_Events is

   use Abohlib.Infrastructure.Testing.Test_Framework;

--   Test suite entry point
   function Run_All_Tests
     (Output : access Test_Output_Port'Class) return Test_Stats_Result.Result;

private

--   Test event initialization
   function Test_Event_Initialization return Void_Result.Result;

--   Test event metadata
   function Test_Event_Metadata return Void_Result.Result;

--   Test event timestamp
   function Test_Event_Timestamp return Void_Result.Result;

--   Test event correlation ID
   function Test_Event_Correlation_ID return Void_Result.Result;

--   Test event list operations
   function Test_Event_List_Operations return Void_Result.Result;

--   Test event equality
   function Test_Event_Equality return Void_Result.Result;

--   Test custom event types
   function Test_Custom_Event_Types return Void_Result.Result;

--   Test event ordering
   function Test_Event_Ordering return Void_Result.Result;

--   Test event list memory management
   function Test_Event_List_Memory return Void_Result.Result;

--   Test event serialization support
   function Test_Event_Attributes return Void_Result.Result;

end Test_Domain_Events;
