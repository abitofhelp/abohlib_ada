--  =============================================================================
--  Abohlib.Core.Domain.Events - Domain Event Infrastructure
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Purpose:
--    Provides base types and infrastructure for domain events. Domain events
--    represent something that happened in the domain that domain experts care
--    about. They are immutable facts about past occurrences.
--
--  Usage:
--    Extend Domain_Event for specific events in your domain. Events should be
--    immutable and contain all information about what happened.
--
--  Example:
--    type Order_Placed_Event is new Domain_Event with record
--       Order_Id : Order_ID_Type;
--       Customer_Id : Customer_ID_Type;
--       Total_Amount : Money_Type;
--    end record;
--  =============================================================================

pragma Ada_2022;

with Ada.Calendar;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Bounded;

package Abohlib.Core.Domain.Events is
   pragma Elaborate_Body;

   --  Maximum length for event names
   Max_Event_Name_Length : constant := 100;

   package Event_Name_Strings is new
     Ada.Strings.Bounded.Generic_Bounded_Length (Max_Event_Name_Length);

   --  ULID is a 26-character string
   subtype ULID_String is String (1 .. 26);

   --  ==========================================================================
   --  Base Domain Event Type
   --  ==========================================================================

   type Domain_Event is abstract tagged record
      Event_Id      : ULID_String;
      Occurred_At   : Ada.Calendar.Time;
      Event_Version : Positive := 1;
   end record;

   --  Get the name of the event (for serialization/logging)
   function Event_Name (Event : Domain_Event) return String is abstract;

   --  Create a new event with auto-generated ID and timestamp
   procedure Initialize_Event (Event : in out Domain_Event'Class);

   --  ==========================================================================
   --  Event List Container
   --  ==========================================================================

   --  Access type for events to handle indefinite types
   type Domain_Event_Access is access all Domain_Event'Class;

   package Event_Lists is new
     Ada.Containers.Doubly_Linked_Lists (Element_Type => Domain_Event_Access);

   subtype Event_List is Event_Lists.List;

   --  ==========================================================================
   --  Event Handler Interface
   --  ==========================================================================

   generic
      type Event_Type is new Domain_Event with private;
   package Event_Handler is
      type Handler_Type is interface;

      procedure Handle (Handler : Handler_Type; Event : Event_Type)
      is abstract;
   end Event_Handler;

   --  ==========================================================================
   --  Event Dispatcher Interface
   --  ==========================================================================

   type Event_Dispatcher is interface;

   procedure Dispatch
     (Dispatcher : Event_Dispatcher; Event : Domain_Event'Class)
   is abstract;
   procedure Dispatch_All (Dispatcher : Event_Dispatcher; Events : Event_List)
   is abstract;

   --  ==========================================================================
   --  Common Event Types
   --  ==========================================================================

   --  Base type for events that reference an aggregate
   generic
      type Aggregate_ID_Type is private;
   package Aggregate_Events is

      type Aggregate_Event is abstract new Domain_Event with record
         Aggregate_Id      : Aggregate_ID_Type;
         Aggregate_Version : Natural := 0;
      end record;

   end Aggregate_Events;

   --  ==========================================================================
   --  Event Store Interface
   --  ==========================================================================

   type Event_Store is interface;

   procedure Append_Events
     (Store : Event_Store; Stream_Id : String; Events : Event_List)
   is abstract;
   function Get_Events
     (Store : Event_Store; Stream_Id : String) return Event_List
   is abstract;
   function Get_Events_After
     (Store : Event_Store; Stream_Id : String; Version : Natural)
      return Event_List
   is abstract;

end Abohlib.Core.Domain.Events;
