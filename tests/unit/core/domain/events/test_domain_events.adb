--   =============================================================================
--   Test_Domain_Events - Implementation
--   =============================================================================
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--   =============================================================================

pragma Ada_2022;
pragma Warnings (Off, "subprogram body has no previous spec");

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Calendar;
with Abohlib.Core.Domain.Events;
with Abohlib.Core.Domain.Value_Objects.Type_Safe_Generic_Id;

package body Test_Domain_Events is

   use Abohlib.Core.Domain.Events;

--   ==========================================================================
--   Test Event Types Setup
--   ==========================================================================

--  Define test event categories
   type Order_Event_Category is null record;
   type Customer_Event_Category is null record;

--  Create ID types for testing
   package Order_Id_Pkg is new
      Abohlib.Core.Domain.Value_Objects.Type_Safe_Generic_Id.Generic_ID_Type
        (Category      => Order_Event_Category,
         Category_Name => "Order",
         Prefix        => "ORD");

   package Customer_Id_Pkg is new
      Abohlib.Core.Domain.Value_Objects.Type_Safe_Generic_Id.Generic_ID_Type
        (Category      => Customer_Event_Category,
         Category_Name => "Customer",
         Prefix        => "CUST");

   subtype Order_Id is Order_Id_Pkg.ID;
   subtype Customer_Id is Customer_Id_Pkg.ID;

--  Define concrete event types
   type Order_Created_Event is new Domain_Event with record
      Order_Id_Value    : Order_Id;
      Customer_Id_Value : Customer_Id;
      Total             : Float;
   end record;

   overriding function Event_Name (Event : Order_Created_Event) return String is
      ("OrderCreated");

   type Order_Shipped_Event is new Domain_Event with record
      Order_Id_Value : Order_Id;
      Tracking_Num   : Unbounded_String;
      Carrier        : Unbounded_String;
   end record;

   overriding function Event_Name (Event : Order_Shipped_Event) return String is
      ("OrderShipped");

   type Customer_Registered_Event is new Domain_Event with record
      Customer_Id_Value : Customer_Id;
      Email             : Unbounded_String;
      Name              : Unbounded_String;
   end record;

   overriding function Event_Name (Event : Customer_Registered_Event) return String is
      ("CustomerRegistered");

--   ==========================================================================
--   Test Functions
--   ==========================================================================

   function Test_Event_Initialization return Void_Result.Result is
      Event : Order_Created_Event;
   begin
--  Initialize event
      Initialize_Event (Event);

--  Set event data
      Event.Order_Id_Value := Order_Id_Pkg.New_ID;
      Event.Customer_Id_Value := Customer_Id_Pkg.New_ID;
      Event.Total := 99.99;

--  Verify event ID is set
      if Event.Event_Id = ULID_String'(others => ' ') then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Event ID not initialized"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Event_Initialization")
         ));
      end if;

--  Verify event ID format (should be ULID-like)
      declare
         ID_String : constant String := Event.Event_Id;
      begin
         if ID_String'Length /= 26 then -- ULID is 26 characters
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Event ID wrong length"),
               Details     => To_Unbounded_String ("Length: " & ID_String'Length'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Event_Initialization")
            ));
         end if;
      end;

--  Verify timestamp is set
      declare
         Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
         use Ada.Calendar;
      begin
--  Timestamp should be very close to now (within 1 second)
         if abs (Event.Occurred_At - Now) > 1.0 then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Event timestamp not current"),
               Details     => Null_Unbounded_String,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Event_Initialization")
            ));
         end if;
      end;

      return Void_Result.Ok (True);
   end Test_Event_Initialization;

--  Metadata not supported in Domain_Event - function removed
   function Test_Event_Metadata return Void_Result.Result is
   begin
--  Metadata operations not supported in Domain_Event
--  Test skipped
      return Void_Result.Ok (True);
   end Test_Event_Metadata;

   function Test_Event_Timestamp return Void_Result.Result is
      use Ada.Calendar;
      Event1, Event2 : Order_Created_Event;
      Start_Time : constant Ada.Calendar.Time := Ada.Calendar.Clock;
   begin
--  Initialize first event
      Initialize_Event (Event1);

--  Small delay
      delay 0.1;

--  Initialize second event
      Initialize_Event (Event2);

--  Verify timestamps are different and ordered
      if Event1.Occurred_At >= Event2.Occurred_At then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Event timestamps not ordered"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Event_Timestamp")
         ));
      end if;

--  Verify timestamps are reasonable
      declare
         -- use Ada.Calendar; -- Already used at line 142
      begin
         if Event1.Occurred_At < Start_Time or
            Event2.Occurred_At > Clock + 1.0
         then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Event timestamps out of range"),
               Details     => Null_Unbounded_String,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Event_Timestamp")
            ));
         end if;
      end;

      return Void_Result.Ok (True);
   end Test_Event_Timestamp;

   function Test_Event_Correlation_ID return Void_Result.Result is
      Event1, Event2 : Order_Created_Event;
--  Correlation_ID : constant String := "REQ-12345-ABCDE";
   begin
--  Initialize events (correlation ID not supported)
      Initialize_Event (Event1);
      Initialize_Event (Event2);

--  Correlation ID not supported in Domain_Event
--  Test that both events have different IDs
      if Event1.Event_Id = Event2.Event_Id then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Event IDs should be unique"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Event_Correlation_ID")
         ));
      end if;

      return Void_Result.Ok (True);
   end Test_Event_Correlation_ID;

   function Test_Event_List_Operations return Void_Result.Result is
      Events : Event_List;
      Event1, Event2, Event3 : aliased Order_Created_Event;
   begin
--  Initialize events
      Initialize_Event (Event1);
      Event1.Order_Id_Value := Order_Id_Pkg.New_ID;
      Event1.Total := 100.00;

      Initialize_Event (Event2);
      Event2.Order_Id_Value := Order_Id_Pkg.New_ID;
      Event2.Total := 200.00;

      Initialize_Event (Event3);
      Event3.Order_Id_Value := Order_Id_Pkg.New_ID;
      Event3.Total := 300.00;

--  Test empty list
      if Natural (Events.Length) /= 0 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("New list should be empty"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Event_List_Operations")
         ));
      end if;

--  Add events
      Events.Append (new Order_Created_Event'(Event1));
      Events.Append (new Order_Created_Event'(Event2));
      Events.Append (new Order_Created_Event'(Event3));

--  Verify count
      if Natural (Events.Length) /= 3 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Wrong event count"),
            Details     => To_Unbounded_String ("Expected: 3, Got: " & Natural (Events.Length)'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Event_List_Operations")
         ));
      end if;

--  Test iteration
      declare
         Total_Sum : Float := 0.0;
      begin
         for Event of Events loop
            if Event.all in Order_Created_Event'Class then
               declare
                  Order_Event : Order_Created_Event renames Order_Created_Event (Event.all);
               begin
                  Total_Sum := Total_Sum + Order_Event.Total;
               end;
            end if;
         end loop;

         if Total_Sum /= 600.00 then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Wrong total sum from events"),
               Details     => To_Unbounded_String ("Expected: 600.00, Got: " & Total_Sum'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Event_List_Operations")
            ));
         end if;
      end;

--  Test clear
      Events.Clear;
      if Natural (Events.Length) /= 0 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("List not cleared"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Event_List_Operations")
         ));
      end if;

      return Void_Result.Ok (True);
   end Test_Event_List_Operations;

   function Test_Event_Equality return Void_Result.Result is
      Event1, Event2 : Order_Created_Event;
      Same_ID : constant String := "01HQ3PJKXG2Q8S7F5XRJM9Z3AB";
      Same_Time : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      Same_Order_Id : constant Order_Id := Order_Id_Pkg.New_ID;
      Same_Customer_Id : constant Customer_Id := Customer_Id_Pkg.New_ID;
   begin
--  Initialize both events
      Initialize_Event (Event1);
      Initialize_Event (Event2);

--  Set all fields to be identical (simulating deserialization of same event)
      Event1.Event_Id := Same_ID;
      Event1.Occurred_At := Same_Time;
      Event1.Order_Id_Value := Same_Order_Id;
      Event1.Customer_Id_Value := Same_Customer_Id;
      Event1.Total := 123.45;

      Event2.Event_Id := Same_ID;
      Event2.Occurred_At := Same_Time;
      Event2.Order_Id_Value := Same_Order_Id;
      Event2.Customer_Id_Value := Same_Customer_Id;
      Event2.Total := 123.45;

--  Events with all same fields should be equal
      if Event1 /= Event2 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Events with same fields should be equal"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Event_Equality")
         ));
      end if;

--  Change one event's ID
      Initialize_Event (Event2);

--  Now they should be different
      if Event1 = Event2 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Events with different IDs should not be equal"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Event_Equality")
         ));
      end if;

      return Void_Result.Ok (True);
   end Test_Event_Equality;

   function Test_Custom_Event_Types return Void_Result.Result is
      Order_Event : Order_Created_Event;
      Ship_Event : Order_Shipped_Event;
      Customer_Event : Customer_Registered_Event;
   begin
--  Initialize different event types
      Initialize_Event (Order_Event);
      Order_Event.Order_Id_Value := Order_Id_Pkg.New_ID;
      Order_Event.Customer_Id_Value := Customer_Id_Pkg.New_ID;
      Order_Event.Total := 149.99;

      Initialize_Event (Ship_Event);
      Ship_Event.Order_Id_Value := Order_Event.Order_Id_Value;
      Ship_Event.Tracking_Num := To_Unbounded_String ("1Z999AA10123456784");
      Ship_Event.Carrier := To_Unbounded_String ("UPS");

      Initialize_Event (Customer_Event);
      Customer_Event.Customer_Id_Value := Order_Event.Customer_Id_Value;
      Customer_Event.Email := To_Unbounded_String ("customer@example.com");
      Customer_Event.Name := To_Unbounded_String ("John Doe");

--  Verify event names
      if Event_Name (Order_Event) /= "OrderCreated" then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Wrong order event name"),
            Details     => To_Unbounded_String ("Got: " & Event_Name (Order_Event)),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Custom_Event_Types")
         ));
      end if;

      if Event_Name (Ship_Event) /= "OrderShipped" then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Wrong ship event name"),
            Details     => To_Unbounded_String ("Got: " & Event_Name (Ship_Event)),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Custom_Event_Types")
         ));
      end if;

      if Event_Name (Customer_Event) /= "CustomerRegistered" then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Wrong customer event name"),
            Details     => To_Unbounded_String ("Got: " & Event_Name (Customer_Event)),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Custom_Event_Types")
         ));
      end if;

--  Test polymorphism
      declare
         Events : Event_List;
      begin
         Events.Append (new Order_Created_Event'(Order_Event));
         Events.Append (new Order_Shipped_Event'(Ship_Event));
         Events.Append (new Customer_Registered_Event'(Customer_Event));

--  Verify we can iterate and get names
         declare
            Names : array (1 .. 3) of Unbounded_String;
            Index : Natural := 0;
         begin
            for Event of Events loop
               Index := Index + 1;
               Names (Index) := To_Unbounded_String (Event_Name (Event.all));
            end loop;

            if Names (1) /= To_Unbounded_String ("OrderCreated") or
               Names (2) /= To_Unbounded_String ("OrderShipped") or
               Names (3) /= To_Unbounded_String ("CustomerRegistered")
            then
               return Void_Result.Err (Test_Error'(
                  Kind        => Assertion_Failed,
                  Message     => To_Unbounded_String ("Wrong event names in polymorphic list"),
                  Details     => Null_Unbounded_String,
                  Line_Number => 0,
                  Test_Name   => To_Unbounded_String ("Test_Custom_Event_Types")
               ));
            end if;
         end;
      end;

      return Void_Result.Ok (True);
   end Test_Custom_Event_Types;

   function Test_Event_Ordering return Void_Result.Result is
      use Ada.Calendar;
      Events : Event_List;
   begin
--  Create events with small delays to ensure different timestamps
      for I in 1 .. 5 loop
         declare
            Event : aliased Order_Created_Event;
         begin
            Initialize_Event (Event);
            Event.Order_Id_Value := Order_Id_Pkg.New_ID;
            Event.Total := Float (I * 100);
            Events.Append (new Order_Created_Event'(Event));
            delay 0.01; -- Small delay to ensure timestamp ordering
         end;
      end loop;

--  Verify events are in chronological order
      declare
         Prev_Time : Ada.Calendar.Time := Ada.Calendar.Time_Of (1901, 1, 1);
      begin
         for Event of Events loop
            if Event.Occurred_At <= Prev_Time then
               return Void_Result.Err (Test_Error'(
                  Kind        => Assertion_Failed,
                  Message     => To_Unbounded_String ("Events not in chronological order"),
                  Details     => Null_Unbounded_String,
                  Line_Number => 0,
                  Test_Name   => To_Unbounded_String ("Test_Event_Ordering")
               ));
            end if;
            Prev_Time := Event.Occurred_At;
         end loop;
      end;

      return Void_Result.Ok (True);
   end Test_Event_Ordering;

   function Test_Event_List_Memory return Void_Result.Result is
      Events : Event_List;
   begin
--  Add many events
      for I in 1 .. 100 loop
         declare
            Event : aliased Order_Created_Event;
         begin
            Initialize_Event (Event);
            Event.Order_Id_Value := Order_Id_Pkg.New_ID;
            Event.Total := Float (I);
            Events.Append (new Order_Created_Event'(Event));
         end;
      end loop;

--  Verify count
      if Natural (Events.Length) /= 100 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Wrong event count after bulk add"),
            Details     => To_Unbounded_String ("Got: " & Natural (Events.Length)'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Event_List_Memory")
         ));
      end if;

--  Clear should deallocate
      Events.Clear;

      if Natural (Events.Length) /= 0 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Events not cleared"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Event_List_Memory")
         ));
      end if;

      return Void_Result.Ok (True);
   end Test_Event_List_Memory;

   function Test_Event_Attributes return Void_Result.Result is
      Event : Order_Created_Event;
   begin
      Initialize_Event (Event);
      Event.Order_Id_Value := Order_Id_Pkg.New_ID;
      Event.Customer_Id_Value := Customer_Id_Pkg.New_ID;
      Event.Total := 299.99;

--  Metadata operations not supported in Domain_Event
--  Test event attributes directly instead

--  Event should have unique ID
      declare
         Event2 : Order_Created_Event;
      begin
         Initialize_Event (Event2);

         if Event.Event_Id = Event2.Event_Id then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Event IDs should be unique"),
               Details     => Null_Unbounded_String,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Event_Attributes")
            ));
         end if;
      end;

      return Void_Result.Ok (True);
   end Test_Event_Attributes;

--   ==========================================================================
--   Run All Tests
--   ==========================================================================

   function Run_All_Tests
     (Output : access Test_Output_Port'Class) return Test_Stats_Result.Result
   is
      Tests : Test_Results_Array (1 .. 10);
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
      Output.Write_Line ("=== Running Domain Events Unit Tests ===");
      Output.Write_Line ("");

--  Run all tests
      Add_Test_Result ("Test_Event_Initialization", Test_Event_Initialization'Access);
--  Test_Event_Metadata commented out - metadata not supported in Domain_Event
      Add_Test_Result ("Test_Event_Timestamp", Test_Event_Timestamp'Access);
      Add_Test_Result ("Test_Event_Correlation_ID", Test_Event_Correlation_ID'Access);
      Add_Test_Result ("Test_Event_List_Operations", Test_Event_List_Operations'Access);
      Add_Test_Result ("Test_Event_Equality", Test_Event_Equality'Access);
      Add_Test_Result ("Test_Custom_Event_Types", Test_Custom_Event_Types'Access);
      Add_Test_Result ("Test_Event_Ordering", Test_Event_Ordering'Access);
      Add_Test_Result ("Test_Event_List_Memory", Test_Event_List_Memory'Access);
      Add_Test_Result ("Test_Event_Attributes", Test_Event_Attributes'Access);

--  Generate summary
      declare
         Stats_Result : constant Test_Stats_Result.Result :=
            Run_Test_Suite ("Domain_Events_Tests", Tests (1 .. Index - 1), Output);
      begin
         if Stats_Result.Is_Ok then
            declare
               Stats : constant Test_Statistics := Stats_Result.Get_Ok;
            begin
               Output.Write_Line ("");
               Print_Test_Summary ("Domain Events Unit Tests", Stats, Output);
               return Test_Stats_Result.Ok (Stats);
            end;
         else
            return Stats_Result;
         end if;
      end;
   end Run_All_Tests;

end Test_Domain_Events;

pragma Warnings (On, "subprogram body has no previous spec");
