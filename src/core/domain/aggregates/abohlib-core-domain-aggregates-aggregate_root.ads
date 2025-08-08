--  =============================================================================
--  Abohlib.Core.Domain.Aggregates.Aggregate_Root - DDD Aggregate Root Base
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Purpose:
--    Base type for aggregate roots with event sourcing, version control, and
--    transaction support following Domain-Driven Design principles.
--
--  Design:
--    - Aggregates maintain consistency boundaries
--    - Support for event sourcing and version control
--    - Optimistic concurrency control via versioning
--    - Transaction support with change tracking
--    - Result-based error handling (no exceptions in public API)
--  =============================================================================

pragma Ada_2022;

with Ada.Finalization;
with Ada.Calendar;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Abohlib.Core.Domain.Events;
with Abohlib.Core.Domain.Result;

generic
   --  The aggregate's unique identifier type
   type Id_Type is private;

   --  The aggregate's name for error messages
   Aggregate_Name : String;

   --  Function to convert ID to string
   with function Id_To_String (Id : Id_Type) return String;

package Abohlib.Core.Domain.Aggregates.Aggregate_Root
is

   --  ==========================================================================
   --  Error Types
   --  ==========================================================================

   type Aggregate_Error_Kind is
     (Concurrency_Conflict,
      Invalid_State,
      Event_Application_Failed,
      Change_Tracking_Error,
      Validation_Failed);

   type Aggregate_Error is record
      Kind             : Aggregate_Error_Kind;
      Message          : Unbounded_String;
      Aggregate_Type   : Unbounded_String;
      Aggregate_Id     : Unbounded_String;
      Expected_Version : Natural := 0;
      Actual_Version   : Natural := 0;
   end record;

   --  Result types
   package Aggregate_Result is new
     Abohlib.Core.Domain.Result.Result_Package
       (Ok_Type  => Boolean,  -- Dummy type for void operations
        Err_Type => Aggregate_Error);

   --  ==========================================================================
   --  Aggregate Root Type
   --  ==========================================================================

   type Aggregate_Root_Type is abstract
     new Ada.Finalization.Controlled with private
   with
     Type_Invariant'Class =>
       Aggregate_Root_Type.Get_Version >= 0
       and then (Aggregate_Root_Type.Is_New
                 = (Aggregate_Root_Type.Get_Version = 0));

   --  ==========================================================================
   --  Query Methods
   --  ==========================================================================

   function Get_Id (Self : Aggregate_Root_Type) return Id_Type
   with Inline;

   function Get_Version (Self : Aggregate_Root_Type) return Natural
   with Inline;

   function Has_Uncommitted_Events (Self : Aggregate_Root_Type) return Boolean
   with Inline;

   function Get_Uncommitted_Events
     (Self : Aggregate_Root_Type) return Abohlib.Core.Domain.Events.Event_List;

   function Created_At (Self : Aggregate_Root_Type) return Ada.Calendar.Time
   with Inline;

   function Updated_At (Self : Aggregate_Root_Type) return Ada.Calendar.Time
   with Inline;

   function Is_New (Self : Aggregate_Root_Type) return Boolean
   with Inline;

   function Is_Tracking_Changes (Self : Aggregate_Root_Type) return Boolean
   with Inline;

   --  ==========================================================================
   --  Abstract Methods (must be implemented by concrete aggregates)
   --  ==========================================================================

   function Is_Valid (Self : Aggregate_Root_Type) return Boolean is abstract
   with Post'Class => (if Is_Valid'Result then Self.Get_Version >= 0);

   procedure Apply_Event
     (Self  : in out Aggregate_Root_Type;
      Event : Abohlib.Core.Domain.Events.Domain_Event'Class)
   is abstract;

   --  ==========================================================================
   --  State Management
   --  ==========================================================================

   procedure Mark_Events_As_Committed (Self : in out Aggregate_Root_Type)
   with Post => not Self.Has_Uncommitted_Events;

   function Load_From_History
     (Self   : in out Aggregate_Root_Type;
      Events : Abohlib.Core.Domain.Events.Event_List)
      return Aggregate_Result.Result
   with
     Pre => not Events.Is_Empty and then Self.Get_Version = 0,
     Post =>
       (if Aggregate_Result.Is_Ok (Load_From_History'Result)
        then
          Self.Get_Version = Natural (Events.Length)
          and then not Self.Has_Uncommitted_Events);

   --  ==========================================================================
   --  Transaction Support
   --  ==========================================================================

   function Begin_Changes
     (Self : in out Aggregate_Root_Type) return Aggregate_Result.Result
   with
     Pre => not Self.Is_Tracking_Changes,
     Post =>
       (if Aggregate_Result.Is_Ok (Begin_Changes'Result)
        then Self.Is_Tracking_Changes);

   function Commit_Changes
     (Self : in out Aggregate_Root_Type) return Aggregate_Result.Result
   with
     Pre => Self.Is_Tracking_Changes,
     Post =>
       (if Aggregate_Result.Is_Ok (Commit_Changes'Result)
        then not Self.Is_Tracking_Changes);

   function Rollback_Changes
     (Self : in out Aggregate_Root_Type) return Aggregate_Result.Result
   with
     Pre => Self.Is_Tracking_Changes,
     Post =>
       (if Aggregate_Result.Is_Ok (Rollback_Changes'Result)
        then not Self.Is_Tracking_Changes);

   --  ==========================================================================
   --  Protected Operations (for use by concrete aggregates)
   --  ==========================================================================

   procedure Raise_Event
     (Self  : in out Aggregate_Root_Type'Class;
      Event : Abohlib.Core.Domain.Events.Domain_Event'Class)
   with Post => Self.Has_Uncommitted_Events;

   procedure Increment_Version (Self : in out Aggregate_Root_Type)
   with
     Pre => Self.Get_Version < Natural'Last,
     Post => Self.Get_Version = Self.Get_Version'Old + 1;

   function Set_Id
     (Self : in out Aggregate_Root_Type; Id : Id_Type)
      return Aggregate_Result.Result
   with Pre => Self.Is_New;

private

   type Change_Tracker is record
      Original_Version     : Natural := 0;
      Events_Before_Change : Abohlib.Core.Domain.Events.Event_List;
   end record;

   type Change_Tracker_Access is access Change_Tracker;

   type Aggregate_Root_Type is abstract new Ada.Finalization.Controlled
   with record
      Id                 : Id_Type;
      Version            : Natural := 0;
      Created_Timestamp  : Ada.Calendar.Time := Ada.Calendar.Clock;
      Updated_Timestamp  : Ada.Calendar.Time := Ada.Calendar.Clock;
      Uncommitted_Events : Abohlib.Core.Domain.Events.Event_List;
      Change_Tracking    : Change_Tracker_Access := null;
   end record;

   --  Controlled type operations
   overriding
   procedure Initialize (Self : in out Aggregate_Root_Type);
   overriding
   procedure Finalize (Self : in out Aggregate_Root_Type);
   overriding
   procedure Adjust (Self : in out Aggregate_Root_Type);

   --  Inline implementations
   function Get_Id (Self : Aggregate_Root_Type) return Id_Type
   is (Self.Id);
   function Get_Version (Self : Aggregate_Root_Type) return Natural
   is (Self.Version);
   function Has_Uncommitted_Events (Self : Aggregate_Root_Type) return Boolean
   is (not Self.Uncommitted_Events.Is_Empty);
   function Created_At (Self : Aggregate_Root_Type) return Ada.Calendar.Time
   is (Self.Created_Timestamp);
   function Updated_At (Self : Aggregate_Root_Type) return Ada.Calendar.Time
   is (Self.Updated_Timestamp);
   function Is_New (Self : Aggregate_Root_Type) return Boolean
   is (Self.Version = 0);
   function Is_Tracking_Changes (Self : Aggregate_Root_Type) return Boolean
   is (Self.Change_Tracking /= null);

end Abohlib.Core.Domain.Aggregates.Aggregate_Root;
