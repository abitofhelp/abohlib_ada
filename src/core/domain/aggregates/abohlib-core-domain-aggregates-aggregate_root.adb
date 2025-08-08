--  =============================================================================
--  Abohlib.Core.Domain.Aggregates.Aggregate_Root - Implementation
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

with Ada.Unchecked_Deallocation;
with Ada.Exceptions;

package body Abohlib.Core.Domain.Aggregates.Aggregate_Root is

   procedure Free is new
     Ada.Unchecked_Deallocation (Change_Tracker, Change_Tracker_Access);

   --  ==========================================================================
   --  Error Creation Helpers
   --  ==========================================================================

   function Create_Aggregate_Error
     (Kind : Aggregate_Error_Kind; Message : String; Id : Id_Type)
      return Aggregate_Error is
   begin
      return
        Aggregate_Error'
          (Kind             => Kind,
           Message          => To_Unbounded_String (Message),
           Aggregate_Type   => To_Unbounded_String (Aggregate_Name),
           Aggregate_Id     => To_Unbounded_String (Id_To_String (Id)),
           Expected_Version => 0,
           Actual_Version   => 0);
   end Create_Aggregate_Error;

   --  ==========================================================================
   --  Query Methods
   --  ==========================================================================

   function Get_Uncommitted_Events
     (Self : Aggregate_Root_Type) return Abohlib.Core.Domain.Events.Event_List
   is
   begin
      return Self.Uncommitted_Events;
   end Get_Uncommitted_Events;

   --  ==========================================================================
   --  State Management
   --  ==========================================================================

   procedure Mark_Events_As_Committed (Self : in out Aggregate_Root_Type) is
   begin
      Self.Uncommitted_Events.Clear;
   end Mark_Events_As_Committed;

   function Load_From_History
     (Self   : in out Aggregate_Root_Type;
      Events : Abohlib.Core.Domain.Events.Event_List)
      return Aggregate_Result.Result is
   begin
      -- Clear current state
      Self.Version := 0;
      Self.Uncommitted_Events.Clear;

      -- Replay all events
      begin
         for Event of Events loop
            Aggregate_Root_Type'Class (Self).Apply_Event (Event.all);
            Self.Version := Self.Version + 1;
         end loop;
      exception
         when E : others =>
            return
              Aggregate_Result.Err
                (Create_Aggregate_Error
                   (Kind    => Event_Application_Failed,
                    Message =>
                      "Failed to apply event: "
                      & Ada.Exceptions.Exception_Message (E),
                    Id      => Self.Id));
      end;

      -- Clear uncommitted events since these are historical
      Self.Uncommitted_Events.Clear;

      -- Validate final state
      if not Aggregate_Root_Type'Class (Self).Is_Valid then
         return
           Aggregate_Result.Err
             (Create_Aggregate_Error
                (Kind    => Invalid_State,
                 Message => "Aggregate state invalid after loading history",
                 Id      => Self.Id));
      end if;

      return Aggregate_Result.Ok (True);
   end Load_From_History;

   --  ==========================================================================
   --  Transaction Support
   --  ==========================================================================

   function Begin_Changes
     (Self : in out Aggregate_Root_Type) return Aggregate_Result.Result is
   begin
      if Self.Change_Tracking /= null then
         return
           Aggregate_Result.Err
             (Create_Aggregate_Error
                (Kind    => Change_Tracking_Error,
                 Message => "Already tracking changes",
                 Id      => Self.Id));
      end if;

      Self.Change_Tracking :=
        new Change_Tracker'
          (Original_Version     => Self.Version,
           Events_Before_Change => Self.Uncommitted_Events);

      return Aggregate_Result.Ok (True);
   end Begin_Changes;

   function Commit_Changes
     (Self : in out Aggregate_Root_Type) return Aggregate_Result.Result is
   begin
      if Self.Change_Tracking = null then
         return
           Aggregate_Result.Err
             (Create_Aggregate_Error
                (Kind    => Change_Tracking_Error,
                 Message => "Not tracking changes",
                 Id      => Self.Id));
      end if;

      Free (Self.Change_Tracking);
      return Aggregate_Result.Ok (True);
   end Commit_Changes;

   function Rollback_Changes
     (Self : in out Aggregate_Root_Type) return Aggregate_Result.Result is
   begin
      if Self.Change_Tracking = null then
         return
           Aggregate_Result.Err
             (Create_Aggregate_Error
                (Kind    => Change_Tracking_Error,
                 Message => "Not tracking changes",
                 Id      => Self.Id));
      end if;

      -- Restore original state
      Self.Version := Self.Change_Tracking.Original_Version;
      Self.Uncommitted_Events := Self.Change_Tracking.Events_Before_Change;

      Free (Self.Change_Tracking);
      return Aggregate_Result.Ok (True);
   end Rollback_Changes;

   --  ==========================================================================
   --  Protected Operations
   --  ==========================================================================

   procedure Raise_Event
     (Self  : in out Aggregate_Root_Type'Class;
      Event : Abohlib.Core.Domain.Events.Domain_Event'Class)
   is
      Event_Copy : constant Abohlib.Core.Domain.Events.Domain_Event_Access :=
        new Abohlib.Core.Domain.Events.Domain_Event'Class'(Event);
   begin
      -- Add to uncommitted events
      Self.Uncommitted_Events.Append (Event_Copy);

      -- Apply the event to update state
      Self.Apply_Event (Event);

      -- Update timestamp
      Self.Updated_Timestamp := Ada.Calendar.Clock;
   end Raise_Event;

   procedure Increment_Version (Self : in out Aggregate_Root_Type) is
   begin
      Self.Version := Self.Version + 1;
   end Increment_Version;

   function Set_Id
     (Self : in out Aggregate_Root_Type; Id : Id_Type)
      return Aggregate_Result.Result is
   begin
      if not Self.Is_New then
         return
           Aggregate_Result.Err
             (Create_Aggregate_Error
                (Kind    => Invalid_State,
                 Message => "Cannot change ID of existing aggregate",
                 Id      => Self.Id));
      end if;

      Self.Id := Id;
      return Aggregate_Result.Ok (True);
   end Set_Id;

   --  ==========================================================================
   --  Controlled Type Operations
   --  ==========================================================================

   overriding
   procedure Initialize (Self : in out Aggregate_Root_Type) is
   begin
      Self.Created_Timestamp := Ada.Calendar.Clock;
      Self.Updated_Timestamp := Self.Created_Timestamp;
   end Initialize;

   overriding
   procedure Finalize (Self : in out Aggregate_Root_Type) is
   begin
      if Self.Change_Tracking /= null then
         Free (Self.Change_Tracking);
      end if;

      -- Clean up event list
      for Event of Self.Uncommitted_Events loop
         declare
            procedure Free_Event is new
              Ada.Unchecked_Deallocation
                (Abohlib.Core.Domain.Events.Domain_Event'Class,
                 Abohlib.Core.Domain.Events.Domain_Event_Access);
         begin
            Free_Event (Event);
         end;
      end loop;
      Self.Uncommitted_Events.Clear;
   end Finalize;

   overriding
   procedure Adjust (Self : in out Aggregate_Root_Type) is
   begin
      -- Deep copy change tracking if present
      if Self.Change_Tracking /= null then
         Self.Change_Tracking := new Change_Tracker'(Self.Change_Tracking.all);
      end if;

      -- Deep copy events
      declare
         Old_Events : constant Abohlib.Core.Domain.Events.Event_List :=
           Self.Uncommitted_Events;
      begin
         Self.Uncommitted_Events.Clear;
         for Event of Old_Events loop
            Self.Uncommitted_Events.Append
              (new Abohlib.Core.Domain.Events.Domain_Event'Class'(Event.all));
         end loop;
      end;
   end Adjust;

end Abohlib.Core.Domain.Aggregates.Aggregate_Root;
