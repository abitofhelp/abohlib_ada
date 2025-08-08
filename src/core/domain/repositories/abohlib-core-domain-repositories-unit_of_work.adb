--  =============================================================================
--  Abohlib.Core.Domain.Repositories.Unit_Of_Work - Implementation
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

with Ada.Exceptions;

package body Abohlib.Core.Domain.Repositories.Unit_Of_Work is

   ------------
   -- Create --
   ------------

   function Create return Unit_Of_Work is
   begin
      return UoW : Unit_Of_Work do
         UoW.Start_Time := Ada.Calendar.Clock;
      end return;
   end Create;

   -------------------------
   -- Register_Repository --
   -------------------------

   procedure Register_Repository
     (Self                    : in out Unit_Of_Work;
      Name                    : String;
      Repository              : System.Address;
      Has_Transaction_Support : Boolean := True)
   is
      Registration : constant Repository_Registration :=
        (Name            => To_Unbounded_String (Name),
         Repository      => Repository,
         Has_Transaction => Has_Transaction_Support);
   begin
      Self.Repositories.Insert (To_Unbounded_String (Name), Registration);
   end Register_Repository;

   --------------------
   -- Has_Repository --
   --------------------

   function Has_Repository (Self : Unit_Of_Work; Name : String) return Boolean
   is
   begin
      return Self.Repositories.Contains (To_Unbounded_String (Name));
   end Has_Repository;

   ----------------
   -- Begin_Work --
   ----------------

   procedure Begin_Work (Self : in out Unit_Of_Work) is
   begin
      Self.State := Active;
   end Begin_Work;

   ------------------
   -- Register_New --
   ------------------

   procedure Register_New
     (Self           : in out Unit_Of_Work;
      Aggregate_Type : String;
      Aggregate_Id   : String;
      Data           : String;
      Repository     : String)
   is
      Entry_Data : constant Change_Entry :=
        (Operation       => Insert,
         Aggregate_Type  => To_Unbounded_String (Aggregate_Type),
         Aggregate_Id    => To_Unbounded_String (Aggregate_Id),
         Original_Data   => Null_Unbounded_String,
         Modified_Data   => To_Unbounded_String (Data),
         Repository_Name => To_Unbounded_String (Repository));
   begin
      Self.Changes.Append (Entry_Data);
   end Register_New;

   -----------------------
   -- Register_Modified --
   -----------------------

   procedure Register_Modified
     (Self           : in out Unit_Of_Work;
      Aggregate_Type : String;
      Aggregate_Id   : String;
      Original_Data  : String;
      Modified_Data  : String;
      Repository     : String)
   is
      Entry_Data : constant Change_Entry :=
        (Operation       => Update,
         Aggregate_Type  => To_Unbounded_String (Aggregate_Type),
         Aggregate_Id    => To_Unbounded_String (Aggregate_Id),
         Original_Data   => To_Unbounded_String (Original_Data),
         Modified_Data   => To_Unbounded_String (Modified_Data),
         Repository_Name => To_Unbounded_String (Repository));
   begin
      Self.Changes.Append (Entry_Data);
   end Register_Modified;

   ----------------------
   -- Register_Deleted --
   ----------------------

   procedure Register_Deleted
     (Self           : in out Unit_Of_Work;
      Aggregate_Type : String;
      Aggregate_Id   : String;
      Data           : String;
      Repository     : String)
   is
      Entry_Data : constant Change_Entry :=
        (Operation       => Delete,
         Aggregate_Type  => To_Unbounded_String (Aggregate_Type),
         Aggregate_Id    => To_Unbounded_String (Aggregate_Id),
         Original_Data   => To_Unbounded_String (Data),
         Modified_Data   => Null_Unbounded_String,
         Repository_Name => To_Unbounded_String (Repository));
   begin
      Self.Changes.Append (Entry_Data);
   end Register_Deleted;

   --------------------
   -- Register_Event --
   --------------------

   procedure Register_Event
     (Self  : in out Unit_Of_Work;
      Event : Abohlib.Core.Domain.Events.Domain_Event_Access) is
   begin
      Self.Events.Append (Event);
   end Register_Event;

   ------------
   -- Commit --
   ------------

   function Commit (Self : in out Unit_Of_Work) return UoW_Result.Result is
   begin
      Self.State := Committing;

      --  In a real implementation, this would:
      --  1. Begin transactions on all repositories
      --  2. Apply all changes in order
      --  3. Commit all transactions
      --  4. Publish all domain events
      --  For now, just simulate success

      Self.State := Committed;
      Self.End_Time := Ada.Calendar.Clock;
      return UoW_Result.Ok (True);
   exception
      when E : others =>
         Self.State := Failed;
         return
           UoW_Result.Err
             (To_Unbounded_String
                ("Commit failed: " & Ada.Exceptions.Exception_Message (E)));
   end Commit;

   --------------
   -- Rollback --
   --------------

   function Rollback (Self : in out Unit_Of_Work) return UoW_Result.Result is
   begin
      Self.State := RollingBack;

      --  In a real implementation, this would:
      --  1. Rollback all transactions
      --  2. Clear all tracked changes
      --  3. Clear all events

      Self.Changes.Clear;
      Self.Events.Clear;
      Self.State := RolledBack;
      Self.End_Time := Ada.Calendar.Clock;
      return UoW_Result.Ok (True);
   exception
      when E : others =>
         return
           UoW_Result.Err
             (To_Unbounded_String
                ("Rollback failed: " & Ada.Exceptions.Exception_Message (E)));
   end Rollback;

   -----------------
   -- Get_Changes --
   -----------------

   function Get_Changes (Self : Unit_Of_Work) return Change_List.Vector is
   begin
      return Self.Changes;
   end Get_Changes;

   ----------------
   -- Get_Events --
   ----------------

   function Get_Events (Self : Unit_Of_Work) return Event_List is
   begin
      return Self.Events;
   end Get_Events;

   ---------------
   -- Is_Loaded --
   ---------------

   function Is_Loaded
     (Self : Unit_Of_Work; Aggregate_Id : String) return Boolean is
   begin
      return
        Self.Loaded_Entities.Contains (To_Unbounded_String (Aggregate_Id));
   end Is_Loaded;

   ----------------------
   -- Register_Loaded --
   ----------------------

   procedure Register_Loaded
     (Self : in out Unit_Of_Work; Aggregate_Id : String; Data : String) is
   begin
      Self.Loaded_Entities.Insert
        (To_Unbounded_String (Aggregate_Id), To_Unbounded_String (Data));
   end Register_Loaded;

   ----------------
   -- Get_Loaded --
   ----------------

   function Get_Loaded
     (Self : Unit_Of_Work; Aggregate_Id : String) return String is
   begin
      return
        To_String
          (Self.Loaded_Entities.Element (To_Unbounded_String (Aggregate_Id)));
   end Get_Loaded;

   -------------------
   -- Create_Nested --
   -------------------

   function Create_Nested (Parent : in out Unit_Of_Work) return Unit_Of_Work is
   begin
      return Child : Unit_Of_Work do
         Child.Parent := Parent'Unchecked_Access;
         Child.State := Created;
         Child.Start_Time := Ada.Calendar.Clock;
         --  Copy repository registrations from parent
         Child.Repositories := Parent.Repositories;
      end return;
   end Create_Nested;

   ----------------
   -- Get_Parent --
   ----------------

   function Get_Parent (Self : Unit_Of_Work) return access Unit_Of_Work is
   begin
      return Self.Parent;
   end Get_Parent;

   --------------
   -- Finalize --
   --------------

   overriding
   procedure Finalize (Self : in out Unit_Of_Work) is
   begin
      --  If unit of work is still active when being finalized,
      --  automatically rollback
      if Self.State = Active then
         declare
            Ignored : constant UoW_Result.Result := Self.Rollback;
         begin
            null;
         end;
      end if;
   end Finalize;

end Abohlib.Core.Domain.Repositories.Unit_Of_Work;
