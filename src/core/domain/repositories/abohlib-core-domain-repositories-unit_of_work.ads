--  =============================================================================
--  Abohlib.Core.Domain.Repositories.Unit_Of_Work - Unit of Work Pattern
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Unit of Work pattern for coordinating changes across multiple aggregates
--  and repositories within a single transaction boundary. Ensures consistency
--  across related operations.
--
--  Usage Example:
--  ```ada
--  declare
--     UoW : Unit_Of_Work := Create;
--  begin
--     UoW.Register_Repository ("users", User_Repo'Address);
--     UoW.Begin_Work;
--
--     UoW.Register_New ("User", "123", User_JSON, "users");
--     UoW.Register_Modified ("Order", "456", Old_JSON, New_JSON, "orders");
--
--     if UoW_Result.Is_Ok (UoW.Commit) then
--        Put_Line ("Transaction committed successfully");
--     end if;
--  end;
--  ```
--
--  Features:
--  - Transaction coordination across repositories
--  - Change tracking with operation history
--  - Domain event collection
--  - Nested transactions
--  - Identity map for avoiding duplicate loads
--  - Automatic rollback on failure
--  =============================================================================

pragma Ada_2022;

with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Finalization;
with Ada.Calendar;
with System;
with Abohlib.Core.Domain.Result;
with Abohlib.Core.Domain.Events;

package Abohlib.Core.Domain.Repositories.Unit_Of_Work is
   pragma Elaborate_Body;

   --  Unit of Work state
   type UoW_State is
     (Created,           --  UoW created but not started
      Active,            --  Tracking changes
      Committing,        --  In process of committing
      Committed,         --  Successfully committed
      RollingBack,       --  In process of rolling back
      RolledBack,        --  Successfully rolled back
      Failed);           --  Failed with error

   --  Operation types tracked by UoW
   type Operation_Type is (Insert, Update, Delete);

   --  Change tracking entry
   type Change_Entry is record
      Operation       : Operation_Type;
      Aggregate_Type  : Unbounded_String;
      Aggregate_Id    : Unbounded_String;
      Original_Data   : Unbounded_String;  --  JSON representation
      Modified_Data   : Unbounded_String;  --  JSON representation
      Repository_Name : Unbounded_String;
   end record;

   --  Result types
   package UoW_Result is new
     Abohlib.Core.Domain.Result.Result_Package
       (Ok_Type  => Boolean,
        Err_Type => Unbounded_String);

   --  Collections
   package Change_List is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Change_Entry);

   subtype Event_List is Abohlib.Core.Domain.Events.Event_List;

   --  Repository registration
   type Repository_Registration is record
      Name            : Unbounded_String;
      Repository      : System.Address;  --  Pointer to actual repository
      Has_Transaction : Boolean;
   end record;

   package Repository_Map is new
     Ada.Containers.Ordered_Maps
       (Key_Type     => Unbounded_String,
        Element_Type => Repository_Registration);

   --  Unit of Work interface
   type Unit_Of_Work is new Ada.Finalization.Limited_Controlled with private
   with Type_Invariant => Is_Valid (Unit_Of_Work);

   --  Create new unit of work
   function Create return Unit_Of_Work;

   --  Register a repository with the UoW
   procedure Register_Repository
     (Self                    : in out Unit_Of_Work;
      Name                    : String;
      Repository              : System.Address;
      Has_Transaction_Support : Boolean := True)
   with Pre => Self.Get_State = Created, Post => Self.Has_Repository (Name);

   --  Check if repository is registered
   function Has_Repository (Self : Unit_Of_Work; Name : String) return Boolean;

   --  Begin tracking changes
   procedure Begin_Work (Self : in out Unit_Of_Work)
   with Pre => Self.Get_State = Created, Post => Self.Get_State = Active;

   --  Track entity changes

   --  Register new entity
   procedure Register_New
     (Self           : in out Unit_Of_Work;
      Aggregate_Type : String;
      Aggregate_Id   : String;
      Data           : String;
      --  JSON representation
      Repository     : String)
   with
     Pre => Self.Get_State = Active and then Self.Has_Repository (Repository);

   --  Register modified entity
   procedure Register_Modified
     (Self           : in out Unit_Of_Work;
      Aggregate_Type : String;
      Aggregate_Id   : String;
      Original_Data  : String;
      --  JSON representation
      Modified_Data  : String;
      --  JSON representation
      Repository     : String)
   with
     Pre => Self.Get_State = Active and then Self.Has_Repository (Repository);

   --  Register deleted entity
   procedure Register_Deleted
     (Self           : in out Unit_Of_Work;
      Aggregate_Type : String;
      Aggregate_Id   : String;
      Data           : String;
      --  JSON representation
      Repository     : String)
   with
     Pre => Self.Get_State = Active and then Self.Has_Repository (Repository);

   --  Register domain event
   procedure Register_Event
     (Self  : in out Unit_Of_Work;
      Event : Abohlib.Core.Domain.Events.Domain_Event_Access)
   with Pre => Self.Get_State = Active;

   --  Commit all changes
   function Commit (Self : in out Unit_Of_Work) return UoW_Result.Result
   with
     Pre => Self.Get_State = Active,
     Post =>
       (if Commit'Result.Is_Ok then Self.Get_State = Committed
        else Self.Get_State = Failed);

   --  Rollback all changes
   function Rollback (Self : in out Unit_Of_Work) return UoW_Result.Result
   with
     Pre => Self.Get_State in Active | Failed,
     Post => (if Rollback'Result.Is_Ok then Self.Get_State = RolledBack);

   --  Get current state
   function Get_State (Self : Unit_Of_Work) return UoW_State
   with Inline;

   --  Get tracked changes
   function Get_Changes (Self : Unit_Of_Work) return Change_List.Vector;

   --  Get registered events
   function Get_Events (Self : Unit_Of_Work) return Event_List;

   --  Check if there are pending changes
   function Has_Changes (Self : Unit_Of_Work) return Boolean
   with Inline;

   --  Identity Map support for avoiding duplicate loads

   --  Check if entity is already loaded
   function Is_Loaded
     (Self : Unit_Of_Work; Aggregate_Id : String) return Boolean;

   --  Register loaded entity
   procedure Register_Loaded
     (Self         : in out Unit_Of_Work;
      Aggregate_Id : String;
      Data         : String);  --  JSON representation

   --  Get loaded entity data
   function Get_Loaded
     (Self : Unit_Of_Work; Aggregate_Id : String) return String
   with Pre => Self.Is_Loaded (Aggregate_Id);

   --  Validation
   function Is_Valid (Self : Unit_Of_Work) return Boolean;

   --  Nested Unit of Work support

   --  Create child UoW for nested transactions
   function Create_Nested (Parent : in out Unit_Of_Work) return Unit_Of_Work
   with Pre => Parent.Get_State = Active;

   --  Check if this is a nested UoW
   function Is_Nested (Self : Unit_Of_Work) return Boolean
   with Inline;

   --  Get parent UoW (if nested)
   function Get_Parent (Self : Unit_Of_Work) return access Unit_Of_Work
   with Pre => Self.Is_Nested;

private

   --  Identity map for loaded entities
   package Identity_Map is new
     Ada.Containers.Ordered_Maps
       (Key_Type     => Unbounded_String,
        Element_Type => Unbounded_String);

   type Unit_Of_Work_Access is access all Unit_Of_Work;

   type Unit_Of_Work is new Ada.Finalization.Limited_Controlled with record
      State           : UoW_State := Created;
      Changes         : Change_List.Vector;
      Events          : Event_List;
      Repositories    : Repository_Map.Map;
      Loaded_Entities : Identity_Map.Map;
      Parent          : Unit_Of_Work_Access := null;
      Start_Time      : Ada.Calendar.Time;
      End_Time        : Ada.Calendar.Time;
   end record;

   --  Finalization
   overriding
   procedure Finalize (Self : in out Unit_Of_Work);

   --  Helper functions
   function Get_State (Self : Unit_Of_Work) return UoW_State
   is (Self.State);

   function Has_Changes (Self : Unit_Of_Work) return Boolean
   is (not Self.Changes.Is_Empty or else not Self.Events.Is_Empty);

   function Is_Nested (Self : Unit_Of_Work) return Boolean
   is (Self.Parent /= null);

   function Is_Valid (Self : Unit_Of_Work) return Boolean
   is (case Self.State is
         when Created => Self.Changes.Is_Empty and then Self.Events.Is_Empty,
         when Active => True,
         when Committing | RollingBack => Self.Has_Changes,
         when Committed | RolledBack | Failed => True);

   --  Commit strategy interface for different repository types
   type Commit_Strategy is interface;

   procedure Execute_Changes
     (Strategy : Commit_Strategy; Changes : Change_List.Vector)
   is abstract;

   procedure Handle_Rollback
     (Strategy : Commit_Strategy; Changes : Change_List.Vector)
   is abstract;

end Abohlib.Core.Domain.Repositories.Unit_Of_Work;
