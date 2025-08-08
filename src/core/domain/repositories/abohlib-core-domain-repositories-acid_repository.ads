--  =============================================================================
--  Abohlib.Core.Domain.Repositories.ACID_Repository - ACID Repository Pattern
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Purpose:
--    ACID-compliant repository pattern supporting transactions, aggregates, and
--    different storage backends (SQL with transactions, document stores with
--    single-document atomicity, distributed systems with sagas).
--
--  Design:
--    - Atomicity: All changes within a transaction succeed or fail together
--    - Consistency: Data integrity rules are maintained
--    - Isolation: Concurrent transactions don't interfere
--    - Durability: Committed changes persist
--    - Result-based error handling (no exceptions in public API)
--  =============================================================================

pragma Ada_2022;

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Abohlib.Core.Domain.Result;
with Abohlib.Core.Domain.Events;

generic
   --  The aggregate root type
   type Aggregate_Type is tagged private;

   --  The aggregate's ID type
   type Id_Type is private;

   --  Aggregate name for error messages
   Aggregate_Name : String;

   --  ID operations
   with function "=" (Left, Right : Id_Type) return Boolean is <>;
   with function Id_To_String (Id : Id_Type) return String;

   --  Aggregate operations
   with function Get_Id (Aggregate : Aggregate_Type) return Id_Type is <>;
   with function Get_Version (Aggregate : Aggregate_Type) return Natural is <>;

package Abohlib.Core.Domain.Repositories.ACID_Repository
is

   --  ==========================================================================
   --  Error Types
   --  ==========================================================================

   type Repository_Error_Kind is
     (Concurrency_Conflict,
      Not_Found,
      Duplicate_Key,
      Transaction_Failed,
      Connection_Failed,
      Constraint_Violation,
      Serialization_Error,
      Storage_Full,
      Invalid_State);

   type Repository_Error is record
      Kind             : Repository_Error_Kind;
      Message          : Unbounded_String;
      Entity_Id        : Unbounded_String;
      Details          : Unbounded_String;
      Expected_Version : Natural := 0;
      Actual_Version   : Natural := 0;
   end record;

   --  ==========================================================================
   --  Result Types
   --  ==========================================================================

   package Save_Result is new
     Abohlib.Core.Domain.Result.Result_Package
       (Ok_Type  => Id_Type,
        Err_Type => Repository_Error);

   package Load_Result is new
     Abohlib.Core.Domain.Result.Result_Package
       (Ok_Type  => Aggregate_Type,
        Err_Type => Repository_Error);

   package Delete_Result is new
     Abohlib.Core.Domain.Result.Result_Package
       (Ok_Type  => Boolean,
        Err_Type => Repository_Error);

   package Transaction_Result is new
     Abohlib.Core.Domain.Result.Result_Package
       (Ok_Type  => Boolean,
        Err_Type => Repository_Error);

   --  Collection types
   package Aggregate_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Aggregate_Type);

   subtype Aggregate_List is Aggregate_Vectors.Vector;

   package List_Result is new
     Abohlib.Core.Domain.Result.Result_Package
       (Ok_Type  => Aggregate_List,
        Err_Type => Repository_Error);

   --  ID array type
   type Id_Array is array (Positive range <>) of Id_Type;

   --  Event result type
   package Event_Result is new
     Abohlib.Core.Domain.Result.Result_Package
       (Ok_Type  => Abohlib.Core.Domain.Events.Event_List,
        Err_Type => Repository_Error);

   --  ==========================================================================
   --  Transaction Support
   --  ==========================================================================

   type Isolation_Level is
     (Read_Uncommitted,    -- Dirty reads allowed
      Read_Committed,      -- No dirty reads
      Repeatable_Read,     -- No dirty/non-repeatable reads
      Serializable);       -- Full isolation

   type Transaction_State is (None, Active, Committed, Rolled_Back);

   --  ==========================================================================
   --  ACID Repository Interface
   --  ==========================================================================

   type ACID_Repository_Interface is interface;

   --  ==========================================================================
   --  Core ACID Operations
   --  ==========================================================================

   function Save
     (Self : ACID_Repository_Interface; Aggregate : Aggregate_Type)
      return Save_Result.Result
   is abstract
   with
     Post'Class =>
       (if Save_Result.Is_Ok (Save'Result)
        then Self.Exists (Save_Result.Get_Ok (Save'Result)));

   function Load
     (Self : ACID_Repository_Interface; Id : Id_Type) return Load_Result.Result
   is abstract;

   function Delete
     (Self : ACID_Repository_Interface; Id : Id_Type)
      return Delete_Result.Result
   is abstract
   with
     Post'Class =>
       (if Delete_Result.Is_Ok (Delete'Result)
          and then Delete_Result.Get_Ok (Delete'Result)
        then not Self.Exists (Id));

   function Exists
     (Self : ACID_Repository_Interface; Id : Id_Type) return Boolean
   is abstract;

   --  ==========================================================================
   --  Transaction Management
   --  ==========================================================================

   function Begin_Transaction
     (Self      : ACID_Repository_Interface;
      Isolation : Isolation_Level := Read_Committed)
      return Transaction_Result.Result
   is abstract
   with
     Pre'Class =>
       not Self.In_Transaction and then Self.Get_Transaction_State = None,
     Post'Class =>
       (if Transaction_Result.Is_Ok (Begin_Transaction'Result)
        then Self.In_Transaction and then Self.Get_Transaction_State = Active);

   function Commit
     (Self : ACID_Repository_Interface) return Transaction_Result.Result
   is abstract
   with
     Pre'Class =>
       Self.In_Transaction and then Self.Get_Transaction_State = Active,
     Post'Class =>
       (if Transaction_Result.Is_Ok (Commit'Result)
        then
          not Self.In_Transaction
          and then Self.Get_Transaction_State = Committed);

   function Rollback
     (Self : ACID_Repository_Interface) return Transaction_Result.Result
   is abstract
   with
     Pre'Class =>
       Self.In_Transaction and then Self.Get_Transaction_State = Active,
     Post'Class =>
       (if Transaction_Result.Is_Ok (Rollback'Result)
        then
          not Self.In_Transaction
          and then Self.Get_Transaction_State = Rolled_Back);

   function In_Transaction (Self : ACID_Repository_Interface) return Boolean
   is abstract;

   function Get_Transaction_State
     (Self : ACID_Repository_Interface) return Transaction_State
   is abstract;

   --  ==========================================================================
   --  Batch Operations
   --  ==========================================================================

   function Save_All
     (Self : ACID_Repository_Interface; Aggregates : Aggregate_List)
      return Save_Result.Result
   is abstract
   with
     Pre'Class =>
       Natural (Aggregates.Length) > 0
       and then (for all A of Aggregates => Get_Version (A) >= 0),
     Post'Class =>
       (if Save_Result.Is_Ok (Save_All'Result)
        then (for all A of Aggregates => Self.Exists (Get_Id (A))));

   function Load_All
     (Self : ACID_Repository_Interface; Ids : Id_Array)
      return List_Result.Result
   is abstract
   with Pre'Class => Ids'Length > 0;

   function Delete_All
     (Self : ACID_Repository_Interface; Ids : Id_Array)
      return Delete_Result.Result
   is abstract
   with Pre'Class => Ids'Length > 0;

   --  ==========================================================================
   --  Event Sourcing Support
   --  ==========================================================================

   function Save_Events
     (Self    : ACID_Repository_Interface;
      Id      : Id_Type;
      Events  : Abohlib.Core.Domain.Events.Event_List;
      Version : Natural) return Save_Result.Result
   is abstract
   with Pre'Class => not Events.Is_Empty and then Version >= 0;

   function Load_Events
     (Self : ACID_Repository_Interface; Id : Id_Type)
      return Event_Result.Result
   is abstract;

   --  ==========================================================================
   --  Snapshot Support
   --  ==========================================================================

   function Save_Snapshot
     (Self : ACID_Repository_Interface; Aggregate : Aggregate_Type)
      return Save_Result.Result
   is abstract;

   function Load_Snapshot
     (Self : ACID_Repository_Interface; Id : Id_Type) return Load_Result.Result
   is abstract;

   --  ==========================================================================
   --  Query Support
   --  ==========================================================================

   --  Specification pattern for queries
   generic
      with function Matches (Aggregate : Aggregate_Type) return Boolean;
   function Find_By_Specification
     (Self : ACID_Repository_Interface'Class) return List_Result.Result;

   --  ==========================================================================
   --  Transaction Helper
   --  ==========================================================================

   generic
      type Operation_Result is private;
      with
        function Execute
          (Repo : ACID_Repository_Interface'Class) return Operation_Result;
      with function Is_Success (Result : Operation_Result) return Boolean;
   function Execute_In_Transaction
     (Self      : ACID_Repository_Interface'Class;
      Isolation : Isolation_Level := Read_Committed) return Operation_Result;

end Abohlib.Core.Domain.Repositories.ACID_Repository;
