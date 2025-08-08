--  =============================================================================
--  Abohlib.Core.Domain.Repositories.Generic_Repository - Generic Repository
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Generic repository interface following the Repository pattern.
--  This provides a reusable interface for any entity type, promoting
--  code reuse and consistency across different repositories.
--
--  Usage Example:
--  ```ada
--  type User is tagged record
--     Id : Natural;
--     Name : Unbounded_String;
--  end record;
--
--  package User_Repository is new Abohlib.Core.Domain.Repositories.Generic_Repository
--    (Entity_Type => User,
--     Id_Type     => Natural);
--  ```
--
--  Features:
--  - Type-safe CRUD operations
--  - Batch operations with parallel support
--  - Specification pattern for queries
--  - Transaction support
--  - Pagination
--  - Ada 2022 parallel iteration
--  =============================================================================

pragma Ada_2022;

with Ada.Containers.Vectors;
with Ada.Iterator_Interfaces;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Abohlib.Core.Domain.Result;

generic
   type Entity_Type is tagged private;
   type Id_Type is private;
   with function "=" (Left, Right : Id_Type) return Boolean is <>;
   pragma Unreferenced ("=");
   with function Image (Id : Id_Type) return String is <>;
   pragma Unreferenced (Image);
   Max_Batch_Size : Positive :=
     1000;  --  Ada 2022: Default for generic parameter
package Abohlib.Core.Domain.Repositories.Generic_Repository is

   --  Repository interface with type invariant
   type Repository_Interface is interface
   with Type_Invariant'Class => Check_Repository_State (Repository_Interface);

   type Repository_Access is access all Repository_Interface'Class;

   --  Ghost function for verification (Ada 2022)
   function Check_Repository_State (Repo : Repository_Interface) return Boolean
   is abstract
   with Ghost;

   --  Result types for repository operations
   package Save_Result is new
     Abohlib.Core.Domain.Result.Result_Package
       (Ok_Type  => Id_Type,
        Err_Type => Unbounded_String);

   package Find_Result is new
     Abohlib.Core.Domain.Result.Result_Package
       (Ok_Type  => Entity_Type,
        Err_Type => Unbounded_String);

   package Delete_Result is new
     Abohlib.Core.Domain.Result.Result_Package
       (Ok_Type  => Boolean,
        Err_Type => Unbounded_String);

   package Entity_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Entity_Type);

   package Find_All_Result is new
     Abohlib.Core.Domain.Result.Result_Package
       (Ok_Type  => Entity_Vectors.Vector,
        Err_Type => Unbounded_String);

   --  Core repository operations with contracts

   --  Save an entity (create or update)
   function Save
     (Self : Repository_Interface; Entity : Entity_Type)
      return Save_Result.Result
   is abstract
   with
     Post'Class =>
       (if Save'Result.Is_Ok then Exists (Self, Save'Result.Get_Ok));

   --  Find an entity by ID
   function Find_By_Id
     (Self : Repository_Interface; Id : Id_Type) return Find_Result.Result
   is abstract
   with Post'Class => (if not Exists (Self, Id) then Find_By_Id'Result.Is_Err);

   --  Find all entities
   function Find_All
     (Self : Repository_Interface) return Find_All_Result.Result
   is abstract
   with
     Post'Class =>
       (if Find_All'Result.Is_Ok
        then Natural (Find_All'Result.Get_Ok.Length) <= Count (Self));

   --  Delete an entity by ID
   function Delete
     (Self : Repository_Interface; Id : Id_Type) return Delete_Result.Result
   is abstract
   with
     Post'Class =>
       (if Delete'Result.Is_Ok and then Delete'Result.Get_Ok
        then not Exists (Self, Id));

   --  Check if entity exists
   function Exists (Self : Repository_Interface; Id : Id_Type) return Boolean
   is abstract;

   --  Get total count
   function Count (Self : Repository_Interface) return Natural is abstract
   with Post'Class => Count'Result >= 0;

   --  Type for arrays of IDs
   type Id_Array is array (Positive range <>) of Id_Type;

   --  Batch operations using Ada 2022 parallel features

   --  Save multiple entities in parallel
   function Save_Batch
     (Self : Repository_Interface; Entities : Entity_Vectors.Vector)
      return Save_Result.Result
   is abstract
   with Pre'Class => Natural (Entities.Length) <= Max_Batch_Size;

   --  Delete multiple entities in parallel
   function Delete_Batch
     (Self : Repository_Interface; Ids : Id_Array) return Delete_Result.Result
   is abstract
   with Pre'Class => Ids'Length <= Max_Batch_Size;

   --  Generic specification pattern support with parallel filtering
   generic
      with function Matches (Entity : Entity_Type) return Boolean;
   function Find_By_Specification
     (Self : Repository_Interface'Class) return Find_All_Result.Result;

   --  Pagination support with contracts
   function Find_Paginated
     (Self     : Repository_Interface;
      Page     : Positive := 1;
      Per_Page : Positive := 20) return Find_All_Result.Result
   is abstract
   with
     Pre'Class => Per_Page <= Max_Batch_Size,
     Post'Class =>
       (if Find_Paginated'Result.Is_Ok
        then Natural (Find_Paginated'Result.Get_Ok.Length) <= Per_Page);

   --  Cursor and iterator types for parallel operations
   type Cursor is private;

   function Has_Element (Position : Cursor) return Boolean;

   package Iterator_Interfaces is new
     Ada.Iterator_Interfaces (Cursor, Has_Element);

   --  Ada 2022: Parallel iteration support
   type Parallel_Iterator is abstract new Repository_Interface with null record
   with
     Constant_Indexing => Element,
     Default_Iterator => Iterate,
     Iterator_Element => Entity_Type;

   function Element
     (Container : Parallel_Iterator; Position : Cursor) return Entity_Type;

   type Iterator_Interface is
     new Iterator_Interfaces.Forward_Iterator with private;

   function Iterate
     (Container : Parallel_Iterator) return Iterator_Interface'Class;

   --  Tampering check function
   function Tampering_With_Cursors_Prohibited
     (Container : Parallel_Iterator) return Boolean;

   No_Element : constant Cursor;

   --  Transaction support using Ada 2022 features
   generic
      type Transaction_Type is limited private;
      with procedure Begin_Transaction (T : out Transaction_Type);
      with procedure Commit (T : in out Transaction_Type);
      with procedure Rollback (T : in out Transaction_Type);
   procedure Execute_In_Transaction
     (Self      : Repository_Interface'Class;
      Operation : not null access procedure);

private
   type Cursor is null record;
   No_Element : constant Cursor := ( null record);

   type Iterator_Interface is new Iterator_Interfaces.Forward_Iterator
   with null record;

   overriding
   function First (Object : Iterator_Interface) return Cursor;
   overriding
   function Next
     (Object : Iterator_Interface; Position : Cursor) return Cursor;

   --  Ada 2022: Use 'Image for debugging
   function Image (C : Cursor) return String;

end Abohlib.Core.Domain.Repositories.Generic_Repository;
