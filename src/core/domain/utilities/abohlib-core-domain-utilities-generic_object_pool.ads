--  =============================================================================
--  Abohlib.Core.Domain.Utilities.Generic_Object_Pool - Generic Object Pool
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================
--  Purpose:
--    Provides a generic object pool pattern implementation that can be
--    instantiated for any type to reduce memory allocation overhead.
--
--  Benefits:
--    - Reduced memory allocations
--    - Better cache locality
--    - Predictable memory usage
--    - Configurable pool sizes
--
--  Usage Pattern:
--    1. Instantiate the generic for your type
--    2. Get objects from pool when needed
--    3. Return objects when done
--    4. Pool automatically manages cleanup
--  =============================================================================

pragma Ada_2022;

with Ada.Containers.Vectors;
with Ada.Finalization;
with Abohlib.Core.Domain.Result;
with Abohlib.Core.Domain.Errors;
with Abohlib.Core.Domain.Types.Counts;

generic
   type Object_Type is private;
   type Object_Access is access all Object_Type;
   
   --  User-provided reset procedure to clean objects before reuse
   with procedure Reset_Object (Obj : in out Object_Type) is <>;
   
   --  Pool configuration
   Initial_Size : Positive := 10;
   Max_Size : Positive := 100;
   
package Abohlib.Core.Domain.Utilities.Generic_Object_Pool is

   use Abohlib.Core.Domain.Types.Counts;
   use Abohlib.Core.Domain.Errors;

   --  ==========================================================================
   --  Type Declarations
   --  ==========================================================================

   type Object_Pool is new Ada.Finalization.Limited_Controlled with private;
   --  Note: Type_Invariant would be ideal here to ensure:
   --    Allocated_Total <= Max_Size and
   --    Available_Count <= Allocated_Total
   --  However, Ada doesn't allow Type_Invariants to access private components
   --  when the type is declared as private. These invariants are maintained
   --  internally by the implementation through careful design of all operations.

   --  Result types for pool operations
   package Pool_Result is new Abohlib.Core.Domain.Result.Result_Package
     (Ok_Type  => Object_Access,
      Err_Type => Resource_Error_Type);

   package Count_Result is new Abohlib.Core.Domain.Result.Result_Package
     (Ok_Type  => Element_Count_Type,
      Err_Type => Resource_Error_Type);

   --  ==========================================================================
   --  Pool Operations
   --  ==========================================================================

   --  Get an object from the pool (or allocate new if pool is empty)
   function Get
     (Pool : in out Object_Pool) return Pool_Result.Result
   with
     Post => (if Pool_Result.Is_Ok (Get'Result) then
                Pool_Result.Get_Ok (Get'Result) /= null);

   --  Return an object to the pool for reuse
   procedure Return_To_Pool
     (Pool : in out Object_Pool;
      Obj  : in out Object_Access)
   with
     Pre  => Obj /= null,
     Post => Obj = null;  -- Nullifies the access after return

   --  ==========================================================================
   --  Pool Status Operations
   --  ==========================================================================

   --  Get number of available objects in pool
   function Available_Count
     (Pool : Object_Pool) return Element_Count_Type
   with
     Post => Available_Count'Result <= Element_Count_Type (Max_Size);

   --  Get total number of objects allocated by pool
   function Allocated_Count
     (Pool : Object_Pool) return Element_Count_Type
   with
     Post => Allocated_Count'Result <= Element_Count_Type (Max_Size);

   --  Check if pool has available objects
   function Has_Available
     (Pool : Object_Pool) return Boolean
   with
     Post => Has_Available'Result = (Available_Count (Pool) > 0);

   --  Get pool capacity
   function Capacity return Element_Count_Type
   with
     Post => Capacity'Result = Element_Count_Type (Max_Size);

   --  ==========================================================================
   --  Pool Management
   --  ==========================================================================

   --  Pre-allocate objects to specified count
   function Preallocate
     (Pool  : in out Object_Pool;
      Count : Element_Count_Type) return Count_Result.Result
   with
     Pre  => Count > 0 and then Natural (Count) <= Max_Size,
     Post => (if Count_Result.Is_Ok (Preallocate'Result) then
                Allocated_Count (Pool) >= Count);

   --  Clear all available objects from pool (deallocates them)
   procedure Clear_Available (Pool : in out Object_Pool)
   with
     Post => Available_Count (Pool) = 0;

private

   --  ==========================================================================
   --  Private Type Declarations
   --  ==========================================================================

   package Object_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Object_Access);

   type Object_Pool is new Ada.Finalization.Limited_Controlled with record
      Available       : Object_Vectors.Vector;
      Allocated_Total : Element_Count_Type := 0;
      High_Water_Mark : Element_Count_Type := 0;  -- Track max allocation
   end record;

   --  RAII cleanup
   overriding procedure Initialize (Pool : in out Object_Pool);
   overriding procedure Finalize (Pool : in out Object_Pool);

end Abohlib.Core.Domain.Utilities.Generic_Object_Pool;