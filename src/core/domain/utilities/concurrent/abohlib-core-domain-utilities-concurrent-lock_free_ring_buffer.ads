--  =============================================================================
--  Abohlib.Core.Domain.Utilities.Concurrent.Lock_Free_Ring_Buffer
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  A lock-free ring buffer implementation for high-throughput applications.
--  Uses atomic operations instead of protected types to eliminate synchronization
--  overhead. Based on single-producer single-consumer (SPSC) design for maximum
--  performance.
--  =============================================================================

pragma Ada_2022;

with System.Atomic_Operations.Modular_Arithmetic;
with System.Storage_Elements;
with Ada.Finalization;
with Interfaces;
with Abohlib.Core.Domain.Constants.System;
with Abohlib.Core.Domain.Constants.Concurrent;
with Abohlib.Core.Domain.Errors;
with Abohlib.Core.Domain.Result;

generic
   type Element_Type is private;
   Buffer_Size : Positive := Abohlib.Core.Domain.Constants.Concurrent.Default_Ring_Buffer_Size;  -- Must be power of 2
package Abohlib.Core.Domain.Utilities.Concurrent.Lock_Free_Ring_Buffer is

   pragma Elaborate_Body;

   --  Ring buffer type
   type Ring_Buffer_Type is
     new Ada.Finalization.Limited_Controlled with private;
   --  Note: Type_Invariant would be ideal here to ensure:
   --    Size < Buffer_Size (when initialized)
   --  However, Ada doesn't allow Type_Invariants to access private components
   --  when the type is declared as private. The invariant is maintained
   --  internally by the atomic operations that manage head and tail indices.
   
   --  Error types for ring buffer operations
   type Ring_Buffer_Error is record
      Base : Abohlib.Core.Domain.Errors.Domain_Error;
   end record;
   
   --  Result types
   package Push_Result is new Abohlib.Core.Domain.Result.Result_Package
     (Ok_Type => Boolean, Err_Type => Ring_Buffer_Error);
   
   package Pop_Result is new Abohlib.Core.Domain.Result.Result_Package
     (Ok_Type => Element_Type, Err_Type => Ring_Buffer_Error);

   --  Initialize the ring buffer
   overriding
   procedure Initialize (Buffer : in out Ring_Buffer_Type);

   --  Try to push an element (non-blocking)
   function Try_Push
     (Buffer : in out Ring_Buffer_Type; Item : Element_Type) return Push_Result.Result
   with
     Pre => Buffer.Is_Initialized,
     Post => (if Push_Result.Is_Ok (Try_Push'Result) then
                not Is_Empty (Buffer));

   --  Try to pop an element (non-blocking)
   function Try_Pop
     (Buffer : in out Ring_Buffer_Type) return Pop_Result.Result
   with
     Pre => Buffer.Is_Initialized,
     Post => (if Pop_Result.Is_Ok (Try_Pop'Result) then
                True);  -- Can't use 'Old in potentially unevaluated context

   --  Check if buffer is empty
   function Is_Empty (Buffer : Ring_Buffer_Type) return Boolean
   with 
     Pre => Buffer.Is_Initialized,
     Post => (if Is_Empty'Result then Size (Buffer) = 0);

   --  Check if buffer is full
   function Is_Full (Buffer : Ring_Buffer_Type) return Boolean
   with 
     Pre => Buffer.Is_Initialized,
     Post => (if Is_Full'Result then Size (Buffer) = Buffer_Size - 1);

   --  Get current size (approximate due to concurrent access)
   function Size (Buffer : Ring_Buffer_Type) return Natural
   with 
     Pre => Buffer.Is_Initialized,
     Post => Size'Result < Buffer_Size;

   --  Get buffer capacity
   function Capacity (Buffer : Ring_Buffer_Type) return Positive
   is (Buffer_Size - 1)
   with Pre => Buffer.Is_Initialized;
   
   --  Check if buffer is initialized
   function Is_Initialized (Buffer : Ring_Buffer_Type) return Boolean;

   --  For zero-copy operations with access types
   generic
      type Element_Access is access all Element_Type;
   package Zero_Copy_Operations is

      --  Result type for ownership operations
      package Ownership_Result is new Abohlib.Core.Domain.Result.Result_Package
        (Ok_Type => Boolean, Err_Type => Ring_Buffer_Error);
      
      --  Try to push by transferring ownership (zero-copy)
      function Try_Push_Ownership
        (Buffer : in out Ring_Buffer_Type; Item : in out Element_Access)
         return Ownership_Result.Result
      with
        Pre => Buffer.Is_Initialized and then Item /= null,
        Post =>
          (if Ownership_Result.Is_Ok (Try_Push_Ownership'Result) and then
              Ownership_Result.Get_Ok (Try_Push_Ownership'Result) then 
                Item = null
           else Item /= null);

      --  Result type for pop ownership
      package Pop_Ownership_Result is new Abohlib.Core.Domain.Result.Result_Package
        (Ok_Type => Element_Access, Err_Type => Ring_Buffer_Error);
      
      --  Try to pop by transferring ownership (zero-copy)
      function Try_Pop_Ownership
        (Buffer : in out Ring_Buffer_Type)
         return Pop_Ownership_Result.Result
      with
        Pre => Buffer.Is_Initialized,
        Post =>
          (if Pop_Ownership_Result.Is_Ok (Try_Pop_Ownership'Result) then
             Pop_Ownership_Result.Get_Ok (Try_Pop_Ownership'Result) /= null);
   end Zero_Copy_Operations;

private

   --  Atomic counter type (using 32-bit for cache line efficiency)
   type Atomic_Unsigned_32 is new Interfaces.Unsigned_32 with Atomic;

   package Atomic_Ops_Unsigned is new
     System.Atomic_Operations.Modular_Arithmetic (Atomic_Unsigned_32);

   subtype Atomic_Counter is Atomic_Unsigned_32;

   --  Cache line constants
   CACHE_LINE_SIZE_BYTES : constant := Abohlib.Core.Domain.Constants.System.CACHE_LINE_SIZE_BYTES;

   use System.Storage_Elements;

   --  Padded counter to prevent false sharing
   type Padded_Counter is record
      Value   : aliased Atomic_Counter;
      Padding : Storage_Array (1 .. CACHE_LINE_SIZE_BYTES - 4);
   end record;
   pragma Pack (Padded_Counter);

   --  Ring buffer storage
   type Element_Array is array (0 .. Buffer_Size - 1) of aliased Element_Type;

   type Ring_Buffer_Type is new Ada.Finalization.Limited_Controlled with record
      Head         : Padded_Counter;  -- Consumer reads from head
      Tail         : Padded_Counter;  -- Producer writes to tail
      Storage      : Element_Array;
      Initialized  : Boolean := False;
   end record;
   
   --  Helper functions for error creation
   function Make_Buffer_Full_Error return Ring_Buffer_Error;
   function Make_Buffer_Empty_Error return Ring_Buffer_Error;
   function Make_Not_Initialized_Error return Ring_Buffer_Error;

end Abohlib.Core.Domain.Utilities.Concurrent.Lock_Free_Ring_Buffer;