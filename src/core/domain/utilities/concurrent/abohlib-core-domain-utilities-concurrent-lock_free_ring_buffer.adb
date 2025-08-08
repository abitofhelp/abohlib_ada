--  =============================================================================
--  Abohlib.Core.Domain.Utilities.Concurrent.Lock_Free_Ring_Buffer - Implementation
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

with Interfaces; use Interfaces;
with Ada.Unchecked_Conversion;

package body Abohlib.Core.Domain.Utilities.Concurrent.Lock_Free_Ring_Buffer is

   use Abohlib.Core.Domain.Errors;

   --  Initialize the ring buffer
   overriding
   procedure Initialize (Buffer : in out Ring_Buffer_Type) is
   begin
      Buffer.Head.Value := 0;
      Buffer.Tail.Value := 0;
      Buffer.Initialized := True;
   end Initialize;
   
   --  Check if buffer is initialized
   function Is_Initialized (Buffer : Ring_Buffer_Type) return Boolean is
     (Buffer.Initialized);
   
   --  Helper functions for error creation
   function Make_Buffer_Full_Error return Ring_Buffer_Error is
   begin
      return Ring_Buffer_Error'(
         Base => Make_Resource_Error(
            Kind => Exhausted,
            Resource_Type => "Ring Buffer",
            Message => "Ring buffer is full",
            Recovery => "Try again later or increase buffer size"
         ).Base
      );
   end Make_Buffer_Full_Error;
   
   function Make_Buffer_Empty_Error return Ring_Buffer_Error is
   begin
      return Ring_Buffer_Error'(
         Base => Make_Resource_Error(
            Kind => Not_Found,
            Resource_Type => "Ring Buffer",
            Message => "Ring buffer is empty",
            Recovery => "Wait for data to be pushed"
         ).Base
      );
   end Make_Buffer_Empty_Error;
   
   function Make_Not_Initialized_Error return Ring_Buffer_Error is
   begin
      return Ring_Buffer_Error'(
         Base => Make_State_Error(
            Current_State => "Uninitialized",
            Attempted_Transition => "Access",
            Message => "Ring buffer not initialized",
            Recovery => "Call Initialize first"
         ).Base
      );
   end Make_Not_Initialized_Error;

   --  Try to push an element (non-blocking)
   function Try_Push
     (Buffer : in out Ring_Buffer_Type; Item : Element_Type) return Push_Result.Result
   is
      Current_Tail : Atomic_Unsigned_32;
      Next_Tail    : Atomic_Unsigned_32;
      Current_Head : Atomic_Unsigned_32;
   begin
      --  Check initialization
      if not Buffer.Initialized then
         return Push_Result.Err (Make_Not_Initialized_Error);
      end if;
      
      --  Load tail position with acquire semantics
      Current_Tail := Buffer.Tail.Value;
      Next_Tail := (Current_Tail + 1) mod Atomic_Unsigned_32 (Buffer_Size);

      --  Load head position to check if full
      Current_Head := Buffer.Head.Value;

      --  Check if buffer is full
      if Next_Tail = Current_Head then
         return Push_Result.Err (Make_Buffer_Full_Error);
      end if;

      --  Store the item
      Buffer.Storage (Natural (Current_Tail)) := Item;

      --  Update tail with release semantics (make write visible)
      Buffer.Tail.Value := Next_Tail;

      return Push_Result.Ok (True);
   end Try_Push;

   --  Try to pop an element (non-blocking)
   function Try_Pop
     (Buffer : in out Ring_Buffer_Type) return Pop_Result.Result
   is
      Current_Head : Atomic_Unsigned_32;
      Current_Tail : Atomic_Unsigned_32;
      Next_Head    : Atomic_Unsigned_32;
      Item         : Element_Type;
   begin
      --  Check initialization
      if not Buffer.Initialized then
         return Pop_Result.Err (Make_Not_Initialized_Error);
      end if;
      
      --  Load head position
      Current_Head := Buffer.Head.Value;

      --  Load tail position to check if empty
      Current_Tail := Buffer.Tail.Value;

      --  Check if buffer is empty
      if Current_Head = Current_Tail then
         return Pop_Result.Err (Make_Buffer_Empty_Error);
      end if;

      --  Read the item
      Item := Buffer.Storage (Natural (Current_Head));

      --  Update head with release semantics
      Next_Head := (Current_Head + 1) mod Atomic_Unsigned_32 (Buffer_Size);
      Buffer.Head.Value := Next_Head;

      return Pop_Result.Ok (Item);
   end Try_Pop;

   --  Check if buffer is empty
   function Is_Empty (Buffer : Ring_Buffer_Type) return Boolean is
   begin
      return Buffer.Head.Value = Buffer.Tail.Value;
   end Is_Empty;

   --  Check if buffer is full
   function Is_Full (Buffer : Ring_Buffer_Type) return Boolean is
      Current_Tail : constant Atomic_Unsigned_32 := Buffer.Tail.Value;
      Next_Tail    : constant Atomic_Unsigned_32 :=
        (Current_Tail + 1) mod Atomic_Unsigned_32 (Buffer_Size);
      Current_Head : constant Atomic_Unsigned_32 := Buffer.Head.Value;
   begin
      return Next_Tail = Current_Head;
   end Is_Full;

   --  Get current size (approximate due to concurrent access)
   function Size (Buffer : Ring_Buffer_Type) return Natural is
      Head : constant Atomic_Unsigned_32 := Buffer.Head.Value;
      Tail : constant Atomic_Unsigned_32 := Buffer.Tail.Value;
   begin
      if Tail >= Head then
         return Natural (Tail - Head);
      else
         return Natural (Atomic_Unsigned_32 (Buffer_Size) - Head + Tail);
      end if;
   end Size;

   --  Zero-copy operations
   package body Zero_Copy_Operations is

      --  Try to push by transferring ownership
      function Try_Push_Ownership
        (Buffer : in out Ring_Buffer_Type; Item : in out Element_Access)
         return Ownership_Result.Result
      is
         Current_Tail : Atomic_Unsigned_32;
         Next_Tail    : Atomic_Unsigned_32;
         Current_Head : Atomic_Unsigned_32;
      begin
         --  Check initialization
         if not Buffer.Initialized then
            return Ownership_Result.Err (Make_Not_Initialized_Error);
         end if;
         
         --  Load tail position with acquire semantics
         Current_Tail := Buffer.Tail.Value;
         Next_Tail := (Current_Tail + 1) mod Atomic_Unsigned_32 (Buffer_Size);

         --  Load head position to check if full
         Current_Head := Buffer.Head.Value;

         --  Check if buffer is full
         if Next_Tail = Current_Head then
            return Ownership_Result.Err (Make_Buffer_Full_Error);
         end if;

         --  Transfer ownership by copying the access value
         declare
            --  We need to store the access value, not the element
            type Access_Holder is record
               Ptr : Element_Access;
            end record;

            function To_Element is new
              Ada.Unchecked_Conversion
                (Source => Access_Holder,
                 Target => Element_Type);
         begin
            Buffer.Storage (Natural (Current_Tail)) :=
              To_Element ((Ptr => Item));
            Item := null;  -- Clear source to indicate ownership transfer
         end;

         --  Update tail with release semantics
         Buffer.Tail.Value := Next_Tail;

         return Ownership_Result.Ok (True);
      end Try_Push_Ownership;

      --  Try to pop by transferring ownership
      function Try_Pop_Ownership
        (Buffer : in out Ring_Buffer_Type)
         return Pop_Ownership_Result.Result
      is
         Current_Head : Atomic_Unsigned_32;
         Current_Tail : Atomic_Unsigned_32;
         Next_Head    : Atomic_Unsigned_32;
         Item         : Element_Access;
      begin
         --  Check initialization
         if not Buffer.Initialized then
            return Pop_Ownership_Result.Err (Make_Not_Initialized_Error);
         end if;
         
         --  Load head position
         Current_Head := Buffer.Head.Value;

         --  Load tail position to check if empty
         Current_Tail := Buffer.Tail.Value;

         --  Check if buffer is empty
         if Current_Head = Current_Tail then
            return Pop_Ownership_Result.Err (Make_Buffer_Empty_Error);
         end if;

         --  Transfer ownership by extracting the access value
         declare
            type Access_Holder is record
               Ptr : Element_Access;
            end record;

            function To_Holder is new
              Ada.Unchecked_Conversion
                (Source => Element_Type,
                 Target => Access_Holder);

            Holder : Access_Holder;
         begin
            Holder := To_Holder (Buffer.Storage (Natural (Current_Head)));
            Item := Holder.Ptr;
         end;

         --  Update head with release semantics
         Next_Head := (Current_Head + 1) mod Atomic_Unsigned_32 (Buffer_Size);
         Buffer.Head.Value := Next_Head;

         return Pop_Ownership_Result.Ok (Item);
      end Try_Pop_Ownership;

   end Zero_Copy_Operations;

end Abohlib.Core.Domain.Utilities.Concurrent.Lock_Free_Ring_Buffer;