--  =============================================================================
--  Abohlib.Core.Domain.Utilities.Concurrent.Lock_Free_Queue_Adapter
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Adapter that implements the domain queue port using lock-free ring buffer.
--  =============================================================================

pragma Ada_2022;

with Abohlib.Core.Domain.Ports.Concurrent.Queue;
with Abohlib.Core.Domain.Utilities.Concurrent.Lock_Free_Ring_Buffer;

package Abohlib.Core.Domain.Utilities.Concurrent.Lock_Free_Queue_Adapter is

   --  Cannot be preelaborated due to dependency on non-preelaborated units

   --  Generic adapter for any element type
   generic
      type Element_Type is private;
      Buffer_Size : Positive;
   package Queue_Adapter is

      --  Instantiate the interface
      package Queue_Interface is new
        Abohlib.Core.Domain.Ports.Concurrent.Queue.Queue_Interface
          (Element_Type => Element_Type,
           Buffer_Size  => Buffer_Size);

      --  Instantiate the implementation
      package Ring_Buffer is new
        Abohlib.Core.Domain.Utilities.Concurrent.Lock_Free_Ring_Buffer
          (Element_Type => Element_Type,
           Buffer_Size  => Buffer_Size);

      --  Adapter type that implements the interface
      type Lock_Free_Queue_Type is limited new Queue_Interface.Queue_Type
      with record
         Buffer : Ring_Buffer.Ring_Buffer_Type;
      end record;

      --  Implement the interface methods
      overriding
      procedure Initialize (Queue : in out Lock_Free_Queue_Type);

      overriding
      function Try_Push
        (Queue : in out Lock_Free_Queue_Type; Item : Element_Type)
         return Boolean;

      overriding
      function Try_Pop
        (Queue : in out Lock_Free_Queue_Type; Item : out Element_Type)
         return Boolean;

      overriding
      function Is_Empty (Queue : Lock_Free_Queue_Type) return Boolean;

      overriding
      function Is_Full (Queue : Lock_Free_Queue_Type) return Boolean;

      overriding
      function Size (Queue : Lock_Free_Queue_Type) return Natural;

   end Queue_Adapter;

end Abohlib.Core.Domain.Utilities.Concurrent.Lock_Free_Queue_Adapter;