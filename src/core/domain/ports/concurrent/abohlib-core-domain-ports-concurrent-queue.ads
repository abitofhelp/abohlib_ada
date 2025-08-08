--  =============================================================================
--  Abohlib.Core.Domain.Ports.Concurrent.Queue - Queue Port Interface
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Domain port for concurrent queue operations. This abstraction allows
--  the application layer to use queues without depending on infrastructure
--  implementations.
--  =============================================================================

pragma Ada_2022;

package Abohlib.Core.Domain.Ports.Concurrent.Queue is

   pragma Preelaborate;

   --  Generic queue interface for any element type
   generic
      type Element_Type is private;
      Buffer_Size : Positive;
   package Queue_Interface is

      type Queue_Type is limited interface;
      type Queue_Access is access all Queue_Type'Class;

      --  Initialize the queue
      procedure Initialize (Queue : in out Queue_Type) is abstract
      with
        Post'Class =>
          Queue.Is_Empty and then not Queue.Is_Full and then Queue.Size = 0;

      --  Try to push an element (non-blocking)
      function Try_Push
        (Queue : in out Queue_Type; Item : Element_Type) return Boolean
      is abstract
      with
        Post'Class => (if Try_Push'Result then not Queue.Is_Empty else True);

      --  Try to pop an element (non-blocking)
      function Try_Pop
        (Queue : in out Queue_Type; Item : out Element_Type) return Boolean
      is abstract
      with
        Post'Class => (if Try_Pop'Result then not Queue.Is_Full else True);

      --  Check if queue is empty
      function Is_Empty (Queue : Queue_Type) return Boolean is abstract
      with Post'Class => Is_Empty'Result = (Queue.Size = 0);

      --  Check if queue is full
      function Is_Full (Queue : Queue_Type) return Boolean is abstract;

      --  Get current size
      function Size (Queue : Queue_Type) return Natural is abstract
      with Post'Class => Size'Result <= Buffer_Size;

   end Queue_Interface;

end Abohlib.Core.Domain.Ports.Concurrent.Queue;