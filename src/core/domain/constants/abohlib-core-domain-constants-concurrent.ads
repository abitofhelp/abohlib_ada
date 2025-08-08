--  =============================================================================
--  Abohlib.Core.Domain.Constants.Concurrent - Concurrent Utilities Constants
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Purpose:
--    Defines constants used by concurrent utilities like ring buffers, 
--    object pools, and other lock-free data structures.
--  =============================================================================

pragma Ada_2022;

package Abohlib.Core.Domain.Constants.Concurrent is
   pragma Pure;

   --  ==========================================================================
   --  Ring Buffer Constants  
   --  ==========================================================================
   
   --  Default size for lock-free ring buffers (must be power of 2)
   Default_Ring_Buffer_Size : constant := 128;
   
   --  Minimum and maximum allowed ring buffer sizes
   Min_Ring_Buffer_Size : constant := 16;
   Max_Ring_Buffer_Size : constant := 65536;  -- 64K elements
   
   --  ==========================================================================
   --  Object Pool Constants
   --  ==========================================================================
   
   --  Default initial pool size
   Default_Pool_Initial_Size : constant := 16;
   
   --  Default maximum pool size  
   Default_Pool_Max_Size : constant := 1024;
   
   --  ==========================================================================
   --  Buffer Manager Constants
   --  ==========================================================================
   
   --  Default number of buffers in buffer manager
   Default_Buffer_Count : constant := 2;
   
   --  Default buffer size in bytes
   Default_Buffer_Size_Bytes : constant := 4096;  -- 4KB
   
   --  ==========================================================================
   --  Queue Constants
   --  ==========================================================================
   
   --  Default queue capacity
   Default_Queue_Capacity : constant := 256;
   
   --  ==========================================================================
   --  Retry Constants
   --  ==========================================================================
   
   --  Default spin count before yielding
   Default_Spin_Count : constant := 100;
   
   --  Default backoff delay in microseconds
   Default_Backoff_Microseconds : constant := 10;

end Abohlib.Core.Domain.Constants.Concurrent;