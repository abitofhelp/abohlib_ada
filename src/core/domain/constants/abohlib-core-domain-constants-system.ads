--  =============================================================================
--  Abohlib.Core.Domain.Constants.System - Hardware and System Constants  
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Purpose:
--    Provides hardware and system-level constants that are used across
--    different applications but are not specific to any business domain.
--    These represent technical, architectural, and hardware characteristics.
--
--  Usage:
--    Import this package to access system-level constants for performance
--    optimization, hardware alignment, and system resource management.
--
--  Examples:
--    - Cache line sizes for avoiding false sharing
--    - Standard page sizes for memory mapping
--    - Network packet sizes and limits
--    - System-level buffer sizes
--  =============================================================================

pragma Ada_2022;

package Abohlib.Core.Domain.Constants.System is

   pragma Pure;

   --  CPU Cache Architecture Constants
   --  Most modern processors use 64-byte cache lines
   CACHE_LINE_SIZE_BYTES : constant := 64;  -- Common x86/x64 cache line size
   
   --  Alternative cache line sizes for specific architectures  
   CACHE_LINE_SIZE_32   : constant := 32;   -- Some embedded processors
   CACHE_LINE_SIZE_128  : constant := 128;  -- Some high-end processors

   --  Memory Management Constants
   STANDARD_PAGE_SIZE   : constant := 4_096;  -- 4KB standard page size
   LARGE_PAGE_SIZE      : constant := 2_097_152;  -- 2MB large/huge page size
   
   --  Common Buffer Sizes (using SI units for consistency with file/data processing)  
   BUFFER_SIZE_1KB      : constant := 1_000;       -- 1KB buffer (SI)
   BUFFER_SIZE_4KB      : constant := 4_000;       -- 4KB buffer (SI)  
   BUFFER_SIZE_16KB     : constant := 16_000;      -- 16KB buffer (SI)
   BUFFER_SIZE_64KB     : constant := 64_000;      -- 64KB buffer (SI)
   BUFFER_SIZE_1MB      : constant := 1_000_000;   -- 1MB buffer (SI, default I/O)
   BUFFER_SIZE_16MB     : constant := 16_000_000;  -- 16MB buffer (SI, large I/O)
   
   --  Common Buffer Alignment
   --  For SIMD operations and DMA transfers
   SIMD_ALIGNMENT_BYTES : constant := 16;   -- 128-bit SIMD alignment
   AVX_ALIGNMENT_BYTES  : constant := 32;   -- 256-bit AVX alignment
   
   --  Network Constants
   ETHERNET_MTU         : constant := 1_500;   -- Standard Ethernet MTU
   JUMBO_FRAME_MTU      : constant := 9_000;   -- Jumbo frame size

end Abohlib.Core.Domain.Constants.System;