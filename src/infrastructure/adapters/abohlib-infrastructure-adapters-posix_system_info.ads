--  =============================================================================
--  Abohlib.Infrastructure.Adapters.POSIX_System_Info - POSIX System Info Adapter
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Purpose:
--    POSIX-specific implementation of the System_Info_Provider interface.
--    Provides system information using POSIX system calls and utilities.
--  =============================================================================

pragma Ada_2022;

with Abohlib.Core.Domain.Ports.System_Info;

package Abohlib.Infrastructure.Adapters.POSIX_System_Info is

   use Abohlib.Core.Domain.Ports.System_Info;

   --  =============================================================================
   --  Constants
   --  =============================================================================
   
   --  System information buffer sizes
   Max_OS_Name_Length      : constant := 256;
   Max_OS_Version_Length   : constant := 256;
   Max_Architecture_Length : constant := 256;

   --  =============================================================================
   --  Type Definition
   --  =============================================================================
   
   --  POSIX implementation of system information provider
   type POSIX_System_Info_Provider is limited new System_Info_Provider
   with null record;

   --  Memory information
   overriding
   function Get_Total_Memory
     (Self : POSIX_System_Info_Provider) return Memory_Result.Result;

   overriding
   function Get_Available_Memory
     (Self : POSIX_System_Info_Provider) return Memory_Result.Result;

   --  CPU information
   overriding
   function Get_CPU_Count
     (Self : POSIX_System_Info_Provider) return CPU_Count_Result.Result;

   overriding
   function Get_Physical_CPU_Count
     (Self : POSIX_System_Info_Provider) return CPU_Count_Result.Result;

   --  System identification
   overriding
   function Get_OS_Name
     (Self : POSIX_System_Info_Provider) return String_Result.Result;

   overriding
   function Get_OS_Version
     (Self : POSIX_System_Info_Provider) return String_Result.Result;

   overriding
   function Get_Architecture
     (Self : POSIX_System_Info_Provider) return String_Result.Result;

end Abohlib.Infrastructure.Adapters.POSIX_System_Info;
