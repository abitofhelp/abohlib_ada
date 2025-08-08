--  =============================================================================
--  Abohlib.Core.Domain.Ports.System_Info - System Information Port Interface
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Purpose:
--    Defines the port interface for system information access following
--    hexagonal architecture principles. This abstracts away system-specific
--    operations from the domain layer.
--
--  Usage:
--    This interface should be implemented by infrastructure adapters
--    that provide concrete system information access.
--  =============================================================================

pragma Ada_2022;

with Abohlib.Core.Domain.Result;
with Ada.Strings.Unbounded;

package Abohlib.Core.Domain.Ports.System_Info is

   use Ada.Strings.Unbounded;

   --  Error types for system information operations
   type System_Info_Error_Kind is
     (System_Call_Failed,
      Unsupported_Platform,
      Access_Denied,
      Information_Unavailable);

   type System_Info_Error is record
      Kind    : System_Info_Error_Kind;
      Message : Unbounded_String;
      Context : Unbounded_String;
   end record;

   --  Result types
   package Memory_Result is new
     Abohlib.Core.Domain.Result.Result_Package
       (Ok_Type  => Long_Long_Integer,
        Err_Type => System_Info_Error);

   package CPU_Count_Result is new
     Abohlib.Core.Domain.Result.Result_Package
       (Ok_Type  => Natural,
        Err_Type => System_Info_Error);

   package String_Result is new
     Abohlib.Core.Domain.Result.Result_Package
       (Ok_Type  => Unbounded_String,
        Err_Type => System_Info_Error);

   --  System information provider interface
   type System_Info_Provider is limited interface;

   --  Memory information
   function Get_Total_Memory
     (Self : System_Info_Provider) return Memory_Result.Result
   is abstract;
   --  Returns total system memory in bytes

   function Get_Available_Memory
     (Self : System_Info_Provider) return Memory_Result.Result
   is abstract;
   --  Returns available system memory in bytes

   --  CPU information
   function Get_CPU_Count
     (Self : System_Info_Provider) return CPU_Count_Result.Result
   is abstract;
   --  Returns number of logical CPU cores

   function Get_Physical_CPU_Count
     (Self : System_Info_Provider) return CPU_Count_Result.Result
   is abstract;
   --  Returns number of physical CPU cores

   --  System identification
   function Get_OS_Name
     (Self : System_Info_Provider) return String_Result.Result
   is abstract;
   --  Returns operating system name

   function Get_OS_Version
     (Self : System_Info_Provider) return String_Result.Result
   is abstract;
   --  Returns operating system version

   function Get_Architecture
     (Self : System_Info_Provider) return String_Result.Result
   is abstract;
   --  Returns system architecture (e.g., "x86_64", "aarch64")

   --  Helper functions for creating errors
   function Make_System_Info_Error
     (Kind : System_Info_Error_Kind; Message : String; Context : String := "")
      return System_Info_Error;

end Abohlib.Core.Domain.Ports.System_Info;
