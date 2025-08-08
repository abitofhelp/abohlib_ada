--   =============================================================================
--   Abohlib.Core.Domain.Utilities.System_Info - System Information Service
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--
--   Service for querying system information using dependency injection.
--   Follows hexagonal architecture by depending on ports instead of concrete
--   infrastructure implementations.
--   =============================================================================

pragma Ada_2022;

with Abohlib.Core.Domain.Ports.System_Info;
with Abohlib.Core.Domain.Result;

package Abohlib.Core.Domain.Utilities.System_Info is

   use Abohlib.Core.Domain.Ports.System_Info;

   --  Result types for convenience
   package Boolean_Result is new
     Abohlib.Core.Domain.Result.Result_Package
       (Ok_Type  => Boolean,
        Err_Type => System_Info_Error);

   --  System information service using dependency injection
   type System_Info_Service
     (Provider : not null access System_Info_Provider'Class)
   is
     limited private;

   --  Create a new system info service with the given provider
   function Create
     (Provider : not null access System_Info_Provider'Class)
      return System_Info_Service;

   --  Get total physical memory in bytes
   function Get_Total_Memory
     (Service : System_Info_Service) return Memory_Result.Result;

   --  Get available (free) memory in bytes
   function Get_Available_Memory
     (Service : System_Info_Service) return Memory_Result.Result;

   --  Get number of CPU cores
   function Get_CPU_Count
     (Service : System_Info_Service) return CPU_Count_Result.Result;

   --  Get physical CPU cores
   function Get_Physical_CPU_Count
     (Service : System_Info_Service) return CPU_Count_Result.Result;

   --  Get OS information
   function Get_OS_Name
     (Service : System_Info_Service) return String_Result.Result;

   function Get_OS_Version
     (Service : System_Info_Service) return String_Result.Result;

   function Get_Architecture
     (Service : System_Info_Service) return String_Result.Result;

   --  Helper functions for common operations
   function Has_Sufficient_Memory
     (Service : System_Info_Service; Required_Bytes : Long_Long_Integer)
      return Boolean_Result.Result
   with Pre => Required_Bytes > 0;

   function Get_Recommended_Memory_Limit
     (Service : System_Info_Service) return Memory_Result.Result;

private

   type System_Info_Service
     (Provider : not null access System_Info_Provider'Class)
   is limited null record;

end Abohlib.Core.Domain.Utilities.System_Info;
