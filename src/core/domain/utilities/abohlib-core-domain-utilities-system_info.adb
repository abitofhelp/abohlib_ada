--   =============================================================================
--   Abohlib.Core.Domain.Utilities.System_Info - Implementation
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--   =============================================================================

pragma Ada_2022;

with Abohlib.Core.Domain.Constants.Bytes;

package body Abohlib.Core.Domain.Utilities.System_Info is

   -- ------------
   --  Create
   -- ------------

   function Create
     (Provider : not null access System_Info_Provider'Class)
      return System_Info_Service
   is ((Provider => Provider));

   -- ----------------------
   --  Get_Total_Memory
   -- ----------------------

   function Get_Total_Memory
     (Service : System_Info_Service) return Memory_Result.Result
   is (Service.Provider.Get_Total_Memory);

   -- --------------------------
   --  Get_Available_Memory
   -- --------------------------

   function Get_Available_Memory
     (Service : System_Info_Service) return Memory_Result.Result
   is (Service.Provider.Get_Available_Memory);

   -- -------------------
   --  Get_CPU_Count
   -- -------------------

   function Get_CPU_Count
     (Service : System_Info_Service) return CPU_Count_Result.Result
   is (Service.Provider.Get_CPU_Count);

   -- ------------------------------
   --  Get_Physical_CPU_Count
   -- ------------------------------

   function Get_Physical_CPU_Count
     (Service : System_Info_Service) return CPU_Count_Result.Result
   is (Service.Provider.Get_Physical_CPU_Count);

   -- -----------------
   --  Get_OS_Name
   -- -----------------

   function Get_OS_Name
     (Service : System_Info_Service) return String_Result.Result
   is (Service.Provider.Get_OS_Name);

   -- --------------------
   --  Get_OS_Version
   -- --------------------

   function Get_OS_Version
     (Service : System_Info_Service) return String_Result.Result
   is (Service.Provider.Get_OS_Version);

   -- ----------------------
   --  Get_Architecture
   -- ----------------------

   function Get_Architecture
     (Service : System_Info_Service) return String_Result.Result
   is (Service.Provider.Get_Architecture);

   -- ---------------------------
   --  Has_Sufficient_Memory
   -- ---------------------------

   function Has_Sufficient_Memory
     (Service : System_Info_Service; Required_Bytes : Long_Long_Integer)
      return Boolean_Result.Result
   is
      Available_Result : constant Memory_Result.Result :=
        Service.Provider.Get_Available_Memory;
   begin
      if Memory_Result.Is_Ok (Available_Result) then
         declare
            Available : constant Long_Long_Integer :=
              Memory_Result.Get_Ok (Available_Result);
            --  Leave at least 512MB free
            Min_Free  : constant Long_Long_Integer :=
              512 * Abohlib.Core.Domain.Constants.Bytes.SI_MB_LLI;
         begin
            return Boolean_Result.Ok (Available > Required_Bytes + Min_Free);
         end;
      else
         return Boolean_Result.Err (Memory_Result.Get_Err (Available_Result));
      end if;
   end Has_Sufficient_Memory;

   -- ---------------------------------
   --  Get_Recommended_Memory_Limit
   -- ---------------------------------

   function Get_Recommended_Memory_Limit
     (Service : System_Info_Service) return Memory_Result.Result
   is
      Available_Result : constant Memory_Result.Result :=
        Service.Provider.Get_Available_Memory;
   begin
      if Memory_Result.Is_Ok (Available_Result) then
         declare
            Available : constant Long_Long_Integer :=
              Memory_Result.Get_Ok (Available_Result);
         begin
            --  Use 80% of available memory
            return Memory_Result.Ok ((Available * 8) / 10);
         end;
      else
         return Available_Result;
      end if;
   end Get_Recommended_Memory_Limit;

end Abohlib.Core.Domain.Utilities.System_Info;
