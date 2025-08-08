--  =============================================================================
--  Abohlib.Infrastructure.Adapters.POSIX_System_Info - Implementation
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

with System.Multiprocessors;
with Interfaces.C;
with Interfaces.C.Strings;
with Ada.Strings.Unbounded;

package body Abohlib.Infrastructure.Adapters.POSIX_System_Info is

   use Interfaces.C;
   use Ada.Strings.Unbounded;

   --  System calls for memory information (platform-specific)

   --  Darwin (macOS) specific
   type size_t is new Interfaces.C.size_t;

   function sysctlbyname
     (name    : Interfaces.C.Strings.chars_ptr;
      oldp    : System.Address;
      oldlenp : access size_t;
      newp    : System.Address;
      newlen  : size_t) return int
   with Import, Convention => C, External_Name => "sysctlbyname";

   -- ----------------------
   --  Get_Total_Memory
   -- ----------------------

   overriding
   function Get_Total_Memory
     (Self : POSIX_System_Info_Provider) return Memory_Result.Result
   is
      pragma Unreferenced (Self);
      Mem_Size : aliased Interfaces.C.long := 0;
      Len      : aliased size_t := Interfaces.C.long'Size / 8;
      Name     : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("hw.memsize");
      Result   : int;
   begin
      Result :=
        sysctlbyname
          (Name, Mem_Size'Address, Len'Access, System.Null_Address, 0);
      Interfaces.C.Strings.Free (Name);

      if Result = 0 then
         return Memory_Result.Ok (Long_Long_Integer (Mem_Size));
      else
         return
           Memory_Result.Err
             (Make_System_Info_Error
                (System_Call_Failed,
                 "Failed to retrieve total memory",
                 "sysctlbyname hw.memsize failed"));
      end if;
   exception
      when others =>
         return
           Memory_Result.Err
             (Make_System_Info_Error
                (Information_Unavailable,
                 "Exception occurred getting total memory",
                 "Unexpected error in Get_Total_Memory"));
   end Get_Total_Memory;

   -- --------------------------
   --  Get_Available_Memory
   -- --------------------------

   overriding
   function Get_Available_Memory
     (Self : POSIX_System_Info_Provider) return Memory_Result.Result
   is
      --  On macOS, we can use vm_stat to get free pages
      --  For now, return 80% of total as a reasonable estimate
      Total_Result : constant Memory_Result.Result := Get_Total_Memory (Self);
   begin
      if Memory_Result.Is_Ok (Total_Result) then
         declare
            Total : constant Long_Long_Integer :=
              Memory_Result.Get_Ok (Total_Result);
         begin
            --  This is a simplified implementation
            --  A full implementation would parse vm_stat output
            return Memory_Result.Ok ((Total * 8) / 10);  -- 80% of total
         end;
      else
         return Total_Result;
      end if;
   exception
      when others =>
         return
           Memory_Result.Err
             (Make_System_Info_Error
                (Information_Unavailable,
                 "Exception occurred getting available memory",
                 "Unexpected error in Get_Available_Memory"));
   end Get_Available_Memory;

   -- -------------------
   --  Get_CPU_Count
   -- -------------------

   overriding
   function Get_CPU_Count
     (Self : POSIX_System_Info_Provider) return CPU_Count_Result.Result
   is
      pragma Unreferenced (Self);
   begin
      return
        CPU_Count_Result.Ok (Natural (System.Multiprocessors.Number_Of_CPUs));
   exception
      when others =>
         return
           CPU_Count_Result.Err
             (Make_System_Info_Error
                (Information_Unavailable,
                 "Exception occurred getting CPU count",
                 "System.Multiprocessors.Number_Of_CPUs failed"));
   end Get_CPU_Count;

   -- ------------------------------
   --  Get_Physical_CPU_Count
   -- ------------------------------

   overriding
   function Get_Physical_CPU_Count
     (Self : POSIX_System_Info_Provider) return CPU_Count_Result.Result
   is
      pragma Unreferenced (Self);
      CPU_Count : aliased Interfaces.C.long := 0;
      Len       : aliased size_t := Interfaces.C.long'Size / 8;
      Name      : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("hw.physicalcpu");
      Result    : int;
   begin
      Result :=
        sysctlbyname
          (Name, CPU_Count'Address, Len'Access, System.Null_Address, 0);
      Interfaces.C.Strings.Free (Name);

      if Result = 0 then
         return CPU_Count_Result.Ok (Natural (CPU_Count));
      else
         return
           CPU_Count_Result.Err
             (Make_System_Info_Error
                (System_Call_Failed,
                 "Failed to retrieve physical CPU count",
                 "sysctlbyname hw.physicalcpu failed"));
      end if;
   exception
      when others =>
         return
           CPU_Count_Result.Err
             (Make_System_Info_Error
                (Information_Unavailable,
                 "Exception occurred getting physical CPU count",
                 "Unexpected error in Get_Physical_CPU_Count"));
   end Get_Physical_CPU_Count;

   -- -----------------
   --  Get_OS_Name
   -- -----------------

   overriding
   function Get_OS_Name
     (Self : POSIX_System_Info_Provider) return String_Result.Result
   is
      pragma Unreferenced (Self);
      OS_Name : String (1 .. Max_OS_Name_Length);
      Len     : aliased size_t := OS_Name'Length;
      Name    : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("kern.ostype");
      Result  : int;
   begin
      Result :=
        sysctlbyname
          (Name, OS_Name'Address, Len'Access, System.Null_Address, 0);
      Interfaces.C.Strings.Free (Name);

      if Result = 0 then
         declare
            Actual_Name : constant String := OS_Name (1 .. Natural (Len) - 1);
         begin
            return String_Result.Ok (To_Unbounded_String (Actual_Name));
         end;
      else
         return
           String_Result.Err
             (Make_System_Info_Error
                (System_Call_Failed,
                 "Failed to retrieve OS name",
                 "sysctlbyname kern.ostype failed"));
      end if;
   exception
      when others =>
         return
           String_Result.Err
             (Make_System_Info_Error
                (Information_Unavailable,
                 "Exception occurred getting OS name",
                 "Unexpected error in Get_OS_Name"));
   end Get_OS_Name;

   -- --------------------
   --  Get_OS_Version
   -- --------------------

   overriding
   function Get_OS_Version
     (Self : POSIX_System_Info_Provider) return String_Result.Result
   is
      pragma Unreferenced (Self);
      OS_Version : String (1 .. Max_OS_Version_Length);
      Len        : aliased size_t := OS_Version'Length;
      Name       : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("kern.osrelease");
      Result     : int;
   begin
      Result :=
        sysctlbyname
          (Name, OS_Version'Address, Len'Access, System.Null_Address, 0);
      Interfaces.C.Strings.Free (Name);

      if Result = 0 then
         declare
            Actual_Version : constant String :=
              OS_Version (1 .. Natural (Len) - 1);
         begin
            return String_Result.Ok (To_Unbounded_String (Actual_Version));
         end;
      else
         return
           String_Result.Err
             (Make_System_Info_Error
                (System_Call_Failed,
                 "Failed to retrieve OS version",
                 "sysctlbyname kern.osrelease failed"));
      end if;
   exception
      when others =>
         return
           String_Result.Err
             (Make_System_Info_Error
                (Information_Unavailable,
                 "Exception occurred getting OS version",
                 "Unexpected error in Get_OS_Version"));
   end Get_OS_Version;

   -- ----------------------
   --  Get_Architecture
   -- ----------------------

   overriding
   function Get_Architecture
     (Self : POSIX_System_Info_Provider) return String_Result.Result
   is
      pragma Unreferenced (Self);
      Arch   : String (1 .. Max_Architecture_Length);
      Len    : aliased size_t := Arch'Length;
      Name   : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("hw.machine");
      Result : int;
   begin
      Result :=
        sysctlbyname (Name, Arch'Address, Len'Access, System.Null_Address, 0);
      Interfaces.C.Strings.Free (Name);

      if Result = 0 then
         declare
            Actual_Arch : constant String := Arch (1 .. Natural (Len) - 1);
         begin
            return String_Result.Ok (To_Unbounded_String (Actual_Arch));
         end;
      else
         return
           String_Result.Err
             (Make_System_Info_Error
                (System_Call_Failed,
                 "Failed to retrieve architecture",
                 "sysctlbyname hw.machine failed"));
      end if;
   exception
      when others =>
         return
           String_Result.Err
             (Make_System_Info_Error
                (Information_Unavailable,
                 "Exception occurred getting architecture",
                 "Unexpected error in Get_Architecture"));
   end Get_Architecture;

end Abohlib.Infrastructure.Adapters.POSIX_System_Info;
