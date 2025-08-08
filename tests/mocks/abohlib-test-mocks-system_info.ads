--   =============================================================================
--   Abohlib.Test.Mocks.System_Info - Mock System Info Provider
--   =============================================================================
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--
--   Purpose:
--     Mock implementation of the System_Info_Provider interface for testing.
--     Provides configurable system information and call tracking.
--   =============================================================================

pragma Ada_2022;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Abohlib.Core.Domain.Ports.System_Info;

package Abohlib.Test.Mocks.System_Info is

   use Abohlib.Core.Domain.Ports.System_Info;

--   Mock system info provider
   type Mock_System_Info_Provider is new System_Info_Provider with record
--  Configurable values
      Total_Memory        : Long_Long_Integer := 8_589_934_592; -- 8 GB default
      Available_Memory    : Long_Long_Integer := 4_294_967_296; -- 4 GB default
      CPU_Count          : Natural := 8;
      Physical_CPU_Count : Natural := 4;
      OS_Name            : Unbounded_String := To_Unbounded_String ("Mock OS");
      OS_Version         : Unbounded_String := To_Unbounded_String ("1.0.0");
      Architecture       : Unbounded_String := To_Unbounded_String ("x86_64");

--  Mock control
      Call_Count         : aliased Natural := 0;
      Fail_Next          : Boolean := False;
      Fail_Error         : System_Info_Error;

--  Per-method failure control
      Fail_Total_Memory     : Boolean := False;
      Fail_Available_Memory : Boolean := False;
      Fail_CPU_Count       : Boolean := False;
      Fail_Physical_CPU    : Boolean := False;
      Fail_OS_Name         : Boolean := False;
      Fail_OS_Version      : Boolean := False;
      Fail_Architecture    : Boolean := False;
   end record;

--   Setup methods
   procedure Set_Total_Memory
     (Self  : in out Mock_System_Info_Provider;
      Value : Long_Long_Integer);

   procedure Set_Available_Memory
     (Self  : in out Mock_System_Info_Provider;
      Value : Long_Long_Integer);

   procedure Set_CPU_Count
     (Self  : in out Mock_System_Info_Provider;
      Value : Natural);

   procedure Set_Physical_CPU_Count
     (Self  : in out Mock_System_Info_Provider;
      Value : Natural);

   procedure Set_OS_Name
     (Self  : in out Mock_System_Info_Provider;
      Value : String);

   procedure Set_OS_Version
     (Self  : in out Mock_System_Info_Provider;
      Value : String);

   procedure Set_Architecture
     (Self  : in out Mock_System_Info_Provider;
      Value : String);

   procedure Set_Fail_Next
     (Self  : in out Mock_System_Info_Provider;
      Error : System_Info_Error);

   procedure Set_Method_Failure
     (Self        : in out Mock_System_Info_Provider;
      Method_Name : String;
      Should_Fail : Boolean);

   function Get_Call_Count
     (Self : Mock_System_Info_Provider) return Natural;

   procedure Reset_Call_Count
     (Self : in out Mock_System_Info_Provider);

   procedure Reset_All
     (Self : in out Mock_System_Info_Provider);

--   Implement System_Info_Provider interface
   overriding function Get_Total_Memory
     (Self : Mock_System_Info_Provider) return Memory_Result.Result;

   overriding function Get_Available_Memory
     (Self : Mock_System_Info_Provider) return Memory_Result.Result;

   overriding function Get_CPU_Count
     (Self : Mock_System_Info_Provider) return CPU_Count_Result.Result;

   overriding function Get_Physical_CPU_Count
     (Self : Mock_System_Info_Provider) return CPU_Count_Result.Result;

   overriding function Get_OS_Name
     (Self : Mock_System_Info_Provider) return String_Result.Result;

   overriding function Get_OS_Version
     (Self : Mock_System_Info_Provider) return String_Result.Result;

   overriding function Get_Architecture
     (Self : Mock_System_Info_Provider) return String_Result.Result;

end Abohlib.Test.Mocks.System_Info;
