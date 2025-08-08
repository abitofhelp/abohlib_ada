--   =============================================================================
--   Abohlib.Test.Mocks.System_Info - Implementation
--   =============================================================================
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--   =============================================================================

pragma Ada_2022;
pragma Warnings (Off, "subprogram body has no previous spec");

with Ada.Unchecked_Conversion;
with System;

package body Abohlib.Test.Mocks.System_Info is

--   ==========================================================================
--   Helper to increment counter (works around 'in' parameter restriction)
--   ==========================================================================

   procedure Increment_Counter (Self : Mock_System_Info_Provider) is
      type Counter_Access is access all Natural;
      function To_Counter_Access is new Ada.Unchecked_Conversion
        (System.Address, Counter_Access);
      Counter : constant Counter_Access := To_Counter_Access (Self.Call_Count'Address);
   begin
      Counter.all := Counter.all + 1;
   end Increment_Counter;

--   ==========================================================================
--   Setup Methods
--   ==========================================================================

   procedure Set_Total_Memory
     (Self  : in out Mock_System_Info_Provider;
      Value : Long_Long_Integer)
   is
   begin
      Self.Total_Memory := Value;
   end Set_Total_Memory;

   procedure Set_Available_Memory
     (Self  : in out Mock_System_Info_Provider;
      Value : Long_Long_Integer)
   is
   begin
      Self.Available_Memory := Value;
   end Set_Available_Memory;

   procedure Set_CPU_Count
     (Self  : in out Mock_System_Info_Provider;
      Value : Natural)
   is
   begin
      Self.CPU_Count := Value;
   end Set_CPU_Count;

   procedure Set_Physical_CPU_Count
     (Self  : in out Mock_System_Info_Provider;
      Value : Natural)
   is
   begin
      Self.Physical_CPU_Count := Value;
   end Set_Physical_CPU_Count;

   procedure Set_OS_Name
     (Self  : in out Mock_System_Info_Provider;
      Value : String)
   is
   begin
      Self.OS_Name := To_Unbounded_String (Value);
   end Set_OS_Name;

   procedure Set_OS_Version
     (Self  : in out Mock_System_Info_Provider;
      Value : String)
   is
   begin
      Self.OS_Version := To_Unbounded_String (Value);
   end Set_OS_Version;

   procedure Set_Architecture
     (Self  : in out Mock_System_Info_Provider;
      Value : String)
   is
   begin
      Self.Architecture := To_Unbounded_String (Value);
   end Set_Architecture;

   procedure Set_Fail_Next
     (Self  : in out Mock_System_Info_Provider;
      Error : System_Info_Error)
   is
   begin
      Self.Fail_Next := True;
      Self.Fail_Error := Error;
   end Set_Fail_Next;

   procedure Set_Method_Failure
     (Self        : in out Mock_System_Info_Provider;
      Method_Name : String;
      Should_Fail : Boolean)
   is
   begin
      if Method_Name = "Get_Total_Memory" then
         Self.Fail_Total_Memory := Should_Fail;
      elsif Method_Name = "Get_Available_Memory" then
         Self.Fail_Available_Memory := Should_Fail;
      elsif Method_Name = "Get_CPU_Count" then
         Self.Fail_CPU_Count := Should_Fail;
      elsif Method_Name = "Get_Physical_CPU_Count" then
         Self.Fail_Physical_CPU := Should_Fail;
      elsif Method_Name = "Get_OS_Name" then
         Self.Fail_OS_Name := Should_Fail;
      elsif Method_Name = "Get_OS_Version" then
         Self.Fail_OS_Version := Should_Fail;
      elsif Method_Name = "Get_Architecture" then
         Self.Fail_Architecture := Should_Fail;
      end if;
   end Set_Method_Failure;

   function Get_Call_Count
     (Self : Mock_System_Info_Provider) return Natural is (Self.Call_Count);

   procedure Reset_Call_Count
     (Self : in out Mock_System_Info_Provider)
   is
   begin
      Self.Call_Count := 0;
   end Reset_Call_Count;

   procedure Reset_All
     (Self : in out Mock_System_Info_Provider)
   is
   begin
      Self.Total_Memory := 8_589_934_592;
      Self.Available_Memory := 4_294_967_296;
      Self.CPU_Count := 8;
      Self.Physical_CPU_Count := 4;
      Self.OS_Name := To_Unbounded_String ("Mock OS");
      Self.OS_Version := To_Unbounded_String ("1.0.0");
      Self.Architecture := To_Unbounded_String ("x86_64");
      Self.Call_Count := 0;
      Self.Fail_Next := False;
      Self.Fail_Total_Memory := False;
      Self.Fail_Available_Memory := False;
      Self.Fail_CPU_Count := False;
      Self.Fail_Physical_CPU := False;
      Self.Fail_OS_Name := False;
      Self.Fail_OS_Version := False;
      Self.Fail_Architecture := False;
   end Reset_All;

--   ==========================================================================
--   Interface Implementation
--   ==========================================================================

   overriding function Get_Total_Memory
     (Self : Mock_System_Info_Provider) return Memory_Result.Result
   is
   begin
      Increment_Counter (Self);

      if Self.Fail_Next then
         return Memory_Result.Err (Self.Fail_Error);
      end if;

      if Self.Fail_Total_Memory then
         return Memory_Result.Err
           (Make_System_Info_Error
              (Kind    => System_Call_Failed,
               Message => "Mock failure for Get_Total_Memory",
               Context => "Testing"));
      end if;

      return Memory_Result.Ok (Self.Total_Memory);
   end Get_Total_Memory;

   overriding function Get_Available_Memory
     (Self : Mock_System_Info_Provider) return Memory_Result.Result
   is
   begin
      Increment_Counter (Self);

      if Self.Fail_Next then
         return Memory_Result.Err (Self.Fail_Error);
      end if;

      if Self.Fail_Available_Memory then
         return Memory_Result.Err
           (Make_System_Info_Error
              (Kind    => System_Call_Failed,
               Message => "Mock failure for Get_Available_Memory",
               Context => "Testing"));
      end if;

      return Memory_Result.Ok (Self.Available_Memory);
   end Get_Available_Memory;

   overriding function Get_CPU_Count
     (Self : Mock_System_Info_Provider) return CPU_Count_Result.Result
   is
   begin
      Increment_Counter (Self);

      if Self.Fail_Next then
--  Note: Cannot modify Fail_Next since Self is 'in' mode
         return CPU_Count_Result.Err (Self.Fail_Error);
      end if;

      if Self.Fail_CPU_Count then
         return CPU_Count_Result.Err
           (Make_System_Info_Error
              (Kind    => System_Call_Failed,
               Message => "Mock failure for Get_CPU_Count",
               Context => "Testing"));
      end if;

      return CPU_Count_Result.Ok (Self.CPU_Count);
   end Get_CPU_Count;

   overriding function Get_Physical_CPU_Count
     (Self : Mock_System_Info_Provider) return CPU_Count_Result.Result
   is
   begin
      Increment_Counter (Self);

      if Self.Fail_Next then
--  Note: Cannot modify Fail_Next since Self is 'in' mode
         return CPU_Count_Result.Err (Self.Fail_Error);
      end if;

      if Self.Fail_Physical_CPU then
         return CPU_Count_Result.Err
           (Make_System_Info_Error
              (Kind    => System_Call_Failed,
               Message => "Mock failure for Get_Physical_CPU_Count",
               Context => "Testing"));
      end if;

      return CPU_Count_Result.Ok (Self.Physical_CPU_Count);
   end Get_Physical_CPU_Count;

   overriding function Get_OS_Name
     (Self : Mock_System_Info_Provider) return String_Result.Result
   is
   begin
      Increment_Counter (Self);

      if Self.Fail_Next then
--  Note: Cannot modify Fail_Next since Self is 'in' mode
         return String_Result.Err (Self.Fail_Error);
      end if;

      if Self.Fail_OS_Name then
         return String_Result.Err
           (Make_System_Info_Error
              (Kind    => Information_Unavailable,
               Message => "Mock failure for Get_OS_Name",
               Context => "Testing"));
      end if;

      return String_Result.Ok (Self.OS_Name);
   end Get_OS_Name;

   overriding function Get_OS_Version
     (Self : Mock_System_Info_Provider) return String_Result.Result
   is
   begin
      Increment_Counter (Self);

      if Self.Fail_Next then
--  Note: Cannot modify Fail_Next since Self is 'in' mode
         return String_Result.Err (Self.Fail_Error);
      end if;

      if Self.Fail_OS_Version then
         return String_Result.Err
           (Make_System_Info_Error
              (Kind    => Information_Unavailable,
               Message => "Mock failure for Get_OS_Version",
               Context => "Testing"));
      end if;

      return String_Result.Ok (Self.OS_Version);
   end Get_OS_Version;

   overriding function Get_Architecture
     (Self : Mock_System_Info_Provider) return String_Result.Result
   is
   begin
      Increment_Counter (Self);

      if Self.Fail_Next then
--  Note: Cannot modify Fail_Next since Self is 'in' mode
         return String_Result.Err (Self.Fail_Error);
      end if;

      if Self.Fail_Architecture then
         return String_Result.Err
           (Make_System_Info_Error
              (Kind    => Information_Unavailable,
               Message => "Mock failure for Get_Architecture",
               Context => "Testing"));
      end if;

      return String_Result.Ok (Self.Architecture);
   end Get_Architecture;

end Abohlib.Test.Mocks.System_Info;

pragma Warnings (On, "subprogram body has no previous spec");
