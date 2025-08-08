--   =============================================================================
--   Abohlib.Test.Mocks.File_System - Mock File System Provider
--   =============================================================================
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--
--   Purpose:
--     Mock implementation of the File_System_Provider interface for testing.
--     Provides configurable behavior and call tracking.
--   =============================================================================

pragma Ada_2022;

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Abohlib.Core.Domain.Ports.File_System;

package Abohlib.Test.Mocks.File_System is

   use Abohlib.Core.Domain.Ports.File_System;

--   Mock file entry for simulating file system
   type Mock_File_Entry is record
      Exists      : Boolean := False;
      Is_Dir      : Boolean := False;
      Is_File     : Boolean := False;
      Size        : Natural := 0;
      Extension   : Unbounded_String;
      Base_Name   : Unbounded_String;
      Directory   : Unbounded_String;
   end record;

--   Map for storing mock file entries
   package File_Map is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Mock_File_Entry,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");

--   Mock file system provider
   type Mock_File_System_Provider is new File_System_Provider with record
      Files              : File_Map.Map;
      Call_Count         : aliased Natural := 0;
      Fail_Next          : Boolean := False;
      Fail_Error         : File_System_Error;
      Path_Separator     : Character := '/';
      Extension_Separator : Character := '.';
   end record;

--   Setup methods
   procedure Add_File
     (Self      : in out Mock_File_System_Provider;
      Path      : String;
      Size      : Natural := 0;
      Extension : String := "";
      Base_Name : String := "";
      Directory : String := "");

   procedure Add_Directory
     (Self : in out Mock_File_System_Provider;
      Path : String);

   procedure Set_Fail_Next
     (Self  : in out Mock_File_System_Provider;
      Error : File_System_Error);

   procedure Clear_Files
     (Self : in out Mock_File_System_Provider);

   function Get_Call_Count
     (Self : Mock_File_System_Provider) return Natural;

   procedure Reset_Call_Count
     (Self : in out Mock_File_System_Provider);

--   Implement File_System_Provider interface
   overriding function Exists
     (Self : Mock_File_System_Provider;
      Path : String) return Boolean_Result.Result;

   overriding function Is_Directory
     (Self : Mock_File_System_Provider;
      Path : String) return Boolean_Result.Result;

   overriding function Is_Regular_File
     (Self : Mock_File_System_Provider;
      Path : String) return Boolean_Result.Result;

   overriding function Get_Extension
     (Self : Mock_File_System_Provider;
      Path : String) return String_Result.Result;

   overriding function Get_Base_Name
     (Self : Mock_File_System_Provider;
      Path : String) return String_Result.Result;

   overriding function Get_Directory_Name
     (Self : Mock_File_System_Provider;
      Path : String) return String_Result.Result;

   overriding function Compose_Path
     (Self      : Mock_File_System_Provider;
      Directory : String;
      Name      : String) return String_Result.Result;

   overriding function With_Extension
     (Self      : Mock_File_System_Provider;
      Path      : String;
      Extension : String) return String_Result.Result;

   overriding function Normalize_Path
     (Self : Mock_File_System_Provider;
      Path : String) return String_Result.Result;

   overriding function Get_Absolute_Path
     (Self : Mock_File_System_Provider;
      Path : String) return String_Result.Result;

   overriding function Get_File_Size
     (Self : Mock_File_System_Provider;
      Path : String) return Natural_Result.Result;

end Abohlib.Test.Mocks.File_System;
