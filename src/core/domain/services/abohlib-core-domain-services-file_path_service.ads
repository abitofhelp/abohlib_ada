--  =============================================================================
--  Abohlib.Core.Domain.Services.File_Path_Service - File Path Domain Service
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Purpose:
--    Domain service for file path operations using dependency injection.
--    Provides safe file system operations without violating Clean Architecture.
--  =============================================================================

pragma Ada_2022;

with Abohlib.Core.Domain.Ports.File_System;
with Abohlib.Core.Domain.Value_Objects.File_Path;
--  Removed unused: with Ada.Strings.Unbounded;

package Abohlib.Core.Domain.Services.File_Path_Service is

   use Abohlib.Core.Domain.Ports.File_System;
   use Abohlib.Core.Domain.Value_Objects.File_Path;
   --  Removed unused: use Ada.Strings.Unbounded;

   --  File path service using dependency injection
   type File_Path_Service_Type
     (Provider : not null access File_System_Provider'Class)
   is
     limited private;

   --  Create a new file path service with the given provider
   function Create
     (Provider : not null access File_System_Provider'Class)
      return File_Path_Service_Type;

   --  File system queries using the injected provider
   function Exists
     (Service : File_Path_Service_Type; Path : File_Path)
      return Boolean_Result.Result;

   function Is_Directory
     (Service : File_Path_Service_Type; Path : File_Path)
      return Boolean_Result.Result;

   function Is_Regular_File
     (Service : File_Path_Service_Type; Path : File_Path)
      return Boolean_Result.Result;

   function Get_File_Size
     (Service : File_Path_Service_Type; Path : File_Path)
      return Natural_Result.Result;

   --  Path manipulation operations
   function Get_Extension
     (Service : File_Path_Service_Type; Path : File_Path)
      return String_Result.Result;

   function Get_Base_Name
     (Service : File_Path_Service_Type; Path : File_Path)
      return String_Result.Result;

   function Get_Directory_Name
     (Service : File_Path_Service_Type; Path : File_Path)
      return String_Result.Result;

   function With_Extension
     (Service : File_Path_Service_Type; Path : File_Path; Extension : String)
      return String_Result.Result
   with Pre => Extension'Length > 0;

   function Join_Path
     (Service : File_Path_Service_Type; Directory : File_Path; Name : String)
      return String_Result.Result
   with Pre => Name'Length > 0;

   function Get_Absolute_Path
     (Service : File_Path_Service_Type; Path : File_Path)
      return String_Result.Result;

private

   type File_Path_Service_Type
     (Provider : not null access File_System_Provider'Class)
   is limited null record;

end Abohlib.Core.Domain.Services.File_Path_Service;
