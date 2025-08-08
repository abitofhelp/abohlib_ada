--  =============================================================================
--  Abohlib.Core.Domain.Ports.File_System - File System Port Interface
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Purpose:
--    Port interface for file system operations following hexagonal architecture.
--    Abstracts file system specific operations from the domain layer.
--  =============================================================================

pragma Ada_2022;

with Ada.Strings.Unbounded;
with Abohlib.Core.Domain.Result;

package Abohlib.Core.Domain.Ports.File_System is

   use Ada.Strings.Unbounded;

   --  File system error types
   type File_System_Error_Kind is
     (File_Not_Found,
      Access_Denied,
      Path_Too_Long,
      Invalid_Path,
      System_Error,
      Operation_Not_Supported);

   type File_System_Error is record
      Kind    : File_System_Error_Kind;
      Message : Unbounded_String;
      Path    : Unbounded_String;
      Context : Unbounded_String;
   end record;

   --  Result types for file system operations
   package Boolean_Result is new
     Abohlib.Core.Domain.Result.Result_Package
       (Ok_Type  => Boolean,
        Err_Type => File_System_Error);

   package String_Result is new
     Abohlib.Core.Domain.Result.Result_Package
       (Ok_Type  => Unbounded_String,
        Err_Type => File_System_Error);

   package Natural_Result is new
     Abohlib.Core.Domain.Result.Result_Package
       (Ok_Type  => Natural,
        Err_Type => File_System_Error);

   --  File system provider interface
   type File_System_Provider is limited interface;

   --  Path validation and queries
   function Exists
     (Self : File_System_Provider; Path : String) return Boolean_Result.Result
   is abstract
   with Pre'Class => Path'Length > 0;

   function Is_Directory
     (Self : File_System_Provider; Path : String) return Boolean_Result.Result
   is abstract
   with Pre'Class => Path'Length > 0;

   function Is_Regular_File
     (Self : File_System_Provider; Path : String) return Boolean_Result.Result
   is abstract
   with Pre'Class => Path'Length > 0;

   --  Path manipulation
   function Get_Extension
     (Self : File_System_Provider; Path : String) return String_Result.Result
   is abstract
   with Pre'Class => Path'Length > 0;

   function Get_Base_Name
     (Self : File_System_Provider; Path : String) return String_Result.Result
   is abstract
   with Pre'Class => Path'Length > 0;

   function Get_Directory_Name
     (Self : File_System_Provider; Path : String) return String_Result.Result
   is abstract
   with Pre'Class => Path'Length > 0;

   function Compose_Path
     (Self : File_System_Provider; Directory : String; Name : String)
      return String_Result.Result
   is abstract
   with Pre'Class => Directory'Length > 0 and Name'Length > 0;

   function With_Extension
     (Self : File_System_Provider; Path : String; Extension : String)
      return String_Result.Result
   is abstract
   with Pre'Class => Path'Length > 0;

   --  Path normalization
   function Normalize_Path
     (Self : File_System_Provider; Path : String) return String_Result.Result
   is abstract
   with Pre'Class => Path'Length > 0;

   function Get_Absolute_Path
     (Self : File_System_Provider; Path : String) return String_Result.Result
   is abstract
   with Pre'Class => Path'Length > 0;

   --  File operations
   function Get_File_Size
     (Self : File_System_Provider; Path : String) return Natural_Result.Result
   is abstract
   with Pre'Class => Path'Length > 0;

   --  Helper functions for creating errors
   function Make_File_System_Error
     (Kind    : File_System_Error_Kind;
      Message : String;
      Path    : String := "";
      Context : String := "") return File_System_Error;

end Abohlib.Core.Domain.Ports.File_System;
