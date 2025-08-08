--  =============================================================================
--  Abohlib.Infrastructure.Adapters.POSIX_File_System - POSIX File System Adapter
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Purpose:
--    POSIX-specific implementation of the File_System_Provider interface.
--    Provides file system operations using Ada.Directories and POSIX APIs.
--  =============================================================================

pragma Ada_2022;

with Abohlib.Core.Domain.Ports.File_System;

package Abohlib.Infrastructure.Adapters.POSIX_File_System is

   use Abohlib.Core.Domain.Ports.File_System;

   --  POSIX implementation of file system provider
   type POSIX_File_System_Provider is limited new File_System_Provider
   with null record;

   --  Path validation and queries
   overriding
   function Exists
     (Self : POSIX_File_System_Provider; Path : String)
      return Boolean_Result.Result;

   overriding
   function Is_Directory
     (Self : POSIX_File_System_Provider; Path : String)
      return Boolean_Result.Result;

   overriding
   function Is_Regular_File
     (Self : POSIX_File_System_Provider; Path : String)
      return Boolean_Result.Result;

   --  Path manipulation
   overriding
   function Get_Extension
     (Self : POSIX_File_System_Provider; Path : String)
      return String_Result.Result;

   overriding
   function Get_Base_Name
     (Self : POSIX_File_System_Provider; Path : String)
      return String_Result.Result;

   overriding
   function Get_Directory_Name
     (Self : POSIX_File_System_Provider; Path : String)
      return String_Result.Result;

   overriding
   function Compose_Path
     (Self : POSIX_File_System_Provider; Directory : String; Name : String)
      return String_Result.Result;

   overriding
   function With_Extension
     (Self : POSIX_File_System_Provider; Path : String; Extension : String)
      return String_Result.Result;

   --  Path normalization
   overriding
   function Normalize_Path
     (Self : POSIX_File_System_Provider; Path : String)
      return String_Result.Result;

   overriding
   function Get_Absolute_Path
     (Self : POSIX_File_System_Provider; Path : String)
      return String_Result.Result;

   --  File operations
   overriding
   function Get_File_Size
     (Self : POSIX_File_System_Provider; Path : String)
      return Natural_Result.Result;

end Abohlib.Infrastructure.Adapters.POSIX_File_System;
