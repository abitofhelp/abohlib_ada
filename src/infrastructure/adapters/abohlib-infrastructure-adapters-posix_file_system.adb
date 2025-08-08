--  =============================================================================
--  Abohlib.Infrastructure.Adapters.POSIX_File_System - Implementation
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

with Ada.Directories;
--  Removed unused: with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.IO_Exceptions;

package body Abohlib.Infrastructure.Adapters.POSIX_File_System is

   use Ada.Strings.Unbounded;
   --  Removed unused: use Ada.Strings.Fixed;
   use Ada.Directories;

   -- ----------
   --  Exists
   -- ----------

   overriding
   function Exists
     (Self : POSIX_File_System_Provider; Path : String)
      return Boolean_Result.Result
   is
      pragma Unreferenced (Self);
   begin
      return Boolean_Result.Ok (Ada.Directories.Exists (Path));
   exception
      when Ada.IO_Exceptions.Name_Error =>
         return
           Boolean_Result.Err
             (Make_File_System_Error
                (Invalid_Path,
                 "Invalid path format",
                 Path,
                 "Ada.Directories.Exists"));
      when others =>
         return
           Boolean_Result.Err
             (Make_File_System_Error
                (System_Error,
                 "Unexpected error checking file existence",
                 Path));
   end Exists;

   -- ------------------
   --  Is_Directory
   -- ------------------

   overriding
   function Is_Directory
     (Self : POSIX_File_System_Provider; Path : String)
      return Boolean_Result.Result
   is
      pragma Unreferenced (Self);
   begin
      if not Ada.Directories.Exists (Path) then
         return Boolean_Result.Ok (False);
      end if;

      return
        Boolean_Result.Ok
          (Ada.Directories.Kind (Path) = Ada.Directories.Directory);
   exception
      when Ada.IO_Exceptions.Name_Error =>
         return
           Boolean_Result.Err
             (Make_File_System_Error
                (Invalid_Path,
                 "Invalid path format",
                 Path,
                 "Ada.Directories.Kind"));
      when others =>
         return
           Boolean_Result.Err
             (Make_File_System_Error
                (System_Error,
                 "Unexpected error checking if directory",
                 Path));
   end Is_Directory;

   -- ----------------------
   --  Is_Regular_File
   -- ----------------------

   overriding
   function Is_Regular_File
     (Self : POSIX_File_System_Provider; Path : String)
      return Boolean_Result.Result
   is
      pragma Unreferenced (Self);
   begin
      if not Ada.Directories.Exists (Path) then
         return Boolean_Result.Ok (False);
      end if;

      return
        Boolean_Result.Ok
          (Ada.Directories.Kind (Path) = Ada.Directories.Ordinary_File);
   exception
      when Ada.IO_Exceptions.Name_Error =>
         return
           Boolean_Result.Err
             (Make_File_System_Error
                (Invalid_Path,
                 "Invalid path format",
                 Path,
                 "Ada.Directories.Kind"));
      when others =>
         return
           Boolean_Result.Err
             (Make_File_System_Error
                (System_Error,
                 "Unexpected error checking if regular file",
                 Path));
   end Is_Regular_File;

   -- -------------------
   --  Get_Extension
   -- -------------------

   overriding
   function Get_Extension
     (Self : POSIX_File_System_Provider; Path : String)
      return String_Result.Result
   is
      pragma Unreferenced (Self);
      Ext : constant String := Ada.Directories.Extension (Path);
   begin
      if Ext'Length > 0 then
         return String_Result.Ok (To_Unbounded_String ("." & Ext));
      else
         return String_Result.Ok (To_Unbounded_String (""));
      end if;
   exception
      when Ada.IO_Exceptions.Name_Error =>
         return
           String_Result.Err
             (Make_File_System_Error
                (Invalid_Path,
                 "Invalid path format",
                 Path,
                 "Ada.Directories.Extension"));
      when others =>
         return
           String_Result.Err
             (Make_File_System_Error
                (System_Error, "Unexpected error getting extension", Path));
   end Get_Extension;

   -- -------------------
   --  Get_Base_Name
   -- -------------------

   overriding
   function Get_Base_Name
     (Self : POSIX_File_System_Provider; Path : String)
      return String_Result.Result
   is
      pragma Unreferenced (Self);
   begin
      return
        String_Result.Ok
          (To_Unbounded_String (Ada.Directories.Simple_Name (Path)));
   exception
      when Ada.IO_Exceptions.Name_Error =>
         return
           String_Result.Err
             (Make_File_System_Error
                (Invalid_Path,
                 "Invalid path format",
                 Path,
                 "Ada.Directories.Simple_Name"));
      when others =>
         return
           String_Result.Err
             (Make_File_System_Error
                (System_Error, "Unexpected error getting base name", Path));
   end Get_Base_Name;

   -- ------------------------
   --  Get_Directory_Name
   -- ------------------------

   overriding
   function Get_Directory_Name
     (Self : POSIX_File_System_Provider; Path : String)
      return String_Result.Result
   is
      pragma Unreferenced (Self);
   begin
      return
        String_Result.Ok
          (To_Unbounded_String (Ada.Directories.Containing_Directory (Path)));
   exception
      when Ada.IO_Exceptions.Name_Error =>
         return
           String_Result.Err
             (Make_File_System_Error
                (Invalid_Path,
                 "Invalid path format",
                 Path,
                 "Ada.Directories.Containing_Directory"));
      when others =>
         return
           String_Result.Err
             (Make_File_System_Error
                (System_Error,
                 "Unexpected error getting directory name",
                 Path));
   end Get_Directory_Name;

   -- -------------------
   --  Compose_Path
   -- -------------------

   overriding
   function Compose_Path
     (Self : POSIX_File_System_Provider; Directory : String; Name : String)
      return String_Result.Result
   is
      pragma Unreferenced (Self);
   begin
      return
        String_Result.Ok
          (To_Unbounded_String (Ada.Directories.Compose (Directory, Name)));
   exception
      when Ada.IO_Exceptions.Name_Error =>
         return
           String_Result.Err
             (Make_File_System_Error
                (Invalid_Path,
                 "Invalid path components",
                 Directory & "/" & Name,
                 "Ada.Directories.Compose"));
      when others =>
         return
           String_Result.Err
             (Make_File_System_Error
                (System_Error,
                 "Unexpected error composing path",
                 Directory & "/" & Name));
   end Compose_Path;

   -- --------------------
   --  With_Extension
   -- --------------------

   overriding
   function With_Extension
     (Self : POSIX_File_System_Provider; Path : String; Extension : String)
      return String_Result.Result
   is
      pragma Unreferenced (Self);
      Base : constant String := Ada.Directories.Base_Name (Path);
      Dir  : constant String := Ada.Directories.Containing_Directory (Path);
      -- Ada.Directories.Compose expects extension WITHOUT dot
      Ext  : constant String :=
        (if Extension'Length > 0 and then Extension (Extension'First) = '.'
         then Extension (Extension'First + 1 .. Extension'Last)
         else Extension);
   begin
      if Dir'Length > 0 then
         return
           String_Result.Ok
             (To_Unbounded_String (Ada.Directories.Compose (Dir, Base, Ext)));
      else
         return
           String_Result.Ok
             (To_Unbounded_String (Ada.Directories.Compose ("", Base, Ext)));
      end if;
   exception
      when Ada.IO_Exceptions.Name_Error =>
         return
           String_Result.Err
             (Make_File_System_Error
                (Invalid_Path,
                 "Invalid path format",
                 Path,
                 "Ada.Directories path manipulation"));
      when others =>
         return
           String_Result.Err
             (Make_File_System_Error
                (System_Error, "Unexpected error changing extension", Path));
   end With_Extension;

   -- ----------------------
   --  Normalize_Path
   -- ----------------------

   overriding
   function Normalize_Path
     (Self : POSIX_File_System_Provider; Path : String)
      return String_Result.Result
   is
      pragma Unreferenced (Self);
   begin
      --  For now, just return the path as-is
      --  A full implementation would resolve ".." and "." components
      return String_Result.Ok (To_Unbounded_String (Path));
   exception
      when others =>
         return
           String_Result.Err
             (Make_File_System_Error
                (System_Error, "Unexpected error normalizing path", Path));
   end Normalize_Path;

   -- ------------------------
   --  Get_Absolute_Path
   -- ------------------------

   overriding
   function Get_Absolute_Path
     (Self : POSIX_File_System_Provider; Path : String)
      return String_Result.Result
   is
      pragma Unreferenced (Self);
   begin
      -- If already absolute, return as-is
      if Path'Length > 0 and then Path (Path'First) = '/' then
         return String_Result.Ok (To_Unbounded_String (Path));
      else
         -- Use Full_Name for relative paths
         return
           String_Result.Ok
             (To_Unbounded_String (Ada.Directories.Full_Name (Path)));
      end if;
   exception
      when Ada.IO_Exceptions.Name_Error =>
         return
           String_Result.Err
             (Make_File_System_Error
                (Invalid_Path,
                 "Invalid path format",
                 Path,
                 "Ada.Directories.Full_Name"));
      when others =>
         return
           String_Result.Err
             (Make_File_System_Error
                (System_Error,
                 "Unexpected error getting absolute path",
                 Path));
   end Get_Absolute_Path;

   -- -------------------
   --  Get_File_Size
   -- -------------------

   overriding
   function Get_File_Size
     (Self : POSIX_File_System_Provider; Path : String)
      return Natural_Result.Result
   is
      pragma Unreferenced (Self);
   begin
      if not Ada.Directories.Exists (Path) then
         return
           Natural_Result.Err
             (Make_File_System_Error
                (File_Not_Found, "File does not exist", Path));
      end if;

      return Natural_Result.Ok (Natural (Ada.Directories.Size (Path)));
   exception
      when Ada.IO_Exceptions.Name_Error =>
         return
           Natural_Result.Err
             (Make_File_System_Error
                (Invalid_Path,
                 "Invalid path format",
                 Path,
                 "Ada.Directories.Size"));
      when others =>
         return
           Natural_Result.Err
             (Make_File_System_Error
                (System_Error, "Unexpected error getting file size", Path));
   end Get_File_Size;

end Abohlib.Infrastructure.Adapters.POSIX_File_System;
