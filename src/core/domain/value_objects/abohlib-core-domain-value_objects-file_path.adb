--  =============================================================================
--  Abohlib.Core.Domain.Value_Objects.File_Path - File Path Value Object Body
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Purpose:
--    Implementation of the simple file path value object that provides basic
--    file path functionality with validation and manipulation operations.
--
--  Usage:
--    See specification file for usage examples.
--  =============================================================================

pragma Ada_2022;

with Ada.Strings.Fixed;
with Ada.Strings; use Ada.Strings;

package body Abohlib.Core.Domain.Value_Objects.File_Path is

   use Ada.Strings.Fixed;

   --  =========================================================================
   --  Helper Functions
   --  =========================================================================

   function Contains_Path_Separator (Path : String) return Boolean is
   begin
      for C of Path loop
         if C = '/' or C = '\' then
            return True;
         end if;
      end loop;
      return False;
   end Contains_Path_Separator;

   function Simple_Path_Join (Directory, Name : String) return String is
      Dir_Trimmed  : constant String := Trim (Directory, Both);
      Name_Trimmed : constant String := Trim (Name, Both);
   begin
      if Dir_Trimmed'Length = 0 then
         return Name_Trimmed;
      elsif Name_Trimmed'Length = 0 then
         return Dir_Trimmed;
      else
         --  Simple path joining with platform-appropriate separator
         if Dir_Trimmed (Dir_Trimmed'Last) = '/'
           or Dir_Trimmed (Dir_Trimmed'Last) = '\'
         then
            return Dir_Trimmed & Name_Trimmed;
         else
            return Dir_Trimmed & "/" & Name_Trimmed;
         end if;
      end if;
   end Simple_Path_Join;

   function Simple_File_Name (Path : String) return String is
      --  Extract filename from path using pure string operations
      Last_Slash : Natural := 0;
   begin
      --  Find the last path separator
      for I in reverse Path'Range loop
         if Path (I) = '/' or Path (I) = '\' then
            Last_Slash := I;
            exit;
         end if;
      end loop;

      if Last_Slash = 0 then
         return Path;  --  No separator found, entire string is filename

      else
         return Path (Last_Slash + 1 .. Path'Last);
      end if;
   end Simple_File_Name;

   function Simple_Directory_Name (Path : String) return String is
      --  Extract directory portion from path using pure string operations
      Last_Slash : Natural := 0;
   begin
      --  Find the last path separator
      for I in reverse Path'Range loop
         if Path (I) = '/' or Path (I) = '\' then
            Last_Slash := I;
            exit;
         end if;
      end loop;

      if Last_Slash = 0 then
         return "";  --  No separator found, no directory part

      else
         return Path (Path'First .. Last_Slash - 1);
      end if;
   end Simple_Directory_Name;

   --  =========================================================================
   --  Constructors
   --  =========================================================================

   function Create
     (Path : String; Category : Path_Category := Input) return File_Path
   is
      Result : File_Path;
   begin
      -- This is now a private implementation that assumes valid input
      Result.Path := To_Unbounded_String (Path);
      Result.Category := Category;
      return Result;
   end Create;

   --  =========================================================================
   --  Accessors
   --  =========================================================================

   function Value (Self : File_Path) return String is
   begin
      return To_String (Self.Path);
   end Value;

   function Category (Self : File_Path) return Path_Category is
   begin
      return Self.Category;
   end Category;

   function File_Name (Self : File_Path) return String is
   begin
      return Simple_File_Name (Value (Self));
   exception
      when others =>
         return "";
   end File_Name;

   function File_Stem (Self : File_Path) return String is
      Name    : constant String := File_Name (Self);
      Dot_Pos : constant Natural :=
        Index (Name, ".", Going => Ada.Strings.Backward);
   begin
      if Dot_Pos > 0 then
         return Name (Name'First .. Dot_Pos - 1);
      else
         return Name;
      end if;
   end File_Stem;

   function Extension (Self : File_Path) return String is
      Name    : constant String := File_Name (Self);
      Dot_Pos : constant Natural :=
        Index (Name, ".", Going => Ada.Strings.Backward);
   begin
      if Dot_Pos > 0 and then Dot_Pos < Name'Last then
         return Name (Dot_Pos .. Name'Last);
      else
         return "";
      end if;
   end Extension;

   --  Note: Parent function moved to Results package for proper error handling

   --  =========================================================================
   --  Query Functions
   --  =========================================================================

   --  Exists function removed - use File_Path_Service instead

   --  Is_File function removed - use File_Path_Service instead

   --  Is_Directory function removed - use File_Path_Service instead

   function Is_Absolute (Self : File_Path) return Boolean is
      Path_Str : constant String := Value (Self);
   begin
      if Path_Str'Length = 0 then
         return False;
      end if;

      --  Unix/Linux/macOS absolute paths start with /
      --  Windows absolute paths start with drive letter followed by :
      return
        Path_Str (Path_Str'First) = '/'
        or else (Path_Str'Length >= 2
                 and then Path_Str (Path_Str'First + 1) = ':');
   end Is_Absolute;

   --  =========================================================================
   --  Path Manipulation
   --  =========================================================================
   --  Note: Path manipulation functions have been moved to the Results child
   --  package to provide Result-based error handling instead of exceptions.

   --  =========================================================================
   --  Validation
   --  =========================================================================

   function Is_Valid (Self : File_Path) return Boolean is
      Path_Str : constant String := Value (Self);
   begin
      --  Basic validation
      if Path_Str'Length = 0 or else Path_Str'Length > MAX_PATH_LENGTH then
         return False;
      end if;

      --  Check for null characters
      for C of Path_Str loop
         if C = ASCII.NUL then
            return False;
         end if;
      end loop;

      --  Category-specific validation
      case Self.Category is
         when Input =>
            --  Input paths should exist and be readable files
            return True; --  Existence check done at usage time

         when Output =>
            --  Output paths should have valid parent directory
            return True; --  Parent directory check done at usage time

         when Temporary =>
            --  Temporary paths have no special requirements
            return True;

         when Configuration =>
            --  Configuration paths should exist
            return True; --  Existence check done at usage time
      end case;
   end Is_Valid;

   --  =========================================================================
   --  Comparison
   --  =========================================================================

   overriding
   function "=" (Left, Right : File_Path) return Boolean is
   begin
      return Left.Path = Right.Path and then Left.Category = Right.Category;
   end "=";

   --  =========================================================================
   --  String Representation
   --  =========================================================================

   function Image (Self : File_Path) return String is
   begin
      return
        "File_Path("
        & Path_Category'Image (Self.Category)
        & ", """
        & Value (Self)
        & """)";
   end Image;

   --  =========================================================================
   --  Controlled Type Operations
   --  =========================================================================

   overriding
   procedure Initialize (Object : in out File_Path) is
   begin
      -- Set a valid default path to satisfy the type invariant
      -- Using "." (current directory) as it's always valid
      Object.Path := To_Unbounded_String (".");
      Object.Category := Input;
   end Initialize;

   overriding
   procedure Adjust (Object : in out File_Path) is
   begin
      --  Unbounded_String handles its own memory, nothing special needed
      null;
   end Adjust;

   overriding
   procedure Finalize (Object : in out File_Path) is
   begin
      --  Unbounded_String handles its own memory, nothing special needed
      null;
   end Finalize;

end Abohlib.Core.Domain.Value_Objects.File_Path;
