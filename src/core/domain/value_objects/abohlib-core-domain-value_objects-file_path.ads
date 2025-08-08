--  =============================================================================
--  Abohlib.Core.Domain.Value_Objects.File_Path - File Path Value Object
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Purpose:
--    A simple, non-generic file path value object that provides basic file path
--    functionality without the complexity of generic instantiation. This provides
--    immutable, self-validating representations of file system paths.
--
--  Usage:
--    Path : constant File_Path := Create ("/home/user/data.txt", Input);
--    if Path.Exists then
--       Process_File (Path.Value);
--    end if;
--  =============================================================================

pragma Ada_2022;

with Ada.Finalization;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;
--  Removed unused: with Abohlib.Core.Domain.Ports.File_System;

package Abohlib.Core.Domain.Value_Objects.File_Path is

   --  Removed unused: use Abohlib.Core.Domain.Ports.File_System;

   --  Simple file path value object
   type File_Path is new Ada.Finalization.Controlled with private
   with Type_Invariant => Is_Valid (File_Path);

   --  Maximum path length constraint
   MAX_PATH_LENGTH : constant := 4_096;  --  Common filesystem limit

   --  Path categories for validation
   type Path_Category is (Input, Output, Temporary, Configuration);

   --  Constructors
   function Create
     (Path : String; Category : Path_Category := Input) return File_Path
   with
     Pre => Path'Length > 0 and then Path'Length <= MAX_PATH_LENGTH,
     Post => Is_Valid (Create'Result);
   --  Creates a new file path with validation

   --  Accessors
   function Value (Self : File_Path) return String
   with Inline;
   --  Gets the underlying path string

   function As_Path (Self : File_Path) return String renames Value;
   --  Alias for compatibility

   function To_String (Self : File_Path) return String renames Value;
   --  Gets the path as a string

   function Category (Self : File_Path) return Path_Category
   with Inline;
   --  Gets the path category

   function File_Name (Self : File_Path) return String
   with
     Pre => Is_Valid (Self),
     Post =>
       File_Name'Result'Length > 0
       and then File_Name'Result'Length <= Value (Self)'Length
       and then not Contains_Path_Separator (File_Name'Result);
   --  Gets the file name component

   function File_Stem (Self : File_Path) return String
   with
     Pre => Is_Valid (Self),
     Post =>
       File_Stem'Result'Length >= 0
       and then File_Stem'Result'Length <= File_Name (Self)'Length;
   --  Gets the file stem (name without extension)

   function Extension (Self : File_Path) return String
   with
     Pre => Is_Valid (Self),
     Post =>
       Extension'Result'Length >= 0
       and then (Extension'Result'Length = 0
                 or else (Extension'Result (Extension'Result'First) = '.'
                          and then Extension'Result'Length <= 255));
   --  Gets the file extension

   --  Note: Parent function moved to Results package for proper error handling

   --  Query functions
   function Is_Absolute (Self : File_Path) return Boolean;
   --  Checks if the path is absolute (pure string operation)

   --  Path manipulation
   --  Note: Use the Result-based versions in File_Path.Results package
   --  for operations that can fail. These are provided for convenience
   --  when you're certain the operations will succeed.

   --  Validation
   function Is_Valid (Self : File_Path) return Boolean;
   --  Validates the file path

   --  Helper function for contracts
   function Contains_Path_Separator (Path : String) return Boolean;

   --  Comparison
   overriding
   function "=" (Left, Right : File_Path) return Boolean;

   --  String representation
   function Image (Self : File_Path) return String;

private

   type File_Path is new Ada.Finalization.Controlled with record
      Path     : Unbounded_String;
      Category : Path_Category;
   end record;

   overriding
   procedure Initialize (Object : in out File_Path);
   overriding
   procedure Adjust (Object : in out File_Path);
   overriding
   procedure Finalize (Object : in out File_Path);

   --  Helper functions for child packages and contracts
   function Simple_Path_Join (Directory, Name : String) return String
   with
     Pre =>
       Directory'Length > 0
       and then Name'Length > 0
       and then Directory'Length + Name'Length + 1 <= MAX_PATH_LENGTH
       and then not Contains_Path_Separator (Name),
     Post =>
       Simple_Path_Join'Result'Length > 0
       and then Simple_Path_Join'Result'Length
                <= Directory'Length + Name'Length + 1
       and then Contains_Path_Separator (Simple_Path_Join'Result);

   function Simple_Directory_Name (Path : String) return String;
end Abohlib.Core.Domain.Value_Objects.File_Path;
