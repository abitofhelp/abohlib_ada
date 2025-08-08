--  =============================================================================
--  Abohlib.Core.Domain.Value_Objects.File_Path.Results - Implementation
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

with Ada.Exceptions;

package body Abohlib.Core.Domain.Value_Objects.File_Path.Results is

   --  Helper functions are now imported from parent package's private part

   --  =========================================================================
   --  Safe Result-based Operations
   --  =========================================================================

   function Create_Safe
     (Path : String; Category : Path_Category := Input)
      return Path_Result.Result is
   begin
      if Path'Length = 0 then
         return
           Path_Result.Err (To_Unbounded_String ("Empty path not allowed"));
      end if;

      if Path'Length > MAX_PATH_LENGTH then
         return
           Path_Result.Err
             (To_Unbounded_String
                ("Path length "
                 & Path'Length'Image
                 & " exceeds maximum "
                 & MAX_PATH_LENGTH'Image
                 & " characters"));
      end if;

      declare
         Result : constant File_Path := Create (Path, Category);
      begin
         if not Is_Valid (Result) then
            return
              Path_Result.Err (To_Unbounded_String ("Invalid path: " & Path));
         end if;

         return Path_Result.Ok (Result);
      end;
   exception
      when E : others =>
         return
           Path_Result.Err
             (To_Unbounded_String
                ("Unexpected error creating path: "
                 & Ada.Exceptions.Exception_Message (E)));
   end Create_Safe;

   function With_Extension_Safe
     (Self : File_Path; Extension : String) return Path_Result.Result is
   begin
      declare
         Base     : constant String := File_Stem (Self);
         Dir      : constant String := Simple_Directory_Name (Value (Self));
         New_Name : constant String :=
           (if Extension'Length > 0 and then Extension (Extension'First) /= '.'
            then Base & "." & Extension
            else Base & Extension);
         New_Path : constant String := Simple_Path_Join (Dir, New_Name);
      begin
         return Create_Safe (New_Path, Category (Self));
      end;
   exception
      when E : others =>
         return
           Path_Result.Err
             (To_Unbounded_String
                ("Cannot change extension: "
                 & Ada.Exceptions.Exception_Message (E)));
   end With_Extension_Safe;

   function Join_Safe
     (Self : File_Path; Component : String) return Path_Result.Result is
   begin
      --  Directory check removed - use File_Path_Service for file system queries
      declare
         New_Path : constant String :=
           Simple_Path_Join (Value (Self), Component);
      begin
         return Create_Safe (New_Path, Category (Self));
      end;
   exception
      when E : others =>
         return
           Path_Result.Err
             (To_Unbounded_String
                ("Cannot join path components: "
                 & Ada.Exceptions.Exception_Message (E)));
   end Join_Safe;

   function Parent_Safe (Self : File_Path) return Path_Result.Result is
   begin
      declare
         Parent_Path : constant String := Simple_Directory_Name (Value (Self));
      begin
         if Parent_Path = Value (Self) then
            return
              Path_Result.Err
                (To_Unbounded_String ("Path has no parent directory"));
         end if;

         return Create_Safe (Parent_Path, Category (Self));
      end;
   exception
      when E : others =>
         return
           Path_Result.Err
             (To_Unbounded_String
                ("Cannot get parent directory: "
                 & Ada.Exceptions.Exception_Message (E)));
   end Parent_Safe;

end Abohlib.Core.Domain.Value_Objects.File_Path.Results;
