--  =============================================================================
--  Abohlib.Core.Domain.Value_Objects.File_Path.Results - File Path Result Types
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Purpose:
--    Provides Result types for File_Path operations that can fail.
--    This child package resolves circular dependency issues with the
--    parent package's private type definition.
--  =============================================================================

pragma Ada_2022;

with Abohlib.Core.Domain.Result;

package Abohlib.Core.Domain.Value_Objects.File_Path.Results is

   --  Result types for File_Path operations
   package Path_Result is new
     Abohlib.Core.Domain.Result.Result_Package
       (Ok_Type  => File_Path,
        Err_Type => Unbounded_String);

   --  Safe constructor returning Result type
   function Create_Safe
     (Path : String; Category : Path_Category := Input)
      return Path_Result.Result
   with Pre => Path'Length > 0 and then Path'Length <= MAX_PATH_LENGTH;

   --  Safe operations returning Result types
   function With_Extension_Safe
     (Self : File_Path; Extension : String) return Path_Result.Result
   with
     Pre =>
       Is_Valid (Self)
       and then Extension'Length > 0
       and then Extension'Length <= 255
       and then Extension (Extension'First) = '.';

   function Join_Safe
     (Self : File_Path; Component : String) return Path_Result.Result
   with
     Pre =>
       Is_Valid (Self)
       and then Component'Length > 0
       and then Value (Self)'Length + Component'Length + 1 <= MAX_PATH_LENGTH;

   function Parent_Safe (Self : File_Path) return Path_Result.Result
   with
     Pre => Is_Valid (Self),
     Post =>
       (if Path_Result.Is_Ok (Parent_Safe'Result)
        then
          Is_Valid (Path_Result.Get_Ok (Parent_Safe'Result))
          and then Value (Path_Result.Get_Ok (Parent_Safe'Result))'Length
                   < Value (Self)'Length);

end Abohlib.Core.Domain.Value_Objects.File_Path.Results;
