--  =============================================================================
--  Abohlib.Infrastructure.Testing.Test_Resources - Implementation
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

with Ada.Directories;
with Ada.Text_IO;
with Abohlib.Infrastructure.Testing.Test_Data;
with Abohlib.Core.Domain.Errors; use Abohlib.Core.Domain.Errors;

package body Abohlib.Infrastructure.Testing.Test_Resources is

   --  ==========================================================================
   --  Error Helpers
   --  ==========================================================================
   
   function Make_Resource_Error (Resource : String; Message : String) return Test_Resource_Error is
   begin
      return Test_Resource_Error'(
         Base => Make_Resource_Error(
            Kind => Unavailable,
            Resource_Type => "Test Resource",
            Message => "Error with " & Resource & ": " & Message,
            Recovery => "Check permissions and disk space"
         ).Base
      );
   end Make_Resource_Error;

   --  ==========================================================================
   --  Internal Utilities
   --  ==========================================================================

   procedure Delete_Tree (Path : String) is
      use Ada.Directories;
   begin
      if Exists (Path) then
         if Kind (Path) = Directory then
            --  Delete contents first
            declare
               Search : Search_Type;
               Dir_Entry : Directory_Entry_Type;
            begin
               Start_Search (Search, Path, "*");
               while More_Entries (Search) loop
                  Get_Next_Entry (Search, Dir_Entry);
                  declare
                     Name : constant String := Simple_Name (Dir_Entry);
                  begin
                     if Name /= "." and then Name /= ".." then
                        Delete_Tree (Full_Name (Dir_Entry));
                     end if;
                  end;
               end loop;
               End_Search (Search);
            end;
            Delete_Directory (Path);
         else
            Delete_File (Path);
         end if;
      end if;
   exception
      when others =>
         null; -- Ignore errors during cleanup
   end Delete_Tree;

   --  ==========================================================================
   --  Resource Management
   --  ==========================================================================

   procedure Track_File
     (Tracker : in out Resource_Tracker;
      Path    : String) is
   begin
      Tracker.Resources.Append
        (Resource_Entry'(
           Kind => File_Resource,
           Path => To_Unbounded_String (Path)));
   end Track_File;

   procedure Track_Directory
     (Tracker : in out Resource_Tracker;
      Path    : String) is
   begin
      Tracker.Resources.Append
        (Resource_Entry'(
           Kind => Directory_Resource,
           Path => To_Unbounded_String (Path)));
   end Track_Directory;

   function Create_Temp_File
     (Tracker : in out Resource_Tracker;
      Name    : String := "") return String_Result.Result is
      use Abohlib.Infrastructure.Testing.Test_Data;
      Filename : constant String := 
         (if Name = "" then Unique_Test_Filename else Name);
      Path : constant String := Default_Temp_Directory & Filename;
   begin
      -- Create empty file
      begin
         declare
            use Ada.Text_IO;
            File : File_Type;
         begin
            Create (File, Out_File, Path);
            Close (File);
         end;
      exception
         when others =>
            return String_Result.Err (Make_Resource_Error (Path, "Failed to create temporary file"));
      end;
      
      Track_File (Tracker, Path);
      return String_Result.Ok (To_Unbounded_String (Path));
   end Create_Temp_File;

   function Create_Temp_Directory
     (Tracker : in out Resource_Tracker;
      Name    : String := "") return String_Result.Result is
      use Abohlib.Infrastructure.Testing.Test_Data;
      Dirname : constant String := 
         (if Name = "" then Unique_Test_Filename ("testdir_", "") else Name);
      Path : constant String := Default_Temp_Directory & Dirname;
   begin
      begin
         Ada.Directories.Create_Directory (Path);
      exception
         when others =>
            return String_Result.Err (Make_Resource_Error (Path, "Failed to create temporary directory"));
      end;
      
      Track_Directory (Tracker, Path);
      return String_Result.Ok (To_Unbounded_String (Path));
   end Create_Temp_Directory;

   function Cleanup (Tracker : in out Resource_Tracker) return Void_Result.Result is
   begin
      -- Clean up in reverse order (LIFO)
      for I in reverse 1 .. Natural (Tracker.Resources.Length) loop
         declare
            Resource : constant Resource_Entry := 
               Tracker.Resources.Element (I);
            Path : constant String := To_String (Resource.Path);
         begin
            begin
               Delete_Tree (Path);
            exception
               when others =>
                  return Void_Result.Err (Make_Resource_Error (Path, "Failed to delete resource"));
            end;
         end;
      end loop;
      Tracker.Resources.Clear;
      return Void_Result.Ok (True);
   end Cleanup;

   function Resource_Count (Tracker : Resource_Tracker) return Natural is
   begin
      return Natural (Tracker.Resources.Length);
   end Resource_Count;

   overriding procedure Finalize (Tracker : in out Resource_Tracker) is
      Ignored : constant Void_Result.Result := Cleanup (Tracker);
      pragma Unreferenced (Ignored);
   begin
      -- Cleanup errors are intentionally ignored during finalization
      null;
   end Finalize;

end Abohlib.Infrastructure.Testing.Test_Resources;