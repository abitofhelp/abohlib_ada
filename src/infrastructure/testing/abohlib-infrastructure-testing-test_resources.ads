--  =============================================================================
--  Abohlib.Infrastructure.Testing.Test_Resources - Simple Resource Tracking
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================
--  Purpose:
--    Provides simple resource tracking for test cleanup.
--    Tracks files and directories created during tests for automatic cleanup.
--  =============================================================================

pragma Ada_2022;
pragma Unevaluated_Use_Of_Old (Allow);

with Ada.Finalization;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Abohlib.Core.Domain.Errors;
with Abohlib.Core.Domain.Result;

package Abohlib.Infrastructure.Testing.Test_Resources is

   --  ==========================================================================
   --  Constants
   --  ==========================================================================
   
   --  Default temporary directory for test resources
   Default_Temp_Directory : constant String := "/tmp/";

   --  ==========================================================================
   --  Error Types
   --  ==========================================================================
   
   type Test_Resource_Error is record
      Base : Abohlib.Core.Domain.Errors.Domain_Error;
   end record;
   
   --  Result types
   package String_Result is new Abohlib.Core.Domain.Result.Result_Package
     (Ok_Type => Unbounded_String, Err_Type => Test_Resource_Error);
     
   package Void_Result is new Abohlib.Core.Domain.Result.Result_Package
     (Ok_Type => Boolean, Err_Type => Test_Resource_Error);

   --  ==========================================================================
   --  Resource Tracker Type
   --  ==========================================================================

   type Resource_Tracker is new Ada.Finalization.Limited_Controlled with private;

   --  ==========================================================================
   --  Resource Management
   --  ==========================================================================

   --  Track a file for cleanup
   procedure Track_File
     (Tracker : in out Resource_Tracker;
      Path    : String)
   with
     Pre => Path'Length > 0,
     Post => Resource_Count (Tracker) = Resource_Count (Tracker)'Old + 1;

   --  Track a directory for cleanup
   procedure Track_Directory
     (Tracker : in out Resource_Tracker;
      Path    : String)
   with
     Pre => Path'Length > 0,
     Post => Resource_Count (Tracker) = Resource_Count (Tracker)'Old + 1;

   --  Create and track a temporary file
   function Create_Temp_File
     (Tracker : in out Resource_Tracker;
      Name    : String := "") return String_Result.Result
   with
     Post => String_Result.Is_Ok (Create_Temp_File'Result) or else
             String_Result.Is_Err (Create_Temp_File'Result);

   --  Create and track a temporary directory
   function Create_Temp_Directory
     (Tracker : in out Resource_Tracker;
      Name    : String := "") return String_Result.Result
   with
     Post => String_Result.Is_Ok (Create_Temp_Directory'Result) or else
             String_Result.Is_Err (Create_Temp_Directory'Result);

   --  Manual cleanup (automatic cleanup happens on finalization)
   function Cleanup (Tracker : in out Resource_Tracker) return Void_Result.Result
   with
     Post => (if Void_Result.Is_Ok (Cleanup'Result) then
                Resource_Count (Tracker) = 0);

   --  Get number of tracked resources
   function Resource_Count (Tracker : Resource_Tracker) return Natural
   with
     Post => Resource_Count'Result >= 0;

private

   type Resource_Kind is (File_Resource, Directory_Resource);

   type Resource_Entry is record
      Kind : Resource_Kind;
      Path : Unbounded_String;
   end record;

   package Resource_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Resource_Entry);

   type Resource_Tracker is new Ada.Finalization.Limited_Controlled with record
      Resources : Resource_Vectors.Vector;
   end record;

   overriding procedure Finalize (Tracker : in out Resource_Tracker);

end Abohlib.Infrastructure.Testing.Test_Resources;