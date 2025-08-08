--  =============================================================================
--  Abohlib.Core.Domain.Services.File_Path_Service - Implementation
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

package body Abohlib.Core.Domain.Services.File_Path_Service is

   -- ------------
   --  Create
   -- ------------

   function Create
     (Provider : not null access File_System_Provider'Class)
      return File_Path_Service_Type is
   begin
      return (Provider => Provider);
   end Create;

   -- ------------
   --  Exists
   -- ------------

   function Exists
     (Service : File_Path_Service_Type; Path : File_Path)
      return Boolean_Result.Result is
   begin
      return Service.Provider.Exists (Path.Value);
   end Exists;

   -- ------------------
   --  Is_Directory
   -- ------------------

   function Is_Directory
     (Service : File_Path_Service_Type; Path : File_Path)
      return Boolean_Result.Result is
   begin
      return Service.Provider.Is_Directory (Path.Value);
   end Is_Directory;

   -- ----------------------
   --  Is_Regular_File
   -- ----------------------

   function Is_Regular_File
     (Service : File_Path_Service_Type; Path : File_Path)
      return Boolean_Result.Result is
   begin
      return Service.Provider.Is_Regular_File (Path.Value);
   end Is_Regular_File;

   -- -------------------
   --  Get_File_Size
   -- -------------------

   function Get_File_Size
     (Service : File_Path_Service_Type; Path : File_Path)
      return Natural_Result.Result is
   begin
      return Service.Provider.Get_File_Size (Path.Value);
   end Get_File_Size;

   -- -------------------
   --  Get_Extension
   -- -------------------

   function Get_Extension
     (Service : File_Path_Service_Type; Path : File_Path)
      return String_Result.Result is
   begin
      return Service.Provider.Get_Extension (Path.Value);
   end Get_Extension;

   -- -------------------
   --  Get_Base_Name
   -- -------------------

   function Get_Base_Name
     (Service : File_Path_Service_Type; Path : File_Path)
      return String_Result.Result is
   begin
      return Service.Provider.Get_Base_Name (Path.Value);
   end Get_Base_Name;

   -- ------------------------
   --  Get_Directory_Name
   -- ------------------------

   function Get_Directory_Name
     (Service : File_Path_Service_Type; Path : File_Path)
      return String_Result.Result is
   begin
      return Service.Provider.Get_Directory_Name (Path.Value);
   end Get_Directory_Name;

   -- --------------------
   --  With_Extension
   -- --------------------

   function With_Extension
     (Service : File_Path_Service_Type; Path : File_Path; Extension : String)
      return String_Result.Result is
   begin
      return Service.Provider.With_Extension (Path.Value, Extension);
   end With_Extension;

   -- ---------------
   --  Join_Path
   -- ---------------

   function Join_Path
     (Service : File_Path_Service_Type; Directory : File_Path; Name : String)
      return String_Result.Result is
   begin
      return Service.Provider.Compose_Path (Directory.Value, Name);
   end Join_Path;

   -- ------------------------
   --  Get_Absolute_Path
   -- ------------------------

   function Get_Absolute_Path
     (Service : File_Path_Service_Type; Path : File_Path)
      return String_Result.Result is
   begin
      return Service.Provider.Get_Absolute_Path (Path.Value);
   end Get_Absolute_Path;

end Abohlib.Core.Domain.Services.File_Path_Service;
