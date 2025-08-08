--  =============================================================================
--  Abohlib.Infrastructure.Errors - Infrastructure Layer Errors
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Purpose:
--    Provides error types for infrastructure concerns such as database
--    connectivity, file system operations, network issues, and external
--    service integration failures.
--
--  Usage:
--    Infrastructure operations return Result types containing either
--    success values or errors from this hierarchy.
--  =============================================================================

pragma Ada_2022;

with Abohlib.Core.Domain.Errors;
with Ada.Strings.Bounded;

package Abohlib.Infrastructure.Errors is

   use Abohlib.Core.Domain.Errors;

   --  Maximum lengths for infrastructure-specific strings
   Max_Service_Name_Length : constant := 100;
   Max_Connection_String_Length : constant := 200;
   Max_Query_Length : constant := 500;

   package Service_Strings is new
     Ada.Strings.Bounded.Generic_Bounded_Length (Max_Service_Name_Length);
   package Connection_Strings is new
     Ada.Strings.Bounded.Generic_Bounded_Length (Max_Connection_String_Length);
   package Query_Strings is new
     Ada.Strings.Bounded.Generic_Bounded_Length (Max_Query_Length);

   --  ==========================================================================
   --  Infrastructure Error Types
   --  ==========================================================================

   --  Database errors
   type Database_Error_Kind is
     (Connection_Failed,
      Query_Failed,
      Transaction_Failed,
      Constraint_Violation,
      Deadlock_Detected,
      Timeout);

   type Database_Error is record
      Base          : Domain_Error;
      Kind          : Database_Error_Kind;
      Query         : Query_Strings.Bounded_String;
      Database_Name : Service_Strings.Bounded_String;
      Error_Code    : Integer := 0;
   end record;

   --  File system errors
   type File_System_Error_Kind is
     (File_Not_Found,
      Permission_Denied,
      Disk_Full,
      Path_Too_Long,
      Invalid_Path,
      IO_Error);

   type File_System_Error is record
      Base      : Domain_Error;
      Kind      : File_System_Error_Kind;
      Path      : Error_Strings.Bounded_String;
      Operation : Field_Strings.Bounded_String;
   end record;

   --  Network errors
   type Network_Error_Kind is
     (Connection_Refused,
      Host_Unreachable,
      Timeout,
      DNS_Resolution_Failed,
      SSL_Error,
      Protocol_Error);

   type Network_Error is record
      Base         : Domain_Error;
      Kind         : Network_Error_Kind;
      Service_Name : Service_Strings.Bounded_String;
      Endpoint     : Connection_Strings.Bounded_String;
      Status_Code  : Integer := 0;
   end record;

   --  External service errors
   type External_Service_Error is record
      Base          : Domain_Error;
      Service_Name  : Service_Strings.Bounded_String;
      Operation     : Field_Strings.Bounded_String;
      Response_Code : Integer := 0;
      Response_Body : Error_Strings.Bounded_String;
   end record;

   --  ==========================================================================
   --  Error Constructors
   --  ==========================================================================

   function Make_Database_Error
     (Kind          : Database_Error_Kind;
      Database_Name : String;
      Query         : String := "";
      Error_Code    : Integer := 0;
      Message       : String := "";
      Recovery      : String := "") return Database_Error;

   function Make_File_System_Error
     (Kind      : File_System_Error_Kind;
      Path      : String;
      Operation : String;
      Message   : String := "";
      Recovery  : String := "") return File_System_Error;

   function Make_Network_Error
     (Kind         : Network_Error_Kind;
      Service_Name : String;
      Endpoint     : String := "";
      Status_Code  : Integer := 0;
      Message      : String := "";
      Recovery     : String := "") return Network_Error;

   function Make_External_Service_Error
     (Service_Name  : String;
      Operation     : String;
      Response_Code : Integer := 0;
      Response_Body : String := "";
      Message       : String := "";
      Recovery      : String := "") return External_Service_Error;

   --  ==========================================================================
   --  Error Utilities
   --  ==========================================================================

   function To_String (Error : Database_Error) return String;
   function To_String (Error : File_System_Error) return String;
   function To_String (Error : Network_Error) return String;
   function To_String (Error : External_Service_Error) return String;

end Abohlib.Infrastructure.Errors;
