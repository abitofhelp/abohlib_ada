--  =============================================================================
--  Abohlib.Infrastructure.Errors - Implementation
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

with Ada.Calendar;
with System;

package body Abohlib.Infrastructure.Errors is

   use Ada.Calendar;

   --  ==========================================================================
   --  Error Constructors
   --  ==========================================================================

   function Make_Database_Error
     (Kind          : Database_Error_Kind;
      Database_Name : String;
      Query         : String := "";
      Error_Code    : Integer := 0;
      Message       : String := "";
      Recovery      : String := "") return Database_Error
   is
      Error_Message    : Error_Strings.Bounded_String;
      Recovery_Message : Recovery_Strings.Bounded_String;
   begin
      --  Build error message
      if Message = "" then
         case Kind is
            when Connection_Failed =>
               Error_Message :=
                 Error_Strings.To_Bounded_String
                   ("Failed to connect to database '" & Database_Name & "'");

            when Query_Failed =>
               Error_Message :=
                 Error_Strings.To_Bounded_String
                   ("Query failed on database '" & Database_Name & "'");

            when Transaction_Failed =>
               Error_Message :=
                 Error_Strings.To_Bounded_String
                   ("Transaction failed on database '" & Database_Name & "'");

            when Constraint_Violation =>
               Error_Message :=
                 Error_Strings.To_Bounded_String
                   ("Constraint violation on database '"
                    & Database_Name
                    & "'");

            when Deadlock_Detected =>
               Error_Message :=
                 Error_Strings.To_Bounded_String
                   ("Deadlock detected on database '" & Database_Name & "'");

            when Timeout =>
               Error_Message :=
                 Error_Strings.To_Bounded_String
                   ("Operation timed out on database '" & Database_Name & "'");
         end case;
      else
         Error_Message := Error_Strings.To_Bounded_String (Message);
      end if;

      --  Build recovery suggestion
      if Recovery = "" then
         case Kind is
            when Connection_Failed =>
               Recovery_Message :=
                 Recovery_Strings.To_Bounded_String
                   ("Check database connection parameters and network");

            when Query_Failed =>
               Recovery_Message :=
                 Recovery_Strings.To_Bounded_String
                   ("Review query syntax and parameters");

            when Transaction_Failed =>
               Recovery_Message :=
                 Recovery_Strings.To_Bounded_String
                   ("Retry transaction or check for conflicts");

            when Constraint_Violation =>
               Recovery_Message :=
                 Recovery_Strings.To_Bounded_String
                   ("Ensure data meets all constraints");

            when Deadlock_Detected =>
               Recovery_Message :=
                 Recovery_Strings.To_Bounded_String
                   ("Retry operation with backoff");

            when Timeout =>
               Recovery_Message :=
                 Recovery_Strings.To_Bounded_String
                   ("Increase timeout or optimize operation");
         end case;
      else
         Recovery_Message := Recovery_Strings.To_Bounded_String (Recovery);
      end if;

      return
        (Base          =>
           (Category            => Integration_Error,
            Severity            => Error,
            Message             => Error_Message,
            Occurred_At         => Clock,
            Recovery_Suggestion => Recovery_Message,
            Correlation_ID      => Generate_Correlation_ID,
            Error_Context       => Context_Strings.Null_Bounded_String,
            Source_Location     => System.Null_Address),
         Kind          => Kind,
         Query         => Query_Strings.To_Bounded_String (Query),
         Database_Name => Service_Strings.To_Bounded_String (Database_Name),
         Error_Code    => Error_Code);
   end Make_Database_Error;

   function Make_File_System_Error
     (Kind      : File_System_Error_Kind;
      Path      : String;
      Operation : String;
      Message   : String := "";
      Recovery  : String := "") return File_System_Error
   is
      Error_Message    : Error_Strings.Bounded_String;
      Recovery_Message : Recovery_Strings.Bounded_String;
   begin
      --  Build error message
      if Message = "" then
         case Kind is
            when File_Not_Found =>
               Error_Message :=
                 Error_Strings.To_Bounded_String ("File not found: " & Path);

            when Permission_Denied =>
               Error_Message :=
                 Error_Strings.To_Bounded_String
                   ("Permission denied for " & Operation & " on: " & Path);

            when Disk_Full =>
               Error_Message :=
                 Error_Strings.To_Bounded_String
                   ("Disk full while " & Operation & " on: " & Path);

            when Path_Too_Long =>
               Error_Message :=
                 Error_Strings.To_Bounded_String ("Path too long: " & Path);

            when Invalid_Path =>
               Error_Message :=
                 Error_Strings.To_Bounded_String ("Invalid path: " & Path);

            when IO_Error =>
               Error_Message :=
                 Error_Strings.To_Bounded_String
                   ("I/O error during " & Operation & " on: " & Path);
         end case;
      else
         Error_Message := Error_Strings.To_Bounded_String (Message);
      end if;

      --  Build recovery suggestion
      if Recovery = "" then
         case Kind is
            when File_Not_Found =>
               Recovery_Message :=
                 Recovery_Strings.To_Bounded_String
                   ("Verify file path and existence");

            when Permission_Denied =>
               Recovery_Message :=
                 Recovery_Strings.To_Bounded_String
                   ("Check file permissions and ownership");

            when Disk_Full =>
               Recovery_Message :=
                 Recovery_Strings.To_Bounded_String
                   ("Free up disk space and retry");

            when Path_Too_Long =>
               Recovery_Message :=
                 Recovery_Strings.To_Bounded_String
                   ("Use shorter path or relocate files");

            when Invalid_Path =>
               Recovery_Message :=
                 Recovery_Strings.To_Bounded_String
                   ("Correct path syntax and special characters");

            when IO_Error =>
               Recovery_Message :=
                 Recovery_Strings.To_Bounded_String
                   ("Check disk health and retry operation");
         end case;
      else
         Recovery_Message := Recovery_Strings.To_Bounded_String (Recovery);
      end if;

      return
        (Base      =>
           (Category            => Resource_Error,
            Severity            => Error,
            Message             => Error_Message,
            Occurred_At         => Clock,
            Recovery_Suggestion => Recovery_Message,
            Correlation_ID      => Generate_Correlation_ID,
            Error_Context       => Context_Strings.Null_Bounded_String,
            Source_Location     => System.Null_Address),
         Kind      => Kind,
         Path      => Error_Strings.To_Bounded_String (Path),
         Operation => Field_Strings.To_Bounded_String (Operation));
   end Make_File_System_Error;

   function Make_Network_Error
     (Kind         : Network_Error_Kind;
      Service_Name : String;
      Endpoint     : String := "";
      Status_Code  : Integer := 0;
      Message      : String := "";
      Recovery     : String := "") return Network_Error
   is
      Error_Message    : Error_Strings.Bounded_String;
      Recovery_Message : Recovery_Strings.Bounded_String;
   begin
      --  Build error message
      if Message = "" then
         case Kind is
            when Connection_Refused =>
               Error_Message :=
                 Error_Strings.To_Bounded_String
                   ("Connection refused to " & Service_Name);

            when Host_Unreachable =>
               Error_Message :=
                 Error_Strings.To_Bounded_String
                   ("Host unreachable: " & Service_Name);

            when Timeout =>
               Error_Message :=
                 Error_Strings.To_Bounded_String
                   ("Network timeout for " & Service_Name);

            when DNS_Resolution_Failed =>
               Error_Message :=
                 Error_Strings.To_Bounded_String
                   ("DNS resolution failed for " & Service_Name);

            when SSL_Error =>
               Error_Message :=
                 Error_Strings.To_Bounded_String
                   ("SSL/TLS error with " & Service_Name);

            when Protocol_Error =>
               Error_Message :=
                 Error_Strings.To_Bounded_String
                   ("Protocol error with " & Service_Name);
         end case;
      else
         Error_Message := Error_Strings.To_Bounded_String (Message);
      end if;

      --  Build recovery suggestion
      if Recovery = "" then
         case Kind is
            when Connection_Refused =>
               Recovery_Message :=
                 Recovery_Strings.To_Bounded_String
                   ("Check if service is running and accepting connections");

            when Host_Unreachable =>
               Recovery_Message :=
                 Recovery_Strings.To_Bounded_String
                   ("Verify network connectivity and routing");

            when Timeout =>
               Recovery_Message :=
                 Recovery_Strings.To_Bounded_String
                   ("Increase timeout or check network latency");

            when DNS_Resolution_Failed =>
               Recovery_Message :=
                 Recovery_Strings.To_Bounded_String
                   ("Check DNS configuration and hostname");

            when SSL_Error =>
               Recovery_Message :=
                 Recovery_Strings.To_Bounded_String
                   ("Verify SSL certificates and configuration");

            when Protocol_Error =>
               Recovery_Message :=
                 Recovery_Strings.To_Bounded_String
                   ("Check protocol version and compatibility");
         end case;
      else
         Recovery_Message := Recovery_Strings.To_Bounded_String (Recovery);
      end if;

      return
        (Base         =>
           (Category            => Integration_Error,
            Severity            => Error,
            Message             => Error_Message,
            Occurred_At         => Clock,
            Recovery_Suggestion => Recovery_Message,
            Correlation_ID      => Generate_Correlation_ID,
            Error_Context       => Context_Strings.Null_Bounded_String,
            Source_Location     => System.Null_Address),
         Kind         => Kind,
         Service_Name => Service_Strings.To_Bounded_String (Service_Name),
         Endpoint     => Connection_Strings.To_Bounded_String (Endpoint),
         Status_Code  => Status_Code);
   end Make_Network_Error;

   function Make_External_Service_Error
     (Service_Name  : String;
      Operation     : String;
      Response_Code : Integer := 0;
      Response_Body : String := "";
      Message       : String := "";
      Recovery      : String := "") return External_Service_Error
   is
      Error_Message    : Error_Strings.Bounded_String;
      Recovery_Message : Recovery_Strings.Bounded_String;
   begin
      --  Build error message
      if Message = "" then
         Error_Message :=
           Error_Strings.To_Bounded_String
             ("External service error from "
              & Service_Name
              & " during "
              & Operation
              & (if Response_Code /= 0
                 then " (code:" & Response_Code'Image & ")"
                 else ""));
      else
         Error_Message := Error_Strings.To_Bounded_String (Message);
      end if;

      --  Build recovery suggestion
      if Recovery = "" then
         Recovery_Message :=
           Recovery_Strings.To_Bounded_String
             ("Check service documentation for error code meaning");
      else
         Recovery_Message := Recovery_Strings.To_Bounded_String (Recovery);
      end if;

      return
        (Base          =>
           (Category            => Integration_Error,
            Severity            => Error,
            Message             => Error_Message,
            Occurred_At         => Clock,
            Recovery_Suggestion => Recovery_Message,
            Correlation_ID      => Generate_Correlation_ID,
            Error_Context       => Context_Strings.Null_Bounded_String,
            Source_Location     => System.Null_Address),
         Service_Name  => Service_Strings.To_Bounded_String (Service_Name),
         Operation     => Field_Strings.To_Bounded_String (Operation),
         Response_Code => Response_Code,
         Response_Body => Error_Strings.To_Bounded_String (Response_Body));
   end Make_External_Service_Error;

   --  ==========================================================================
   --  Error Utilities
   --  ==========================================================================

   function To_String (Error : Database_Error) return String is
      Base_String : constant String := To_String (Error.Base);
   begin
      return
        Base_String
        & " [Database: "
        & Service_Strings.To_String (Error.Database_Name)
        & (if Error.Error_Code /= 0 then ", Code: " & Error.Error_Code'Image
           else "")
        & (if Query_Strings.Length (Error.Query) > 0
           then ", Query: " & Query_Strings.To_String (Error.Query)
           else "")
        & "]";
   end To_String;

   function To_String (Error : File_System_Error) return String is
      Base_String : constant String := To_String (Error.Base);
   begin
      return
        Base_String
        & " [Path: "
        & Error_Strings.To_String (Error.Path)
        & ", Operation: "
        & Field_Strings.To_String (Error.Operation)
        & "]";
   end To_String;

   function To_String (Error : Network_Error) return String is
      Base_String : constant String := To_String (Error.Base);
   begin
      return
        Base_String
        & " [Service: "
        & Service_Strings.To_String (Error.Service_Name)
        & (if Connection_Strings.Length (Error.Endpoint) > 0
           then ", Endpoint: " & Connection_Strings.To_String (Error.Endpoint)
           else "")
        & (if Error.Status_Code /= 0
           then ", Status: " & Error.Status_Code'Image
           else "")
        & "]";
   end To_String;

   function To_String (Error : External_Service_Error) return String is
      Base_String : constant String := To_String (Error.Base);
   begin
      return
        Base_String
        & " [Service: "
        & Service_Strings.To_String (Error.Service_Name)
        & ", Operation: "
        & Field_Strings.To_String (Error.Operation)
        & (if Error.Response_Code /= 0
           then ", Response Code: " & Error.Response_Code'Image
           else "")
        & (if Error_Strings.Length (Error.Response_Body) > 0
           then ", Body: " & Error_Strings.To_String (Error.Response_Body)
           else "")
        & "]";
   end To_String;

end Abohlib.Infrastructure.Errors;
