--  =============================================================================
--  Abohlib.Presentation.Errors - Presentation Layer Errors
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Purpose:
--    Provides error types for presentation layer concerns such as request
--    validation, response formatting, authentication, and API-specific errors.
--
--  Usage:
--    Presentation layer operations return Result types containing either
--    success values or errors from this hierarchy.
--  =============================================================================

pragma Ada_2022;

with Abohlib.Core.Domain.Errors;
with Ada.Strings.Bounded;

package Abohlib.Presentation.Errors is

   use Abohlib.Core.Domain.Errors;

   --  Maximum lengths for presentation-specific strings
   Max_Endpoint_Length : constant := 200;
   Max_Header_Name_Length : constant := 50;
   Max_Content_Type_Length : constant := 100;

   package Endpoint_Strings is new
     Ada.Strings.Bounded.Generic_Bounded_Length (Max_Endpoint_Length);
   package Header_Strings is new
     Ada.Strings.Bounded.Generic_Bounded_Length (Max_Header_Name_Length);
   package Content_Type_Strings is new
     Ada.Strings.Bounded.Generic_Bounded_Length (Max_Content_Type_Length);

   --  ==========================================================================
   --  Presentation Error Types
   --  ==========================================================================

   --  Request validation errors
   type Request_Error_Kind is
     (Invalid_Format,
      Missing_Required_Field,
      Invalid_Content_Type,
      Request_Too_Large,
      Unsupported_Method,
      Invalid_Header);

   type Request_Error is record
      Base            : Domain_Error;
      Kind            : Request_Error_Kind;
      Endpoint        : Endpoint_Strings.Bounded_String;
      Field_Name      : Field_Strings.Bounded_String;
      Expected_Format : Error_Strings.Bounded_String;
   end record;

   --  Authentication/Authorization errors
   type Auth_Error_Kind is
     (Invalid_Credentials,
      Token_Expired,
      Token_Invalid,
      Insufficient_Permissions,
      Session_Expired,
      Two_Factor_Required);

   type Auth_Error is record
      Base                : Domain_Error;
      Kind                : Auth_Error_Kind;
      Required_Permission : Field_Strings.Bounded_String;
      User_Id             : Field_Strings.Bounded_String;
   end record;

   --  Response formatting errors
   type Response_Error is record
      Base                : Domain_Error;
      Content_Type        : Content_Type_Strings.Bounded_String;
      Serialization_Error : Error_Strings.Bounded_String;
   end record;

   --  Rate limiting errors
   type Rate_Limit_Error is record
      Base                : Domain_Error;
      Limit               : Natural;
      Window_Seconds      : Natural;
      Retry_After_Seconds : Natural;
   end record;

   --  ==========================================================================
   --  Error Constructors
   --  ==========================================================================

   function Make_Request_Error
     (Kind            : Request_Error_Kind;
      Endpoint        : String;
      Field_Name      : String := "";
      Expected_Format : String := "";
      Message         : String := "";
      Recovery        : String := "") return Request_Error;

   function Make_Auth_Error
     (Kind                : Auth_Error_Kind;
      Required_Permission : String := "";
      User_Id             : String := "";
      Message             : String := "";
      Recovery            : String := "") return Auth_Error;

   function Make_Response_Error
     (Content_Type        : String;
      Serialization_Error : String;
      Message             : String := "";
      Recovery            : String := "") return Response_Error;

   function Make_Rate_Limit_Error
     (Limit               : Natural;
      Window_Seconds      : Natural;
      Retry_After_Seconds : Natural;
      Message             : String := "";
      Recovery            : String := "") return Rate_Limit_Error;

   --  ==========================================================================
   --  Error Utilities
   --  ==========================================================================

   function To_String (Error : Request_Error) return String;
   function To_String (Error : Auth_Error) return String;
   function To_String (Error : Response_Error) return String;
   function To_String (Error : Rate_Limit_Error) return String;

   --  HTTP status code mappings
   function To_HTTP_Status (Error : Request_Error) return Natural;
   function To_HTTP_Status (Error : Auth_Error) return Natural;
   function To_HTTP_Status (Error : Response_Error) return Natural;
   function To_HTTP_Status (Error : Rate_Limit_Error) return Natural;

end Abohlib.Presentation.Errors;
