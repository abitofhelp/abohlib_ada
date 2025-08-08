--  =============================================================================
--  Abohlib.Presentation.Errors - Implementation
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

with Ada.Calendar;
with Abohlib.Presentation.Constants.HTTP_Status;
with System;

package body Abohlib.Presentation.Errors is

   use Ada.Calendar;
   use Abohlib.Presentation.Constants.HTTP_Status;

   --  ==========================================================================
   --  Error Constructors
   --  ==========================================================================

   function Make_Request_Error
     (Kind            : Request_Error_Kind;
      Endpoint        : String;
      Field_Name      : String := "";
      Expected_Format : String := "";
      Message         : String := "";
      Recovery        : String := "") return Request_Error
   is
      Error_Message    : Error_Strings.Bounded_String;
      Recovery_Message : Recovery_Strings.Bounded_String;
   begin
      --  Build error message
      if Message = "" then
         case Kind is
            when Invalid_Format =>
               Error_Message :=
                 Error_Strings.To_Bounded_String
                   ("Invalid request format at " & Endpoint);

            when Missing_Required_Field =>
               Error_Message :=
                 Error_Strings.To_Bounded_String
                   ("Missing required field '"
                    & Field_Name
                    & "' at "
                    & Endpoint);

            when Invalid_Content_Type =>
               Error_Message :=
                 Error_Strings.To_Bounded_String
                   ("Invalid content type at " & Endpoint);

            when Request_Too_Large =>
               Error_Message :=
                 Error_Strings.To_Bounded_String
                   ("Request too large at " & Endpoint);

            when Unsupported_Method =>
               Error_Message :=
                 Error_Strings.To_Bounded_String
                   ("Unsupported HTTP method at " & Endpoint);

            when Invalid_Header =>
               Error_Message :=
                 Error_Strings.To_Bounded_String
                   ("Invalid header at " & Endpoint);
         end case;
      else
         Error_Message := Error_Strings.To_Bounded_String (Message);
      end if;

      --  Build recovery suggestion
      if Recovery = "" then
         case Kind is
            when Invalid_Format =>
               Recovery_Message :=
                 Recovery_Strings.To_Bounded_String
                   ("Check request format against API documentation");

            when Missing_Required_Field =>
               Recovery_Message :=
                 Recovery_Strings.To_Bounded_String
                   ("Include all required fields in request");

            when Invalid_Content_Type =>
               Recovery_Message :=
                 Recovery_Strings.To_Bounded_String
                   ("Use supported content type (e.g., application/json)");

            when Request_Too_Large =>
               Recovery_Message :=
                 Recovery_Strings.To_Bounded_String
                   ("Reduce request size or use pagination");

            when Unsupported_Method =>
               Recovery_Message :=
                 Recovery_Strings.To_Bounded_String
                   ("Use supported HTTP method for this endpoint");

            when Invalid_Header =>
               Recovery_Message :=
                 Recovery_Strings.To_Bounded_String
                   ("Check header format and values");
         end case;
      else
         Recovery_Message := Recovery_Strings.To_Bounded_String (Recovery);
      end if;

      return
        (Base            =>
           (Category            => Validation_Error,
            Severity            => Error,
            Message             => Error_Message,
            Occurred_At         => Clock,
            Recovery_Suggestion => Recovery_Message,
            Correlation_ID      => Generate_Correlation_ID,
            Error_Context       =>
              Context_Strings.To_Bounded_String
                ("Request error at endpoint: " & Endpoint),
            Source_Location     => System.Null_Address),
         Kind            => Kind,
         Endpoint        => Endpoint_Strings.To_Bounded_String (Endpoint),
         Field_Name      => Field_Strings.To_Bounded_String (Field_Name),
         Expected_Format => Error_Strings.To_Bounded_String (Expected_Format));
   end Make_Request_Error;

   function Make_Auth_Error
     (Kind                : Auth_Error_Kind;
      Required_Permission : String := "";
      User_Id             : String := "";
      Message             : String := "";
      Recovery            : String := "") return Auth_Error
   is
      Error_Message    : Error_Strings.Bounded_String;
      Recovery_Message : Recovery_Strings.Bounded_String;
   begin
      --  Build error message
      if Message = "" then
         case Kind is
            when Invalid_Credentials =>
               Error_Message :=
                 Error_Strings.To_Bounded_String
                   ("Invalid credentials provided");

            when Token_Expired =>
               Error_Message :=
                 Error_Strings.To_Bounded_String
                   ("Authentication token has expired");

            when Token_Invalid =>
               Error_Message :=
                 Error_Strings.To_Bounded_String
                   ("Invalid authentication token");

            when Insufficient_Permissions =>
               Error_Message :=
                 Error_Strings.To_Bounded_String
                   ("Insufficient permissions"
                    & (if Required_Permission /= ""
                       then " (requires: " & Required_Permission & ")"
                       else ""));

            when Session_Expired =>
               Error_Message :=
                 Error_Strings.To_Bounded_String ("Session has expired");

            when Two_Factor_Required =>
               Error_Message :=
                 Error_Strings.To_Bounded_String
                   ("Two-factor authentication required");
         end case;
      else
         Error_Message := Error_Strings.To_Bounded_String (Message);
      end if;

      --  Build recovery suggestion
      if Recovery = "" then
         case Kind is
            when Invalid_Credentials =>
               Recovery_Message :=
                 Recovery_Strings.To_Bounded_String
                   ("Check username and password");

            when Token_Expired =>
               Recovery_Message :=
                 Recovery_Strings.To_Bounded_String
                   ("Refresh authentication token");

            when Token_Invalid =>
               Recovery_Message :=
                 Recovery_Strings.To_Bounded_String
                   ("Obtain new authentication token");

            when Insufficient_Permissions =>
               Recovery_Message :=
                 Recovery_Strings.To_Bounded_String
                   ("Request required permissions from administrator");

            when Session_Expired =>
               Recovery_Message :=
                 Recovery_Strings.To_Bounded_String
                   ("Log in again to create new session");

            when Two_Factor_Required =>
               Recovery_Message :=
                 Recovery_Strings.To_Bounded_String
                   ("Complete two-factor authentication");
         end case;
      else
         Recovery_Message := Recovery_Strings.To_Bounded_String (Recovery);
      end if;

      return
        (Base                =>
           (Category            => Authorization_Error,
            Severity            => Error,
            Message             => Error_Message,
            Occurred_At         => Clock,
            Recovery_Suggestion => Recovery_Message,
            Correlation_ID      => Generate_Correlation_ID,
            Error_Context       =>
              Context_Strings.To_Bounded_String
                ("Auth error"
                 & (if User_Id /= "" then " for user: " & User_Id else "")),
            Source_Location     => System.Null_Address),
         Kind                => Kind,
         Required_Permission =>
           Field_Strings.To_Bounded_String (Required_Permission),
         User_Id             => Field_Strings.To_Bounded_String (User_Id));
   end Make_Auth_Error;

   function Make_Response_Error
     (Content_Type        : String;
      Serialization_Error : String;
      Message             : String := "";
      Recovery            : String := "") return Response_Error
   is
      Error_Message    : Error_Strings.Bounded_String;
      Recovery_Message : Recovery_Strings.Bounded_String;
   begin
      --  Build error message
      if Message = "" then
         Error_Message :=
           Error_Strings.To_Bounded_String
             ("Failed to serialize response as "
              & Content_Type
              & ": "
              & Serialization_Error);
      else
         Error_Message := Error_Strings.To_Bounded_String (Message);
      end if;

      --  Build recovery suggestion
      if Recovery = "" then
         Recovery_Message :=
           Recovery_Strings.To_Bounded_String
             ("Check data format and serialization settings");
      else
         Recovery_Message := Recovery_Strings.To_Bounded_String (Recovery);
      end if;

      return
        (Base                =>
           (Category            => Integration_Error,
            Severity            => Error,
            Message             => Error_Message,
            Occurred_At         => Clock,
            Recovery_Suggestion => Recovery_Message,
            Correlation_ID      => Generate_Correlation_ID,
            Error_Context       =>
              Context_Strings.To_Bounded_String
                ("Response serialization error for content type: "
                 & Content_Type),
            Source_Location     => System.Null_Address),
         Content_Type        =>
           Content_Type_Strings.To_Bounded_String (Content_Type),
         Serialization_Error =>
           Error_Strings.To_Bounded_String (Serialization_Error));
   end Make_Response_Error;

   function Make_Rate_Limit_Error
     (Limit               : Natural;
      Window_Seconds      : Natural;
      Retry_After_Seconds : Natural;
      Message             : String := "";
      Recovery            : String := "") return Rate_Limit_Error
   is
      Error_Message    : Error_Strings.Bounded_String;
      Recovery_Message : Recovery_Strings.Bounded_String;
   begin
      --  Build error message
      if Message = "" then
         Error_Message :=
           Error_Strings.To_Bounded_String
             ("Rate limit exceeded: "
              & Limit'Image
              & " requests per "
              & Window_Seconds'Image
              & " seconds");
      else
         Error_Message := Error_Strings.To_Bounded_String (Message);
      end if;

      --  Build recovery suggestion
      if Recovery = "" then
         Recovery_Message :=
           Recovery_Strings.To_Bounded_String
             ("Wait "
              & Retry_After_Seconds'Image
              & " seconds before retrying");
      else
         Recovery_Message := Recovery_Strings.To_Bounded_String (Recovery);
      end if;

      return
        (Base                =>
           (Category            => Resource_Error,
            Severity            => Warning,
            Message             => Error_Message,
            Occurred_At         => Clock,
            Recovery_Suggestion => Recovery_Message,
            Correlation_ID      => Generate_Correlation_ID,
            Error_Context       =>
              Context_Strings.To_Bounded_String
                ("Rate limit exceeded: "
                 & Limit'Image
                 & " requests per "
                 & Window_Seconds'Image
                 & " seconds"),
            Source_Location     => System.Null_Address),
         Limit               => Limit,
         Window_Seconds      => Window_Seconds,
         Retry_After_Seconds => Retry_After_Seconds);
   end Make_Rate_Limit_Error;

   --  ==========================================================================
   --  Error Utilities
   --  ==========================================================================

   function To_String (Error : Request_Error) return String is
      Base_String : constant String := To_String (Error.Base);
   begin
      return
        Base_String
        & " [Endpoint: "
        & Endpoint_Strings.To_String (Error.Endpoint)
        & (if Field_Strings.Length (Error.Field_Name) > 0
           then ", Field: " & Field_Strings.To_String (Error.Field_Name)
           else "")
        & (if Error_Strings.Length (Error.Expected_Format) > 0
           then
             ", Expected: " & Error_Strings.To_String (Error.Expected_Format)
           else "")
        & "]";
   end To_String;

   function To_String (Error : Auth_Error) return String is
      Base_String : constant String := To_String (Error.Base);
   begin
      return
        Base_String
        & (if Field_Strings.Length (Error.User_Id) > 0
           then " [User: " & Field_Strings.To_String (Error.User_Id) & "]"
           else "")
        & (if Field_Strings.Length (Error.Required_Permission) > 0
           then
             " [Required: "
             & Field_Strings.To_String (Error.Required_Permission)
             & "]"
           else "");
   end To_String;

   function To_String (Error : Response_Error) return String is
      Base_String : constant String := To_String (Error.Base);
   begin
      return
        Base_String
        & " [Content-Type: "
        & Content_Type_Strings.To_String (Error.Content_Type)
        & ", Error: "
        & Error_Strings.To_String (Error.Serialization_Error)
        & "]";
   end To_String;

   function To_String (Error : Rate_Limit_Error) return String is
      Base_String : constant String := To_String (Error.Base);
   begin
      return
        Base_String
        & " [Limit:"
        & Error.Limit'Image
        & "/"
        & Error.Window_Seconds'Image
        & "s"
        & ", Retry after:"
        & Error.Retry_After_Seconds'Image
        & "s]";
   end To_String;

   --  ==========================================================================
   --  HTTP Status Code Mappings
   --  ==========================================================================

   function To_HTTP_Status (Error : Request_Error) return Natural is
   begin
      case Error.Kind is
         when Invalid_Format | Missing_Required_Field | Invalid_Header =>
            return HTTP_BAD_REQUEST;

         when Invalid_Content_Type =>
            return HTTP_UNSUPPORTED_MEDIA_TYPE;

         when Request_Too_Large =>
            return HTTP_REQUEST_ENTITY_TOO_LARGE;

         when Unsupported_Method =>
            return HTTP_METHOD_NOT_ALLOWED;
      end case;
   end To_HTTP_Status;

   function To_HTTP_Status (Error : Auth_Error) return Natural is
   begin
      case Error.Kind is
         when Invalid_Credentials | Token_Invalid | Session_Expired =>
            return HTTP_UNAUTHORIZED;

         when Token_Expired =>
            return HTTP_UNAUTHORIZED;

         when Insufficient_Permissions =>
            return HTTP_FORBIDDEN;

         when Two_Factor_Required =>
            return HTTP_UNAUTHORIZED;
      end case;
   end To_HTTP_Status;

   function To_HTTP_Status (Error : Response_Error) return Natural is
      pragma Unreferenced (Error);
   begin
      return HTTP_INTERNAL_SERVER_ERROR;
   end To_HTTP_Status;

   function To_HTTP_Status (Error : Rate_Limit_Error) return Natural is
      pragma Unreferenced (Error);
   begin
      return HTTP_TOO_MANY_REQUESTS;
   end To_HTTP_Status;

end Abohlib.Presentation.Errors;
