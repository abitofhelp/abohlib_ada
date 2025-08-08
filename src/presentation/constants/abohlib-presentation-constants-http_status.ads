--  =============================================================================
--  Abohlib.Presentation.Constants.HTTP_Status - HTTP Status Code Constants
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Purpose:
--    Defines HTTP status codes as named constants to eliminate magic numbers
--    in presentation layer error handling. These constants represent standard
--    HTTP response status codes as defined in RFC 7231 and related specifications.
--
--  Usage:
--    Use these constants instead of magic numbers when returning HTTP status
--    codes from error handling functions or API responses.
--
--  Examples:
--    return HTTP_BAD_REQUEST;           -- Instead of: return 400;
--    return HTTP_INTERNAL_SERVER_ERROR; -- Instead of: return 500;
--
--  References:
--    - RFC 7231: Hypertext Transfer Protocol (HTTP/1.1): Semantics and Content
--    - RFC 6585: Additional HTTP Status Codes
--  =============================================================================

pragma Ada_2022;

package Abohlib.Presentation.Constants.HTTP_Status is
   pragma Pure;

   --  ==========================================================================
   --  1xx Informational Response Status Codes
   --  ==========================================================================
   
   HTTP_CONTINUE                        : constant := 100;
   HTTP_SWITCHING_PROTOCOLS             : constant := 101;
   HTTP_PROCESSING                      : constant := 102;
   HTTP_EARLY_HINTS                     : constant := 103;

   --  ==========================================================================
   --  2xx Successful Response Status Codes
   --  ==========================================================================
   
   HTTP_OK                              : constant := 200;
   HTTP_CREATED                         : constant := 201;
   HTTP_ACCEPTED                        : constant := 202;
   HTTP_NON_AUTHORITATIVE_INFORMATION   : constant := 203;
   HTTP_NO_CONTENT                      : constant := 204;
   HTTP_RESET_CONTENT                   : constant := 205;
   HTTP_PARTIAL_CONTENT                 : constant := 206;

   --  ==========================================================================
   --  3xx Redirection Status Codes
   --  ==========================================================================
   
   HTTP_MULTIPLE_CHOICES                : constant := 300;
   HTTP_MOVED_PERMANENTLY               : constant := 301;
   HTTP_FOUND                           : constant := 302;
   HTTP_SEE_OTHER                       : constant := 303;
   HTTP_NOT_MODIFIED                    : constant := 304;
   HTTP_TEMPORARY_REDIRECT              : constant := 307;
   HTTP_PERMANENT_REDIRECT              : constant := 308;

   --  ==========================================================================
   --  4xx Client Error Status Codes
   --  ==========================================================================
   
   HTTP_BAD_REQUEST                     : constant := 400;
   HTTP_UNAUTHORIZED                    : constant := 401;
   HTTP_PAYMENT_REQUIRED                : constant := 402;
   HTTP_FORBIDDEN                       : constant := 403;
   HTTP_NOT_FOUND                       : constant := 404;
   HTTP_METHOD_NOT_ALLOWED              : constant := 405;
   HTTP_NOT_ACCEPTABLE                  : constant := 406;
   HTTP_PROXY_AUTHENTICATION_REQUIRED   : constant := 407;
   HTTP_REQUEST_TIMEOUT                 : constant := 408;
   HTTP_CONFLICT                        : constant := 409;
   HTTP_GONE                            : constant := 410;
   HTTP_LENGTH_REQUIRED                 : constant := 411;
   HTTP_PRECONDITION_FAILED             : constant := 412;
   HTTP_REQUEST_ENTITY_TOO_LARGE        : constant := 413;
   HTTP_REQUEST_URI_TOO_LONG            : constant := 414;
   HTTP_UNSUPPORTED_MEDIA_TYPE          : constant := 415;
   HTTP_REQUESTED_RANGE_NOT_SATISFIABLE : constant := 416;
   HTTP_EXPECTATION_FAILED              : constant := 417;
   HTTP_IM_A_TEAPOT                     : constant := 418;
   HTTP_UNPROCESSABLE_ENTITY            : constant := 422;
   HTTP_LOCKED                          : constant := 423;
   HTTP_FAILED_DEPENDENCY               : constant := 424;
   HTTP_TOO_EARLY                       : constant := 425;
   HTTP_UPGRADE_REQUIRED                : constant := 426;
   HTTP_PRECONDITION_REQUIRED           : constant := 428;
   HTTP_TOO_MANY_REQUESTS               : constant := 429;
   HTTP_REQUEST_HEADER_FIELDS_TOO_LARGE : constant := 431;
   HTTP_UNAVAILABLE_FOR_LEGAL_REASONS   : constant := 451;

   --  ==========================================================================
   --  5xx Server Error Status Codes
   --  ==========================================================================
   
   HTTP_INTERNAL_SERVER_ERROR           : constant := 500;
   HTTP_NOT_IMPLEMENTED                 : constant := 501;
   HTTP_BAD_GATEWAY                     : constant := 502;
   HTTP_SERVICE_UNAVAILABLE             : constant := 503;
   HTTP_GATEWAY_TIMEOUT                 : constant := 504;
   HTTP_HTTP_VERSION_NOT_SUPPORTED      : constant := 505;
   HTTP_VARIANT_ALSO_NEGOTIATES         : constant := 506;
   HTTP_INSUFFICIENT_STORAGE            : constant := 507;
   HTTP_LOOP_DETECTED                   : constant := 508;
   HTTP_NOT_EXTENDED                    : constant := 510;
   HTTP_NETWORK_AUTHENTICATION_REQUIRED : constant := 511;

end Abohlib.Presentation.Constants.HTTP_Status;