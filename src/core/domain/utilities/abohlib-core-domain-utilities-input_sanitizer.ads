--  =============================================================================
--  Abohlib.Core.Domain.Utilities.Input_Sanitizer - Input Sanitization Utilities
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Purpose:
--    Provides utilities for sanitizing and validating input data to prevent
--    security vulnerabilities and ensure data integrity.
--
--  Features:
--    - SQL injection prevention
--    - Command injection prevention
--    - Path traversal prevention
--    - HTML/XML encoding
--    - Whitelist validation
--  =============================================================================

pragma Ada_2022;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Abohlib.Core.Domain.Result;
with Abohlib.Core.Domain.Types.Strings; use Abohlib.Core.Domain.Types.Strings;

package Abohlib.Core.Domain.Utilities.Input_Sanitizer is

   --  ==========================================================================
   --  Types
   --  ==========================================================================

   type Sanitization_Error is record
      Message : Unbounded_String;
      Input   : Unbounded_String;
   end record;

   --  Result types for sanitization operations
   package Sanitization_Result is new
     Result.Result_Package
       (Ok_Type  => Unbounded_String,
        Err_Type => Sanitization_Error);

   --  ==========================================================================
   --  SQL Injection Prevention
   --  ==========================================================================

   --  Sanitize input for use in SQL queries
   function Sanitize_SQL_Input (Input : String) return Sanitization_Result.Result
   with
     Post =>
       (if Sanitization_Result.Is_Ok (Sanitize_SQL_Input'Result) then
          not Contains_SQL_Metacharacters
            (To_String (Sanitization_Result.Get_Ok (Sanitize_SQL_Input'Result))));

   --  Check if string contains SQL metacharacters
   function Contains_SQL_Metacharacters (Input : String) return Boolean;
   pragma Inline (Contains_SQL_Metacharacters);

   --  ==========================================================================
   --  Command Injection Prevention
   --  ==========================================================================

   --  Sanitize input for use in shell commands
   function Sanitize_Shell_Input (Input : String) return Sanitization_Result.Result
   with
     Post =>
       (if Sanitization_Result.Is_Ok (Sanitize_Shell_Input'Result) then
          not Contains_Shell_Metacharacters
            (To_String (Sanitization_Result.Get_Ok (Sanitize_Shell_Input'Result))));

   --  Check if string contains shell metacharacters
   function Contains_Shell_Metacharacters (Input : String) return Boolean;
   pragma Inline (Contains_Shell_Metacharacters);

   --  ==========================================================================
   --  Path Traversal Prevention
   --  ==========================================================================

   --  Sanitize file paths to prevent directory traversal attacks
   function Sanitize_File_Path (Path : String) return Sanitization_Result.Result
   with
     Post =>
       (if Sanitization_Result.Is_Ok (Sanitize_File_Path'Result) then
          not Contains_Path_Traversal_Pattern
            (To_String (Sanitization_Result.Get_Ok (Sanitize_File_Path'Result))));

   --  Check if path contains directory traversal patterns
   function Contains_Path_Traversal_Pattern (Path : String) return Boolean;
   pragma Inline (Contains_Path_Traversal_Pattern);

   --  ==========================================================================
   --  HTML/XML Encoding
   --  ==========================================================================

   --  Encode special characters for safe HTML output
   function Encode_HTML (Input : String) return String
   with
     Post => not Contains_HTML_Metacharacters (Encode_HTML'Result);

   --  Encode special characters for safe XML output
   function Encode_XML (Input : String) return String
   with
     Post => not Contains_XML_Metacharacters (Encode_XML'Result);

   --  Check functions
   function Contains_HTML_Metacharacters (Input : String) return Boolean;
   pragma Inline (Contains_HTML_Metacharacters);

   function Contains_XML_Metacharacters (Input : String) return Boolean;
   pragma Inline (Contains_XML_Metacharacters);

   --  ==========================================================================
   --  Whitelist Validation
   --  ==========================================================================

   --  Validate input contains only alphanumeric characters
   function Is_Alphanumeric (Input : String) return Boolean
   with Post => Is_Alphanumeric'Result = (for all C of Input => 
                  (C in 'A' .. 'Z') or (C in 'a' .. 'z') or (C in '0' .. '9'));
   pragma Inline (Is_Alphanumeric);

   --  Validate input contains only specified characters
   function Is_Whitelisted
     (Input : String; Allowed_Chars : String) return Boolean
   with Post => Is_Whitelisted'Result = 
                  (for all C of Input => (for some A of Allowed_Chars => C = A));

   --  Remove non-whitelisted characters from input
   function Strip_Non_Whitelisted
     (Input : String; Allowed_Chars : String) return String
   with Post => Is_Whitelisted (Strip_Non_Whitelisted'Result, Allowed_Chars);

   --  ==========================================================================
   --  General Utilities
   --  ==========================================================================

   --  Truncate string to maximum length
   function Truncate (Input : String; Max_Length : Truncation_Length_Type) return String
   with Post => Truncate'Result'Length <= Natural(Max_Length);
   pragma Inline (Truncate);

   --  Remove leading and trailing whitespace
   function Trim_Whitespace (Input : String) return String;

   --  Normalize whitespace (multiple spaces to single space)
   function Normalize_Whitespace (Input : String) return String;

private

   --  Constants for dangerous patterns
   SQL_Metacharacters : constant String := "';""--/**/";
   Shell_Metacharacters : constant String := "&|;`$(){}[]<>*?!~";
   Path_Traversal_Patterns : constant array (1 .. 3) of String (1 .. 3) :=
     ["../", "..\", "..."];
   HTML_Entities : constant array (1 .. 5) of String (1 .. 6) :=
     ["&amp; ", "&lt;  ", "&gt;  ", "&quot;", "&#39; "];

end Abohlib.Core.Domain.Utilities.Input_Sanitizer;