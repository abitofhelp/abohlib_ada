--  =============================================================================
--  Abohlib.Core.Domain.Utilities.Input_Sanitizer - Implementation
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

with Ada.Strings.Fixed;
with Ada.Characters.Latin_1;

package body Abohlib.Core.Domain.Utilities.Input_Sanitizer is

   use Ada.Strings.Fixed;

   --  ==========================================================================
   --  SQL Injection Prevention
   --  ==========================================================================

   function Contains_SQL_Metacharacters (Input : String) return Boolean is
   begin
      for C of Input loop
         if Index (SQL_Metacharacters, String'(1 => C)) > 0 then
            return True;
         end if;
      end loop;
      return False;
   end Contains_SQL_Metacharacters;

   function Sanitize_SQL_Input (Input : String) return Sanitization_Result.Result is
      Result : Unbounded_String := Null_Unbounded_String;
   begin
      --  Check for dangerous patterns
      if Index (Input, "--") > 0 or else
         Index (Input, "/*") > 0 or else
         Index (Input, "*/") > 0
      then
         return Sanitization_Result.Err
           ((Message => To_Unbounded_String ("SQL comment sequences not allowed"),
             Input   => To_Unbounded_String (Input)));
      end if;

      --  Escape single quotes by doubling them
      for C of Input loop
         if C = ''' then
            Append (Result, "''");
         else
            Append (Result, C);
         end if;
      end loop;

      return Sanitization_Result.Ok (Result);
   end Sanitize_SQL_Input;

   --  ==========================================================================
   --  Command Injection Prevention
   --  ==========================================================================

   function Contains_Shell_Metacharacters (Input : String) return Boolean is
   begin
      for C of Input loop
         if Index (Shell_Metacharacters, String'(1 => C)) > 0 then
            return True;
         end if;
      end loop;
      return False;
   end Contains_Shell_Metacharacters;

   function Sanitize_Shell_Input (Input : String) return Sanitization_Result.Result is
   begin
      if Contains_Shell_Metacharacters (Input) then
         return Sanitization_Result.Err
           ((Message => To_Unbounded_String ("Shell metacharacters not allowed"),
             Input   => To_Unbounded_String (Input)));
      end if;

      --  Additional checks for common injection patterns
      if Index (Input, "$(" ) > 0 or else
         Index (Input, "${" ) > 0 or else
         Index (Input, "`" ) > 0
      then
         return Sanitization_Result.Err
           ((Message => To_Unbounded_String ("Command substitution not allowed"),
             Input   => To_Unbounded_String (Input)));
      end if;

      return Sanitization_Result.Ok (To_Unbounded_String (Input));
   end Sanitize_Shell_Input;

   --  ==========================================================================
   --  Path Traversal Prevention
   --  ==========================================================================

   function Contains_Path_Traversal_Pattern (Path : String) return Boolean is
   begin
      --  Check for .. patterns
      if Index (Path, "..") > 0 then
         return True;
      end if;

      --  Check for absolute paths (security risk in some contexts)
      if Path'Length > 0 and then 
         (Path (Path'First) = '/' or else Path (Path'First) = '\')
      then
         return True;
      end if;

      return False;
   end Contains_Path_Traversal_Pattern;

   function Sanitize_File_Path (Path : String) return Sanitization_Result.Result is
   begin
      if Contains_Path_Traversal_Pattern (Path) then
         return Sanitization_Result.Err
           ((Message => To_Unbounded_String ("Path traversal patterns not allowed"),
             Input   => To_Unbounded_String (Path)));
      end if;

      --  Additional validation for Windows drive letters
      if Path'Length >= 2 and then
         Path (Path'First + 1) = ':' and then
         Path (Path'First) in 'A' .. 'Z' | 'a' .. 'z'
      then
         return Sanitization_Result.Err
           ((Message => To_Unbounded_String ("Absolute paths not allowed"),
             Input   => To_Unbounded_String (Path)));
      end if;

      return Sanitization_Result.Ok (To_Unbounded_String (Path));
   end Sanitize_File_Path;

   --  ==========================================================================
   --  HTML/XML Encoding
   --  ==========================================================================

   function Contains_HTML_Metacharacters (Input : String) return Boolean is
   begin
      for C of Input loop
         case C is
            when '<' | '>' | '&' | '"' | ''' =>
               return True;
            when others =>
               null;
         end case;
      end loop;
      return False;
   end Contains_HTML_Metacharacters;

   function Encode_HTML (Input : String) return String is
      Result : Unbounded_String := Null_Unbounded_String;
   begin
      for C of Input loop
         case C is
            when '&' =>
               Append (Result, "&amp;");
            when '<' =>
               Append (Result, "&lt;");
            when '>' =>
               Append (Result, "&gt;");
            when '"' =>
               Append (Result, "&quot;");
            when ''' =>
               Append (Result, "&#39;");
            when others =>
               Append (Result, C);
         end case;
      end loop;
      return To_String (Result);
   end Encode_HTML;

   function Contains_XML_Metacharacters (Input : String) return Boolean is
   begin
      return Contains_HTML_Metacharacters (Input);
   end Contains_XML_Metacharacters;

   function Encode_XML (Input : String) return String is
   begin
      --  XML encoding is the same as HTML for basic cases
      return Encode_HTML (Input);
   end Encode_XML;

   --  ==========================================================================
   --  Whitelist Validation
   --  ==========================================================================

   function Is_Alphanumeric (Input : String) return Boolean is
   begin
      for C of Input loop
         if not ((C in 'A' .. 'Z') or (C in 'a' .. 'z') or (C in '0' .. '9')) then
            return False;
         end if;
      end loop;
      return True;
   end Is_Alphanumeric;

   function Is_Whitelisted
     (Input : String; Allowed_Chars : String) return Boolean is
   begin
      for C of Input loop
         if Index (Allowed_Chars, String'(1 => C)) = 0 then
            return False;
         end if;
      end loop;
      return True;
   end Is_Whitelisted;

   function Strip_Non_Whitelisted
     (Input : String; Allowed_Chars : String) return String is
      Result : Unbounded_String := Null_Unbounded_String;
   begin
      for C of Input loop
         if Index (Allowed_Chars, String'(1 => C)) > 0 then
            Append (Result, C);
         end if;
      end loop;
      return To_String (Result);
   end Strip_Non_Whitelisted;

   --  ==========================================================================
   --  General Utilities
   --  ==========================================================================

   function Truncate (Input : String; Max_Length : Truncation_Length_Type) return String is
   begin
      if Input'Length <= Natural(Max_Length) then
         return Input;
      else
         return Input (Input'First .. Input'First + Natural(Max_Length) - 1);
      end if;
   end Truncate;

   function Trim_Whitespace (Input : String) return String is
   begin
      return Trim (Input, Ada.Strings.Both);
   end Trim_Whitespace;

   function Normalize_Whitespace (Input : String) return String is
      Result : Unbounded_String := Null_Unbounded_String;
      Last_Was_Space : Boolean := False;
   begin
      for C of Input loop
         if C = ' ' or else 
            C = Ada.Characters.Latin_1.HT or else
            C = Ada.Characters.Latin_1.LF or else
            C = Ada.Characters.Latin_1.CR
         then
            if not Last_Was_Space then
               Append (Result, ' ');
               Last_Was_Space := True;
            end if;
         else
            Append (Result, C);
            Last_Was_Space := False;
         end if;
      end loop;

      --  Trim the result
      return Trim (To_String (Result), Ada.Strings.Both);
   end Normalize_Whitespace;

end Abohlib.Core.Domain.Utilities.Input_Sanitizer;