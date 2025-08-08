--  =============================================================================
--  Abohlib.Core.Application.Use_Cases.Simple_Validator - Simple Validation Use Case
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Purpose:
--    Simple validation use case for demonstrating application layer functionality
--    and increasing test coverage without complex error handling.
--  =============================================================================

pragma Ada_2022;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Abohlib.Core.Domain.Result;
with Abohlib.Core.Domain.Types.Strings; use Abohlib.Core.Domain.Types.Strings;

package Abohlib.Core.Application.Use_Cases.Simple_Validator is

   --  Constants
   Default_Max_String_Length : constant String_Length_Type := 1000;  -- Default maximum string length for validation

   --  ==========================================================================
   --  Simple Result Types
   --  ==========================================================================

   package Validation_Result is new
     Abohlib.Core.Domain.Result.Result_Package
       (Ok_Type  => Boolean,
        Err_Type => Unbounded_String);

   package String_Result is new
     Abohlib.Core.Domain.Result.Result_Package
       (Ok_Type  => Unbounded_String,
        Err_Type => Unbounded_String);

   --  ==========================================================================
   --  Simple Validator Use Case
   --  ==========================================================================

   type Simple_Validator is tagged limited null record;

   --  Validate string length
   function Validate_Length
     (Self       : Simple_Validator;
      Input      : String;
      Min_Length : String_Length_Type := 1;
      Max_Length : String_Length_Type := Default_Max_String_Length) return Validation_Result.Result
   with Pre => Min_Length <= Max_Length;

   --  Validate string contains only alphanumeric characters
   function Validate_Alphanumeric
     (Self : Simple_Validator; Input : String) return Validation_Result.Result;

   --  Sanitize string by removing special characters
   function Sanitize_String
     (Self : Simple_Validator; Input : String) return String_Result.Result;

   --  Batch validate multiple strings
   type String_Array is array (Positive range <>) of Unbounded_String;

   function Validate_Batch
     (Self : Simple_Validator; Inputs : String_Array)
      return Validation_Result.Result
   with Pre => Inputs'Length > 0 and then Inputs'Length <= 100;

   --  Check if string is safe (no special characters)
   function Is_Safe_String
     (Self : Simple_Validator; Input : String) return Boolean;

end Abohlib.Core.Application.Use_Cases.Simple_Validator;
