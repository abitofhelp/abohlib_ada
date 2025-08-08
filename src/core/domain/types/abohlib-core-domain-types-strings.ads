--  =============================================================================
--  Abohlib.Core.Domain.Types.Strings - String Length Strong Types
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Purpose:
--    Provides strong types for string length constraints to enforce
--    domain-specific limits at compile time and runtime.
--
--  Design:
--    - Base type for general string lengths
--    - Subtypes with specific ranges for different uses
--    - Prevents buffer overflows and enforces business rules
--    - Clear API contracts through type constraints
--  =============================================================================

pragma Ada_2022;

package Abohlib.Core.Domain.Types.Strings is

   --  ==========================================================================
   --  Base String Length Type
   --  ==========================================================================
   
   --  Base type for all string length measurements
   type String_Length_Type is new Natural;
   
   --  ==========================================================================
   --  Domain-Specific String Length Constraints
   --  ==========================================================================
   
   --  User-related strings
   type Username_Length_Type is new String_Length_Type
     with Static_Predicate => Username_Length_Type in 0 .. 50;
   type Password_Length_Type is new String_Length_Type
     with Static_Predicate => Password_Length_Type in 0 .. 128;
   type Email_Length_Type is new String_Length_Type
     with Static_Predicate => Email_Length_Type in 0 .. 254;
   
   --  File system strings
   type Filename_Length_Type is new String_Length_Type
     with Static_Predicate => Filename_Length_Type in 0 .. 255;
   type Path_Length_Type is new String_Length_Type
     with Static_Predicate => Path_Length_Type in 0 .. 4096;
   
   --  Network-related strings
   type URL_Length_Type is new String_Length_Type
     with Static_Predicate => URL_Length_Type in 0 .. 2048;
   
   --  Logging strings
   type Log_Key_Length_Type is new String_Length_Type
     with Static_Predicate => Log_Key_Length_Type in 0 .. 32;
   type Log_Value_Length_Type is new String_Length_Type
     with Static_Predicate => Log_Value_Length_Type in 0 .. 256;
   type Log_Message_Length_Type is new String_Length_Type
     with Static_Predicate => Log_Message_Length_Type in 0 .. 1024;
   
   --  ==========================================================================
   --  String Operation Types
   --  ==========================================================================
   
   --  Truncation length for string operations
   type Truncation_Length_Type is new String_Length_Type
     with Static_Predicate => Truncation_Length_Type > 0;

end Abohlib.Core.Domain.Types.Strings;