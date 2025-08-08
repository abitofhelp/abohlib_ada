--  =============================================================================
--  Abohlib.Core.Domain.Constants.Limits - Domain Business Rule Limits
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Purpose:
--    Provides domain-specific limits and constraints that represent business
--    rules. These constants define the boundaries of valid domain operations.
--
--  Usage:
--    Import this package to access domain-specific limits for validation
--    and business rule enforcement within the domain layer.
--
--  Example Domain Constants:
--    These are examples of true domain constants that represent business rules,
--    not technical implementation details.
--  =============================================================================

pragma Ada_2022;

with Abohlib.Core.Domain.Constants.Bytes;
with Abohlib.Core.Domain.Types.Bytes; use Abohlib.Core.Domain.Types.Bytes;
with Abohlib.Core.Domain.Types.Strings; use Abohlib.Core.Domain.Types.Strings;
with Abohlib.Core.Domain.Types.Counts; use Abohlib.Core.Domain.Types.Counts;

package Abohlib.Core.Domain.Constants.Limits is

   --  Example: Maximum file size for document uploads (business rule)
   --  Uses the technical constant but applies business logic
   MAX_DOCUMENT_SIZE : constant Abohlib.Core.Domain.Types.Bytes.File_Size_Type :=
     Abohlib.Core.Domain.Types.Bytes.File_Size_Type 
       (Long_Long_Integer (10) * Long_Long_Integer (Abohlib.Core.Domain.Constants.Bytes.IEC_MiB));

   --  Example: Maximum name lengths (business rules)
   MAX_USERNAME_LENGTH : constant Username_Length_Type := 50;
   MAX_FILENAME_LENGTH : constant Filename_Length_Type := 255;

   --  Example: Business-specific limits
   MAX_CONCURRENT_SESSIONS : constant Session_Count_Type := 100;
   MIN_PASSWORD_LENGTH : constant Password_Length_Type := 8;
   MAX_PASSWORD_LENGTH : constant Password_Length_Type := 128;

end Abohlib.Core.Domain.Constants.Limits;
