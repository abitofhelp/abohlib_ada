--  =============================================================================
--  Abohlib.Core.Domain.Value_Objects.Constrained_Strings - Type-Safe Strings
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Purpose:
--    Provides constrained string subtypes for enhanced type safety.
--    These types prevent common string-related errors through compile-time checks.
--  =============================================================================

pragma Ada_2022;

package Abohlib.Core.Domain.Value_Objects.Constrained_Strings is

   --  Path component constraints
   MAX_PATH_LENGTH : constant := 4_096;
   MAX_FILENAME_LENGTH : constant := 255;
   MAX_EXTENSION_LENGTH : constant := 127;

   --  Constrained string subtypes with validation predicates
   subtype Valid_Path_String is String (1 .. MAX_PATH_LENGTH)
   with
     Predicate =>
       Valid_Path_String'Length > 0
       and then (for all C of Valid_Path_String => C /= ASCII.NUL);

   subtype Valid_Filename_String is String (1 .. MAX_FILENAME_LENGTH)
   with
     Predicate =>
       Valid_Filename_String'Length > 0
       and then Valid_Filename_String'Length <= MAX_FILENAME_LENGTH
       and then (for all C of Valid_Filename_String
                 => C /= ASCII.NUL and C /= '/' and C /= '\');

   subtype Valid_Extension_String is String (1 .. MAX_EXTENSION_LENGTH)
   with
     Predicate =>
       Valid_Extension_String'Length >= 0
       and then Valid_Extension_String'Length <= MAX_EXTENSION_LENGTH
       and then (for all C of Valid_Extension_String
                 => C /= ASCII.NUL and C /= '/' and C /= '\');

   --  Bounded string types for dynamic sizing
   --  Note: No default discriminant to avoid Storage_Error warnings
   type Bounded_Path_String (Max_Length : Natural) is record
      Value  : String (1 .. Max_Length);
      Length : Natural := 0;
   end record
   with
     Predicate => Bounded_Path_String.Length <= Bounded_Path_String.Max_Length;

   type Bounded_Filename_String (Max_Length : Natural) is record
      Value  : String (1 .. Max_Length);
      Length : Natural := 0;
   end record
   with
     Predicate =>
       Bounded_Filename_String.Length <= Bounded_Filename_String.Max_Length;

   --  Convenience subtypes with common sizes
   subtype Default_Bounded_Path_String is
     Bounded_Path_String (MAX_PATH_LENGTH);
   subtype Default_Bounded_Filename_String is
     Bounded_Filename_String (MAX_FILENAME_LENGTH);

   --  Helper functions for bounded strings
   function To_Bounded_Path (S : String) return Bounded_Path_String
   with Pre => S'Length <= MAX_PATH_LENGTH;

   function To_Bounded_Filename (S : String) return Bounded_Filename_String
   with Pre => S'Length <= MAX_FILENAME_LENGTH;

   function To_String (B : Bounded_Path_String) return String
   with Post => To_String'Result'Length = B.Length;

   function To_String (B : Bounded_Filename_String) return String
   with Post => To_String'Result'Length = B.Length;

   --  Validation functions
   function Is_Valid_Path_Character (C : Character) return Boolean
   is (C /= ASCII.NUL);

   function Is_Valid_Filename_Character (C : Character) return Boolean
   is (C /= ASCII.NUL
       and C /= '/'
       and C /= '\'
       and C /= '<'
       and C /= '>'
       and C /= ':'
       and C /= '"'
       and C /= '|'
       and C /= '?'
       and C /= '*');

end Abohlib.Core.Domain.Value_Objects.Constrained_Strings;
