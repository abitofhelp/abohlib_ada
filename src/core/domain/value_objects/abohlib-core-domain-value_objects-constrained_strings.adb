--  =============================================================================
--  Abohlib.Core.Domain.Value_Objects.Constrained_Strings - Implementation
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

package body Abohlib.Core.Domain.Value_Objects.Constrained_Strings is

   function To_Bounded_Path (S : String) return Bounded_Path_String is
   begin
      return Result : Bounded_Path_String (S'Length) do
         Result.Value (1 .. S'Length) := S;
         Result.Length := S'Length;
      end return;
   end To_Bounded_Path;

   function To_Bounded_Filename (S : String) return Bounded_Filename_String is
   begin
      return Result : Bounded_Filename_String (S'Length) do
         Result.Value (1 .. S'Length) := S;
         Result.Length := S'Length;
      end return;
   end To_Bounded_Filename;

   function To_String (B : Bounded_Path_String) return String is
   begin
      return B.Value (1 .. B.Length);
   end To_String;

   function To_String (B : Bounded_Filename_String) return String is
   begin
      return B.Value (1 .. B.Length);
   end To_String;

end Abohlib.Core.Domain.Value_Objects.Constrained_Strings;
