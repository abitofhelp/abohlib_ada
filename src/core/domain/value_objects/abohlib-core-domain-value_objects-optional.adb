--  =============================================================================
--  Abohlib.Core.Domain.Value_Objects.Optional - Implementation
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

package body Abohlib.Core.Domain.Value_Objects.Optional is

   function Map (Opt : Optional_Type) return Optional_Type is
   begin
      if Opt.Has_Value then
         return Make_Some (Map_Function (Opt.Value));
      else
         return None;
      end if;
   end Map;

   function Filter (Opt : Optional_Type) return Optional_Type is
   begin
      if Opt.Has_Value and then Predicate (Opt.Value) then
         return Opt;
      else
         return None;
      end if;
   end Filter;

   overriding
   function "=" (Left, Right : Optional_Type) return Boolean is
   begin
      if Left.Has_Value and Right.Has_Value then
         return Left.Value = Right.Value;
      else
         return Left.Has_Value = Right.Has_Value;
      end if;
   end "=";

end Abohlib.Core.Domain.Value_Objects.Optional;