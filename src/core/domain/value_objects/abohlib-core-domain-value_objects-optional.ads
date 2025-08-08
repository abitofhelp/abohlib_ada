--  =============================================================================
--  Abohlib.Core.Domain.Value_Objects.Optional - Generic Optional/Maybe Type
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Generic implementation of the Optional/Maybe pattern for nullable values.
--  Provides type-safe handling of values that may or may not be present.
--  =============================================================================

pragma Ada_2022;

generic
   type Value_Type is private;
   Default_Value : Value_Type;

package Abohlib.Core.Domain.Value_Objects.Optional
with Pure
is

   --  Optional type definition
   type Optional_Type is private;

   --  Constructors
   function Make_Some (Value : Value_Type) return Optional_Type
   with Post => Is_Present (Make_Some'Result) and then
                Get (Make_Some'Result) = Value;

   function None return Optional_Type
   with Post => Is_Empty (None'Result);

   --  Accessors
   function Is_Present (Opt : Optional_Type) return Boolean
   with Inline;

   function Is_Empty (Opt : Optional_Type) return Boolean
   with Inline;

   function Get (Opt : Optional_Type) return Value_Type
   with Pre => Is_Present (Opt);

   function Get_Or_Default (Opt : Optional_Type) return Value_Type
   with Inline;

   function Get_Or_Else
     (Opt : Optional_Type; Default : Value_Type) return Value_Type
   with Inline;

   --  Functional operations
   generic
      with function Map_Function (Value : Value_Type) return Value_Type;
   function Map (Opt : Optional_Type) return Optional_Type;

   generic
      with function Predicate (Value : Value_Type) return Boolean;
   function Filter (Opt : Optional_Type) return Optional_Type;

   --  Equality
   overriding
   function "=" (Left, Right : Optional_Type) return Boolean;

private

   type Optional_Type is record
      Has_Value : Boolean := False;
      Value     : Value_Type := Default_Value;
   end record;

   function Make_Some (Value : Value_Type) return Optional_Type
   is (Has_Value => True, Value => Value);

   function None return Optional_Type
   is (Has_Value => False, Value => Default_Value);

   function Is_Present (Opt : Optional_Type) return Boolean
   is (Opt.Has_Value);

   function Is_Empty (Opt : Optional_Type) return Boolean
   is (not Opt.Has_Value);

   function Get (Opt : Optional_Type) return Value_Type
   is (Opt.Value);

   function Get_Or_Default (Opt : Optional_Type) return Value_Type
   is (if Opt.Has_Value then Opt.Value else Default_Value);

   function Get_Or_Else
     (Opt : Optional_Type; Default : Value_Type) return Value_Type
   is (if Opt.Has_Value then Opt.Value else Default);

end Abohlib.Core.Domain.Value_Objects.Optional;