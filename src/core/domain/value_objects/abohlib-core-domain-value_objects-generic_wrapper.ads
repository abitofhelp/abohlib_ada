--  =============================================================================
--  Abohlib.Core.Domain.Value_Objects.Generic_Wrapper - Type-Safe Value Object Wrapper
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT

--  Purpose:
--    Generic type-safe wrapper for creating value objects with validation,
--    immutability, and domain-specific operations. Reduces boilerplate for
--    simple value objects while maintaining type safety.

--  Usage:
--    Instantiate this generic package to create type-safe wrappers around
--    primitive types or records, automatically getting validation, builders,
--    and optional operations like ordering and arithmetic.

--  Example:
--    function Is_Valid_Age (Age : Natural) return Boolean is (Age <= 150);
--    function Age_To_String (Age : Natural) return String is (Age'Image);
--    function String_To_Age (S : String) return Natural is (Natural'Value (S));

--    package Age_Value is new Generic_Wrapper
--       (Wrapped_Type => Natural,
--        Value_Object_Name => "Age",
--        Is_Valid_Value => Is_Valid_Age,
--        Value_To_String => Age_To_String,
--        String_To_Value => String_To_Age,
--        Default_Value => 0);

pragma Ada_2022;

with Ada.Finalization;
with Ada.Strings.Unbounded;
pragma Unreferenced (Ada.Strings.Unbounded);
use Ada.Strings.Unbounded;

generic
   --  The wrapped type
   type Wrapped_Type is private;

   --  Value object name for error messages
   Value_Object_Name : String;

   --  Validation function
   with function Is_Valid_Value (Value : Wrapped_Type) return Boolean;

   --  String conversion functions
   with function Value_To_String (Value : Wrapped_Type) return String;
   with function String_To_Value (Str : String) return Wrapped_Type;

   --  Equality (if not predefined)
   with function "=" (Left, Right : Wrapped_Type) return Boolean is <>;

   --  Optional: Default value
   Default_Value : Wrapped_Type;

package Abohlib.Core.Domain.Value_Objects.Generic_Wrapper
is
   pragma Preelaborate;

   --  The value object type
   type Value_Type is new Ada.Finalization.Controlled with private
   with Type_Invariant => Is_Valid (Value_Type);

   --  Constructors

   --  Create with validation
   function Create (Value : Wrapped_Type) return Value_Type
   with
     Pre => Is_Valid_Value (Value),
     Post => Get_Value (Create'Result) = Value;

   --  Exception raised when creating invalid value
   Invalid_Value_Error : exception;

   --  Create from string
   function From_String (Str : String) return Value_Type
   with
     Pre => Is_Valid_String (Str),
     Post => To_String (From_String'Result) = Str;

   --  Create from string (raises Invalid_Value_Error if invalid)
   --  Use From_String with Is_Valid_String check for safe creation

   --  Create default value
   function Create_Default return Value_Type
   with Post => Get_Value (Create_Default'Result) = Default_Value;

   --  Accessors

   --  Get the wrapped value
   function Get_Value (Self : Value_Type) return Wrapped_Type
   with Inline;

   --  Get string representation
   function To_String (Self : Value_Type) return String
   with Inline, Post => To_String'Result = Value_To_String (Get_Value (Self));

   --  Get display string (with name)
   function Display (Self : Value_Type) return String
   with Post => Display'Result'Length > 0;

   --  Validation

   --  Check if value object is valid
   function Is_Valid (Self : Value_Type) return Boolean
   with Inline;

   --  Check if string can create valid value
   function Is_Valid_String (Str : String) return Boolean;

   --  Comparison operators

   overriding
   function "=" (Left, Right : Value_Type) return Boolean
   with Inline;

   --  Optional: Ordering (if wrapped type supports it)
   generic
      with function "<" (Left, Right : Wrapped_Type) return Boolean is <>;
   package Ordered is
      function "<" (Left, Right : Value_Type) return Boolean
      with Inline;

      function "<=" (Left, Right : Value_Type) return Boolean
      with Inline;

      function ">" (Left, Right : Value_Type) return Boolean
      with Inline;

      function ">=" (Left, Right : Value_Type) return Boolean
      with Inline;

      --  Min/Max operations
      function Min (Left, Right : Value_Type) return Value_Type
      with Post => Min'Result = Left or else Min'Result = Right;

      function Max (Left, Right : Value_Type) return Value_Type
      with Post => Max'Result = Left or else Max'Result = Right;
   end Ordered;

   --  Optional: Arithmetic operations (if wrapped type supports them)
   generic
      with function "+" (Left, Right : Wrapped_Type) return Wrapped_Type is <>;
      with function "-" (Left, Right : Wrapped_Type) return Wrapped_Type is <>;
      with function "*" (Left, Right : Wrapped_Type) return Wrapped_Type is <>;
      with function "/" (Left, Right : Wrapped_Type) return Wrapped_Type is <>;
   package Arithmetic is
      function "+" (Left, Right : Value_Type) return Value_Type;
      function "-" (Left, Right : Value_Type) return Value_Type;
      function "*" (Left, Right : Value_Type) return Value_Type;
      function "/" (Left, Right : Value_Type) return Value_Type;

      --  Safe arithmetic with validation
      function Add_Safe
        (Left, Right : Value_Type; Result : out Value_Type) return Boolean;
      function Sub_Safe
        (Left, Right : Value_Type; Result : out Value_Type) return Boolean;
      function Mul_Safe
        (Left, Right : Value_Type; Result : out Value_Type) return Boolean;
      function Div_Safe
        (Left, Right : Value_Type; Result : out Value_Type) return Boolean;
   end Arithmetic;

   --  Transform operations

   --  Map function over value
   generic
      type Target_Type is private;
      with function Transform (Value : Wrapped_Type) return Target_Type;
   function Map (Self : Value_Type) return Target_Type;

   --  Apply function with validation
   generic
      with function Apply (Value : Wrapped_Type) return Wrapped_Type;
   function Apply_Checked (Self : Value_Type) return Value_Type;

   --  Builder pattern support
   type Value_Builder is tagged private;

   --  Create builder
   function Builder return Value_Builder
   with Post => not Is_Built (Builder'Result);

   --  Set value
   function With_Value
     (B : Value_Builder; Value : Wrapped_Type) return Value_Builder
   with Post => not Is_Built (With_Value'Result);

   --  Build value object
   function Build (B : Value_Builder'Class) return Value_Type
   with
     Pre =>
       not Is_Built (B)
       and then Has_Value (B)
       and then Is_Valid_Value (Get_Value (B)),
     Post => Get_Value (Build'Result) = Get_Value (B);

   --  Safe build with validation check
   function Build_Safe
     (B : Value_Builder'Class; Result : out Value_Type) return Boolean
   with Pre => not Is_Built (B);

   --  Check if builder has been used
   function Is_Built (B : Value_Builder) return Boolean;

   --  Builder helper functions
   function Has_Value (B : Value_Builder) return Boolean;
   function Get_Value (B : Value_Builder) return Wrapped_Type;

   --  Utility functions

   --  Get value object type name
   function Type_Name return String
   is (Value_Object_Name);

   --  Validation error message
   function Validation_Error_Message (Value : Wrapped_Type) return String;

   --  Image function for debugging
   function Image (Self : Value_Type) return String renames Display;

private

   type Value_Type is new Ada.Finalization.Controlled with record
      Value : Wrapped_Type := Default_Value;
   end record;

   --  Inline implementations
   function Get_Value (Self : Value_Type) return Wrapped_Type
   is (Self.Value);

   function To_String (Self : Value_Type) return String
   is (Value_To_String (Self.Value));

   function Is_Valid (Self : Value_Type) return Boolean
   is (Is_Valid_Value (Self.Value));

   overriding
   function "=" (Left, Right : Value_Type) return Boolean
   is (Left.Value = Right.Value);

   --  Builder implementation
   type Value_Builder is tagged record
      Value          : Wrapped_Type := Default_Value;
      Has_Value_Flag : Boolean := False;
      Built          : Boolean := False;
   end record
   with Type_Invariant => 
      (if Value_Builder.Built then Value_Builder.Has_Value_Flag);

end Abohlib.Core.Domain.Value_Objects.Generic_Wrapper;
