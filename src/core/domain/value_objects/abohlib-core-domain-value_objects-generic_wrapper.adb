--  =============================================================================
--  Abohlib.Core.Domain.Value_Objects.Generic_Wrapper - Implementation
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

with Ada.Exceptions;

package body Abohlib.Core.Domain.Value_Objects.Generic_Wrapper is

   function Create (Value : Wrapped_Type) return Value_Type is
   begin
      if not Is_Valid_Value (Value) then
         raise Constraint_Error
           with
             "Invalid "
             & Value_Object_Name
             & ": "
             & Validation_Error_Message (Value);
      end if;
      return (Ada.Finalization.Controlled with Value => Value);
   end Create;

   function From_String (Str : String) return Value_Type is
   begin
      begin
         declare
            Value : constant Wrapped_Type := String_To_Value (Str);
         begin
            return Create (Value);
         end;
      exception
         when E : others =>
            raise Constraint_Error
              with
                "Cannot create "
                & Value_Object_Name
                & " from string '"
                & Str
                & "': "
                & Ada.Exceptions.Exception_Message (E);
      end;
   end From_String;

   function Create_Default return Value_Type is
   begin
      return Create (Default_Value);
   end Create_Default;

   function Display (Self : Value_Type) return String is
   begin
      return Value_Object_Name & "(" & To_String (Self) & ")";
   end Display;

   function Is_Valid_String (Str : String) return Boolean is
   begin
      declare
         Value : constant Wrapped_Type := String_To_Value (Str);
      begin
         return Is_Valid_Value (Value);
      end;
   exception
      when others =>
         return False;
   end Is_Valid_String;

   package body Ordered is

      function "<" (Left, Right : Value_Type) return Boolean is
      begin
         return Left.Value < Right.Value;
      end "<";

      function "<=" (Left, Right : Value_Type) return Boolean is
      begin
         return Left.Value < Right.Value or else Left.Value = Right.Value;
      end "<=";

      function ">" (Left, Right : Value_Type) return Boolean is
      begin
         return Right.Value < Left.Value;
      end ">";

      function ">=" (Left, Right : Value_Type) return Boolean is
      begin
         return not (Left.Value < Right.Value);
      end ">=";

      function Min (Left, Right : Value_Type) return Value_Type is
      begin
         if Left < Right then
            return Left;
         else
            return Right;
         end if;
      end Min;

      function Max (Left, Right : Value_Type) return Value_Type is
      begin
         if Left > Right then
            return Left;
         else
            return Right;
         end if;
      end Max;

   end Ordered;

   package body Arithmetic is

      function "+" (Left, Right : Value_Type) return Value_Type is
         Result_Value : constant Wrapped_Type := Left.Value + Right.Value;
      begin
         return Create (Result_Value);
      end "+";

      function "-" (Left, Right : Value_Type) return Value_Type is
         Result_Value : constant Wrapped_Type := Left.Value - Right.Value;
      begin
         return Create (Result_Value);
      end "-";

      function "*" (Left, Right : Value_Type) return Value_Type is
         Result_Value : constant Wrapped_Type := Left.Value * Right.Value;
      begin
         return Create (Result_Value);
      end "*";

      function "/" (Left, Right : Value_Type) return Value_Type is
         Result_Value : constant Wrapped_Type := Left.Value / Right.Value;
      begin
         return Create (Result_Value);
      end "/";

      function Add_Safe
        (Left, Right : Value_Type; Result : out Value_Type) return Boolean
      is
         Result_Value : constant Wrapped_Type := Left.Value + Right.Value;
      begin
         if Is_Valid_Value (Result_Value) then
            Result := Create (Result_Value);
            return True;
         else
            return False;
         end if;
      exception
         when others =>
            return False;
      end Add_Safe;

      function Sub_Safe
        (Left, Right : Value_Type; Result : out Value_Type) return Boolean
      is
         Result_Value : constant Wrapped_Type := Left.Value - Right.Value;
      begin
         if Is_Valid_Value (Result_Value) then
            Result := Create (Result_Value);
            return True;
         else
            return False;
         end if;
      exception
         when others =>
            return False;
      end Sub_Safe;

      function Mul_Safe
        (Left, Right : Value_Type; Result : out Value_Type) return Boolean
      is
         Result_Value : constant Wrapped_Type := Left.Value * Right.Value;
      begin
         if Is_Valid_Value (Result_Value) then
            Result := Create (Result_Value);
            return True;
         else
            return False;
         end if;
      exception
         when others =>
            return False;
      end Mul_Safe;

      function Div_Safe
        (Left, Right : Value_Type; Result : out Value_Type) return Boolean is
      begin
         declare
            Result_Value : constant Wrapped_Type := Left.Value / Right.Value;
         begin
            if Is_Valid_Value (Result_Value) then
               Result := Create (Result_Value);
               return True;
            else
               return False;
            end if;
         end;
      exception
         when others =>
            return False;
      end Div_Safe;

   end Arithmetic;

   function Map (Self : Value_Type) return Target_Type is
   begin
      return Transform (Self.Value);
   end Map;

   function Apply_Checked (Self : Value_Type) return Value_Type is
      New_Value : constant Wrapped_Type := Apply (Self.Value);
   begin
      return Create (New_Value);
   end Apply_Checked;

   function Builder return Value_Builder is
   begin
      return (Value => Default_Value, Has_Value_Flag => False, Built => False);
   end Builder;

   function With_Value
     (B : Value_Builder; Value : Wrapped_Type) return Value_Builder
   is
      Result : Value_Builder := B;
   begin
      Result.Value := Value;
      Result.Has_Value_Flag := True;
      return Result;
   end With_Value;

   function Build (B : Value_Builder'Class) return Value_Type is
   begin
      return Create (B.Value);
   end Build;

   function Build_Safe
     (B : Value_Builder'Class; Result : out Value_Type) return Boolean is
   begin
      if B.Built or else not B.Has_Value_Flag then
         return False;
      end if;
      if Is_Valid_Value (B.Value) then
         Result := Create (B.Value);
         return True;
      else
         return False;
      end if;
   end Build_Safe;

   function Is_Built (B : Value_Builder) return Boolean is
   begin
      return B.Built;
   end Is_Built;

   function Validation_Error_Message (Value : Wrapped_Type) return String is
   begin
      return "Value '" & Value_To_String (Value) & "' is invalid";
   end Validation_Error_Message;

   function Has_Value (B : Value_Builder) return Boolean is
   begin
      return B.Has_Value_Flag;
   end Has_Value;

   function Get_Value (B : Value_Builder) return Wrapped_Type is
   begin
      return B.Value;
   end Get_Value;

end Abohlib.Core.Domain.Value_Objects.Generic_Wrapper;
