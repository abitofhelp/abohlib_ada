--  =============================================================================
--  Abohlib.Core.Domain.Result - Result-Based Error Handling Pattern
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Purpose:
--    Provides the Result<T,E> pattern for functional error handling throughout
--    the library. This approach avoids using exceptions for control flow while
--    maintaining type safety and composability.
--
--  Usage:
--    The Result type represents either a successful value (Ok) or an error (Err).
--    This follows functional programming principles and provides explicit error
--    handling without exceptions.
--
--  Example:
--    function Divide (A, B : Float) return Result is
--    begin
--       if B = 0.0 then
--          return Err ("Division by zero");
--       else
--          return Ok (A / B);
--       end if;
--    end Divide;
--  =============================================================================

pragma Ada_2022;

with Ada.Finalization;

package Abohlib.Core.Domain.Result is
   pragma Preelaborate;

   --  ==========================================================================
   --  Result Type Definition
   --  ==========================================================================
   --  The Result type represents either a successful value (Ok) or an error (Err).
   --  This follows functional programming principles and provides explicit error
   --  handling without exceptions.

   generic
      type Ok_Type is private;
      type Err_Type is private;
   package Result_Package is

      type Result_Variant is (Ok_Variant, Err_Variant);
      type Result (Variant : Result_Variant) is
        new Ada.Finalization.Controlled with private;

      --  Constructors
      function Ok (Value : Ok_Type) return Result
      with Post => Is_Ok (Ok'Result) and then Get_Ok (Ok'Result) = Value;

      function Err (Error : Err_Type) return Result
      with Post => Is_Err (Err'Result) and then Get_Err (Err'Result) = Error;

      --  Query functions
      function Is_Ok (R : Result) return Boolean
      with Post => Is_Ok'Result = (R.Variant = Ok_Variant);
      pragma Inline (Is_Ok);

      function Is_Err (R : Result) return Boolean
      with Post => Is_Err'Result = (R.Variant = Err_Variant);
      pragma Inline (Is_Err);

      --  Value extraction (raises Constraint_Error if wrong variant accessed)
      function Get_Ok (R : Result) return Ok_Type
      with Pre => Is_Ok (R);

      function Get_Err (R : Result) return Err_Type
      with Pre => Is_Err (R);

      --  Safe value extraction with defaults
      function Get_Ok_Or (R : Result; Default : Ok_Type) return Ok_Type
      with
        Post =>
          (if Is_Ok (R) then Get_Ok_Or'Result = Get_Ok (R)
           else Get_Ok_Or'Result = Default);

      function Get_Err_Or (R : Result; Default : Err_Type) return Err_Type
      with
        Post =>
          (if Is_Err (R) then Get_Err_Or'Result = Get_Err (R)
           else Get_Err_Or'Result = Default);

      --  Note: Functional operations like Map, Map_Err, and And_Then that would
      --  transform between different Result types cannot be defined within this
      --  generic package due to Ada's restrictions on generic instantiations

      --  Pattern matching
      generic
         type Return_Type is private;
      function Match
        (R        : Result;
         Ok_Case  : access function (V : Ok_Type) return Return_Type;
         Err_Case : access function (E : Err_Type) return Return_Type)
         return Return_Type;

   private
      type Result (Variant : Result_Variant) is new Ada.Finalization.Controlled
      with record
         case Variant is
            when Ok_Variant =>
               Ok_Value : Ok_Type;

            when Err_Variant =>
               Err_Value : Err_Type;
         end case;
      end record;

   end Result_Package;

   --  ==========================================================================
   --  Result Utilities
   --  ==========================================================================

   --  Try to execute an operation, converting exceptions to Result
   generic
      type T is private;
      type E is private;
      with package Result_Type is new Result_Package (T, E);
      with function Try_Operation return T;
      with function Create_Error (Message : String) return E;
   function Try_As_Result return Result_Type.Result;

   --  Execute multiple operations, stopping at first error
   generic
      type T is private;
      type E is private;
      type Operation_Array is array (Positive range <>)
      of access function return T;
      with package Result_Type is new Result_Package (T, E);
      with function Create_Error (Message : String) return E;
   function Try_All (Operations : Operation_Array) return Result_Type.Result;

   --  Collect results from multiple operations
   --  NOTE: Commented out due to Ada limitation with unconstrained array types
   --  generic
   --     type T is private;
   --     type E is private;
   --     type T_Array is array (Positive range <>) of T;
   --     with package Result_Type is new Result_Package (T, E);
   --     with package Array_Result is new Result_Package (T_Array, E);
   --     type Result_Array is array (Positive range <>) of Result_Type.Result;
   --  function Collect_Results
   --    (Results : Result_Array) return Array_Result.Result;

end Abohlib.Core.Domain.Result;
