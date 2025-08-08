--  =============================================================================
--  Abohlib.Core.Domain.Result.Functional - Implementation
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Implementation Notes:
--    This package implements functional combinators for Result types.
--    Each operation follows a consistent pattern:
--    1. Check if the Result is Ok or Err
--    2. Apply the appropriate transformation
--    3. Return a new Result with the transformed value
--
--    The implementations assume that when error types or ok types need
--    to be preserved between source and target Results, they are the same
--    type. This is enforced through the generic formal parameters.
--  =============================================================================

pragma Ada_2022;

package body Abohlib.Core.Domain.Result.Functional is

   --  ==========================================================================
   --  Map Operation
   --  ==========================================================================
   function Map (R : Source_Result.Result) return Target_Result.Result is
   begin
      if Source_Result.Is_Ok (R) then
         --  Apply the transformation to the Ok value
         return Target_Result.Ok (Transform (Source_Result.Get_Ok (R)));
      else
         --  Pass through the error unchanged
         --  Note: This works because both Results share the same Err_Type
         return Target_Result.Err (Source_Result.Get_Err (R));
      end if;
   end Map;

   --  ==========================================================================
   --  Map_Err Operation
   --  ==========================================================================
   function Map_Err (R : Source_Result.Result) return Target_Result.Result is
   begin
      if Source_Result.Is_Err (R) then
         --  Apply the transformation to the Err value
         return Target_Result.Err (Transform (Source_Result.Get_Err (R)));
      else
         --  Pass through the Ok value unchanged
         --  Note: This works because both Results share the same Ok_Type
         return Target_Result.Ok (Source_Result.Get_Ok (R));
      end if;
   end Map_Err;

   --  ==========================================================================
   --  And_Then Operation
   --  ==========================================================================
   function And_Then (R : Source_Result.Result) return Target_Result.Result is
   begin
      if Source_Result.Is_Ok (R) then
         --  Apply the operation that returns a Result
         --  This "flattens" the Result<Result<T>> into Result<T>
         return Operation (Source_Result.Get_Ok (R));
      else
         --  Short-circuit: pass through the error without calling Operation
         --  Note: This works because both Results share the same Err_Type
         return Target_Result.Err (Source_Result.Get_Err (R));
      end if;
   end And_Then;

   --  ==========================================================================
   --  Or_Else Operation
   --  ==========================================================================
   function Or_Else (R : Result_Pkg.Result) return Result_Pkg.Result is
   begin
      if Result_Pkg.Is_Ok (R) then
         return R;
      else
         return Alternative (Result_Pkg.Get_Err (R));
      end if;
   end Or_Else;

   --  ==========================================================================
   --  Map_Same Operation
   --  ==========================================================================
   function Map_Same (R : Result_Pkg.Result) return Result_Pkg.Result is
   begin
      if Result_Pkg.Is_Ok (R) then
         return Result_Pkg.Ok (Transform (Result_Pkg.Get_Ok (R)));
      else
         return R;
      end if;
   end Map_Same;

   --  ==========================================================================
   --  Map_Err_Same Operation
   --  ==========================================================================
   function Map_Err_Same (R : Result_Pkg.Result) return Result_Pkg.Result is
   begin
      if Result_Pkg.Is_Err (R) then
         return Result_Pkg.Err (Transform (Result_Pkg.Get_Err (R)));
      else
         return R;
      end if;
   end Map_Err_Same;

   --  ==========================================================================
   --  Unwrap_Or Operation
   --  ==========================================================================
   function Unwrap_Or (R : Result_Pkg.Result; Default : Result_Pkg.Ok_Type)
      return Result_Pkg.Ok_Type is
   begin
      if Result_Pkg.Is_Ok (R) then
         return Result_Pkg.Get_Ok (R);
      else
         return Default;
      end if;
   end Unwrap_Or;

   --  ==========================================================================
   --  Unwrap_Or_Else Operation
   --  ==========================================================================
   function Unwrap_Or_Else (R : Result_Pkg.Result) return Result_Pkg.Ok_Type is
   begin
      if Result_Pkg.Is_Ok (R) then
         return Result_Pkg.Get_Ok (R);
      else
         return Default_Fn (Result_Pkg.Get_Err (R));
      end if;
   end Unwrap_Or_Else;

   --  ==========================================================================
   --  Is_Ok_And Operation
   --  ==========================================================================
   function Is_Ok_And (R : Result_Pkg.Result) return Boolean is
   begin
      return Result_Pkg.Is_Ok (R) and then
             Predicate (Result_Pkg.Get_Ok (R));
   end Is_Ok_And;

   --  ==========================================================================
   --  Is_Err_And Operation
   --  ==========================================================================
   function Is_Err_And (R : Result_Pkg.Result) return Boolean is
   begin
      return Result_Pkg.Is_Err (R) and then
             Predicate (Result_Pkg.Get_Err (R));
   end Is_Err_And;

end Abohlib.Core.Domain.Result.Functional;