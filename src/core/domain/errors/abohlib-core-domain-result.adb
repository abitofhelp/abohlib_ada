--  =============================================================================
--  Abohlib.Core.Domain.Result - Implementation
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

with Ada.Exceptions;

package body Abohlib.Core.Domain.Result is

   --  ==========================================================================
   --  Result_Package Implementation
   --  ==========================================================================

   package body Result_Package is

      --  Constructors
      function Ok (Value : Ok_Type) return Result is
      begin
         return
           (Ada.Finalization.Controlled
            with Variant  => Ok_Variant,
                 Ok_Value => Value);
      end Ok;

      function Err (Error : Err_Type) return Result is
      begin
         return
           (Ada.Finalization.Controlled
            with Variant   => Err_Variant,
                 Err_Value => Error);
      end Err;

      --  Query functions
      function Is_Ok (R : Result) return Boolean is
      begin
         return R.Variant = Ok_Variant;
      end Is_Ok;

      function Is_Err (R : Result) return Boolean is
      begin
         return R.Variant = Err_Variant;
      end Is_Err;

      --  Value extraction
      function Get_Ok (R : Result) return Ok_Type is
      begin
         if R.Variant /= Ok_Variant then
            raise Constraint_Error
              with "Attempted to get Ok value from Err result";
         end if;
         return R.Ok_Value;
      end Get_Ok;

      function Get_Err (R : Result) return Err_Type is
      begin
         if R.Variant /= Err_Variant then
            raise Constraint_Error
              with "Attempted to get Err value from Ok result";
         end if;
         return R.Err_Value;
      end Get_Err;

      --  Safe value extraction with defaults
      function Get_Ok_Or (R : Result; Default : Ok_Type) return Ok_Type is
      begin
         if Is_Ok (R) then
            return R.Ok_Value;
         else
            return Default;
         end if;
      end Get_Ok_Or;

      function Get_Err_Or (R : Result; Default : Err_Type) return Err_Type is
      begin
         if Is_Err (R) then
            return R.Err_Value;
         else
            return Default;
         end if;
      end Get_Err_Or;

      --  Pattern matching
      function Match
        (R        : Result;
         Ok_Case  : access function (V : Ok_Type) return Return_Type;
         Err_Case : access function (E : Err_Type) return Return_Type)
         return Return_Type is
      begin
         if Is_Ok (R) then
            return Ok_Case (R.Ok_Value);
         else
            return Err_Case (R.Err_Value);
         end if;
      end Match;

   end Result_Package;

   --  ==========================================================================
   --  Result Utilities Implementation
   --  ==========================================================================

   function Try_As_Result return Result_Type.Result is
   begin
      declare
         Value : constant T := Try_Operation;
      begin
         return Result_Type.Ok (Value);
      end;
   exception
      when E : others =>
         return
           Result_Type.Err
             (Create_Error (Ada.Exceptions.Exception_Message (E)));
   end Try_As_Result;

   function Try_All (Operations : Operation_Array) return Result_Type.Result is
   begin
      for Op of Operations loop
         declare
            Value : constant T := Op.all;
            pragma Unreferenced (Value);
         begin
            --  Continue to next operation
            null;
         end;
      end loop;

      --  All operations succeeded, return last result
      if Operations'Length > 0 then
         return Result_Type.Ok (Operations (Operations'Last).all);
      else
         --  No operations provided - return error instead of raising exception
         return
           Result_Type.Err
             (Create_Error ("Try_All requires at least one operation"));
      end if;
   exception
      when E : others =>
         --  Convert any unexpected exception to error result
         return
           Result_Type.Err
             (Create_Error
                ("Unexpected error in Try_All: "
                 & Ada.Exceptions.Exception_Message (E)));
   end Try_All;

   --  function Collect_Results (Results : Result_Array) return Array_Result.Result
   --  is
   --
   --     Collected : T_Array (Results'Range);
   --  begin
   --     for I in Results'Range loop
   --        if Result_Type.Is_Err (Results (I)) then
   --           --  Return first error encountered
   --           return Array_Result.Err (Result_Type.Get_Err (Results (I)));
   --        else
   --           Collected (I) := Result_Type.Get_Ok (Results (I));
   --        end if;
   --     end loop;
   --
   --     return Array_Result.Ok (Collected);
   --  end Collect_Results;

end Abohlib.Core.Domain.Result;
