--  =============================================================================
--  Abohlib.Core.Domain.Utilities.Safe_Call - Exception to Result Wrapper
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Generic wrapper that converts exception-raising functions to Result-returning
--  functions. Essential for wrapping third-party library calls that may raise
--  exceptions, ensuring exceptions don't cross architectural boundaries.
--
--  Usage:
--    -- Instantiate for your types:
--    package String_Safe_Call is new Safe_Call_Package
--      (Ok_Type => String,
--       Err_Type => Domain_Error);
--
--    -- Wrap a function that might raise exceptions:
--    Result := String_Safe_Call.Wrap_Call (Some_External_Function'Access);
--  =============================================================================

pragma Ada_2022;

with Abohlib.Core.Domain.Result;
with Abohlib.Core.Domain.Errors;

package Abohlib.Core.Domain.Utilities.Safe_Call is

   --  Generic package for creating type-specific safe call wrappers
   generic
      type Ok_Type is private;
      type Err_Type is private;
      with package Result_Package is new Abohlib.Core.Domain.Result.Result_Package
        (Ok_Type => Ok_Type,
         Err_Type => Err_Type);
      with function Make_Error
        (Exception_Name : String;
         Exception_Message : String;
         Context : String) return Err_Type;
   package Safe_Call_Package is

      --  Wrap a parameterless function
      function Wrap_Call
        (Call : not null access function return Ok_Type;
         Error_Context : String := "") return Result_Package.Result;

      --  Wrap a function with one parameter
      generic
         type Param_Type is private;
      function Wrap_Call_With_Param
        (Call : not null access function (Param : Param_Type) return Ok_Type;
         Param : Param_Type;
         Error_Context : String := "") return Result_Package.Result;

      --  Wrap a function with two parameters
      generic
         type Param1_Type is private;
         type Param2_Type is private;
      function Wrap_Call_With_2_Params
        (Call : not null access function (P1 : Param1_Type; P2 : Param2_Type) return Ok_Type;
         Param1 : Param1_Type;
         Param2 : Param2_Type;
         Error_Context : String := "") return Result_Package.Result;

      --  Wrap a function with three parameters
      generic
         type Param1_Type is private;
         type Param2_Type is private;
         type Param3_Type is private;
      function Wrap_Call_With_3_Params
        (Call : not null access function 
           (P1 : Param1_Type; P2 : Param2_Type; P3 : Param3_Type) return Ok_Type;
         Param1 : Param1_Type;
         Param2 : Param2_Type;
         Param3 : Param3_Type;
         Error_Context : String := "") return Result_Package.Result;

      --  Wrap a procedure (requires a success value to return)
      generic
         with procedure Proc;
      function Wrap_Procedure
        (Success_Value : Ok_Type;
         Error_Context : String := "") return Result_Package.Result;

      --  Wrap a procedure with one parameter
      generic
         type Param_Type is private;
         with procedure Proc (Param : Param_Type);
      function Wrap_Procedure_With_Param
        (Param : Param_Type;
         Success_Value : Ok_Type;
         Error_Context : String := "") return Result_Package.Result;

   end Safe_Call_Package;

   --  ==========================================================================
   --  Convenience package for common error types
   --  ==========================================================================

   --  Safe call wrapper using Domain_Error
   generic
      type Ok_Type is private;
   package Domain_Error_Safe_Call is

      package Domain_Error_Result is new Abohlib.Core.Domain.Result.Result_Package
        (Ok_Type => Ok_Type,
         Err_Type => Abohlib.Core.Domain.Errors.Domain_Error);

      --  Error constructor for Domain_Error
      function Make_Domain_Error_From_Exception
        (Exception_Name : String;
         Exception_Message : String;
         Context : String) return Abohlib.Core.Domain.Errors.Domain_Error;

      package Safe_Call is new Safe_Call_Package
        (Ok_Type => Ok_Type,
         Err_Type => Abohlib.Core.Domain.Errors.Domain_Error,
         Result_Package => Domain_Error_Result,
         Make_Error => Make_Domain_Error_From_Exception);

      --  Re-export commonly used functions for convenience
      function Wrap_Call
        (Call : not null access function return Ok_Type;
         Error_Context : String := "") return Domain_Error_Result.Result
         renames Safe_Call.Wrap_Call;

   end Domain_Error_Safe_Call;

end Abohlib.Core.Domain.Utilities.Safe_Call;