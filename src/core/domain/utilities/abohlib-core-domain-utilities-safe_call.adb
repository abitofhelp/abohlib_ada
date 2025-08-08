--  =============================================================================
--  Abohlib.Core.Domain.Utilities.Safe_Call - Implementation
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

with Ada.Exceptions;

package body Abohlib.Core.Domain.Utilities.Safe_Call is

   package body Safe_Call_Package is

      function Wrap_Call
        (Call : not null access function return Ok_Type;
         Error_Context : String := "") return Result_Package.Result
      is
      begin
         return Result_Package.Ok (Call.all);
      exception
         when E : others =>
            return Result_Package.Err
              (Make_Error
                 (Exception_Name => Ada.Exceptions.Exception_Name (E),
                  Exception_Message => Ada.Exceptions.Exception_Message (E),
                  Context => Error_Context));
      end Wrap_Call;

      function Wrap_Call_With_Param
        (Call : not null access function (Param : Param_Type) return Ok_Type;
         Param : Param_Type;
         Error_Context : String := "") return Result_Package.Result
      is
      begin
         return Result_Package.Ok (Call (Param));
      exception
         when E : others =>
            return Result_Package.Err
              (Make_Error
                 (Exception_Name => Ada.Exceptions.Exception_Name (E),
                  Exception_Message => Ada.Exceptions.Exception_Message (E),
                  Context => Error_Context));
      end Wrap_Call_With_Param;

      function Wrap_Call_With_2_Params
        (Call : not null access function (P1 : Param1_Type; P2 : Param2_Type) return Ok_Type;
         Param1 : Param1_Type;
         Param2 : Param2_Type;
         Error_Context : String := "") return Result_Package.Result
      is
      begin
         return Result_Package.Ok (Call (Param1, Param2));
      exception
         when E : others =>
            return Result_Package.Err
              (Make_Error
                 (Exception_Name => Ada.Exceptions.Exception_Name (E),
                  Exception_Message => Ada.Exceptions.Exception_Message (E),
                  Context => Error_Context));
      end Wrap_Call_With_2_Params;

      function Wrap_Call_With_3_Params
        (Call : not null access function 
           (P1 : Param1_Type; P2 : Param2_Type; P3 : Param3_Type) return Ok_Type;
         Param1 : Param1_Type;
         Param2 : Param2_Type;
         Param3 : Param3_Type;
         Error_Context : String := "") return Result_Package.Result
      is
      begin
         return Result_Package.Ok (Call (Param1, Param2, Param3));
      exception
         when E : others =>
            return Result_Package.Err
              (Make_Error
                 (Exception_Name => Ada.Exceptions.Exception_Name (E),
                  Exception_Message => Ada.Exceptions.Exception_Message (E),
                  Context => Error_Context));
      end Wrap_Call_With_3_Params;

      function Wrap_Procedure
        (Success_Value : Ok_Type;
         Error_Context : String := "") return Result_Package.Result
      is
      begin
         Proc;
         return Result_Package.Ok (Success_Value);
      exception
         when E : others =>
            return Result_Package.Err
              (Make_Error
                 (Exception_Name => Ada.Exceptions.Exception_Name (E),
                  Exception_Message => Ada.Exceptions.Exception_Message (E),
                  Context => Error_Context));
      end Wrap_Procedure;

      function Wrap_Procedure_With_Param
        (Param : Param_Type;
         Success_Value : Ok_Type;
         Error_Context : String := "") return Result_Package.Result
      is
      begin
         Proc (Param);
         return Result_Package.Ok (Success_Value);
      exception
         when E : others =>
            return Result_Package.Err
              (Make_Error
                 (Exception_Name => Ada.Exceptions.Exception_Name (E),
                  Exception_Message => Ada.Exceptions.Exception_Message (E),
                  Context => Error_Context));
      end Wrap_Procedure_With_Param;

   end Safe_Call_Package;

   package body Domain_Error_Safe_Call is

      function Make_Domain_Error_From_Exception
        (Exception_Name : String;
         Exception_Message : String;
         Context : String) return Abohlib.Core.Domain.Errors.Domain_Error
      is
         use Abohlib.Core.Domain.Errors;
         Full_Message : constant String :=
           Exception_Name & ": " & Exception_Message &
           (if Context = "" then "" else " [Context: " & Context & "]");
      begin
         return Make_Business_Rule_Error
           (Kind => Business_Logic_Failed,
            Rule_Name => "External Call",
            Rule_Context => Exception_Name,
            Message => Full_Message,
            Recovery => "Check the external library documentation for this error",
            Context => Context).Base;
      end Make_Domain_Error_From_Exception;

   end Domain_Error_Safe_Call;

end Abohlib.Core.Domain.Utilities.Safe_Call;