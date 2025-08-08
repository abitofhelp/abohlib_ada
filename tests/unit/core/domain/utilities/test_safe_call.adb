--   =============================================================================
--   Test_Safe_Call - Unit tests for Safe Call Wrapper
--   =============================================================================
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--   =============================================================================

pragma Ada_2022;
pragma Warnings (Off, "subprogram body has no previous spec");

with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Abohlib.Core.Domain.Utilities.Safe_Call;
with Abohlib.Core.Domain.Errors;

package body Test_Safe_Call is

   use Abohlib.Infrastructure.Testing.Test_Framework;
   use Abohlib.Core.Domain.Errors;

--   Test exceptions
   Test_Exception : exception;

--   ==========================================================================
--   Test support functions
--   ==========================================================================

   function Test_Safe_Function return Integer is
   begin
      return 42;
   end Test_Safe_Function;

   function Test_Unsafe_Function return Integer is
   begin
      raise Test_Exception with "Test error message";
      return 0;  -- Never reached
   end Test_Unsafe_Function;

   function Test_Function_With_Param (Value : Integer) return Integer is
   begin
      if Value < 0 then
         raise Test_Exception with "Negative value not allowed";
      end if;
      return Value * 2;
   end Test_Function_With_Param;

--   Test counter for procedures
   Counter : Natural := 0;

   procedure Test_Safe_Procedure is
   begin
      Counter := Counter + 1;
   end Test_Safe_Procedure;

   pragma Warnings (Off, "procedure ""Test_Unsafe_Procedure"" is not referenced");
   procedure Test_Unsafe_Procedure is
   pragma Warnings (On, "procedure ""Test_Unsafe_Procedure"" is not referenced");
   begin
      raise Test_Exception with "Procedure error";
   end Test_Unsafe_Procedure;

--   ==========================================================================
--   Test instantiations
--   ==========================================================================

   package Integer_Domain_Error_Safe_Call is
     new Abohlib.Core.Domain.Utilities.Safe_Call.Domain_Error_Safe_Call
       (Ok_Type => Integer);

--   ==========================================================================
--   Test cases
--   ==========================================================================

   function Test_Safe_Function_Call return Void_Result.Result is
      use Integer_Domain_Error_Safe_Call;
      Result : constant Domain_Error_Result.Result :=
        Wrap_Call (Test_Safe_Function'Access);
   begin
      if not Result.Is_Ok then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Safe function should succeed"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Safe_Function_Call")
         ));
      elsif Result.Get_Ok /= 42 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Expected value 42, got " & Result.Get_Ok'Image),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Safe_Function_Call")
         ));
      else
         return Void_Result.Ok (True);
      end if;
   end Test_Safe_Function_Call;

   function Test_Unsafe_Function_Call return Void_Result.Result is
      use Integer_Domain_Error_Safe_Call;
      Result : constant Domain_Error_Result.Result :=
        Wrap_Call (Test_Unsafe_Function'Access);
   begin
      if Result.Is_Ok then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Unsafe function should fail"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Unsafe_Function_Call")
         ));
      else
         declare
            Error : constant Domain_Error := Result.Get_Err;
            Error_Str : constant String := To_String (Error);
         begin
            if Ada.Strings.Fixed.Index (Error_Str, "TEST_EXCEPTION") = 0 then
               return Void_Result.Err (Test_Error'(
                  Kind        => Assertion_Failed,
                  Message     => To_Unbounded_String ("Error should contain exception name"),
                  Details     => To_Unbounded_String (Error_Str),
                  Line_Number => 0,
                  Test_Name   => To_Unbounded_String ("Test_Unsafe_Function_Call")
               ));
            elsif Ada.Strings.Fixed.Index (Error_Str, "Test error message") = 0 then
               return Void_Result.Err (Test_Error'(
                  Kind        => Assertion_Failed,
                  Message     => To_Unbounded_String ("Error should contain exception message"),
                  Details     => To_Unbounded_String (Error_Str),
                  Line_Number => 0,
                  Test_Name   => To_Unbounded_String ("Test_Unsafe_Function_Call")
               ));
            else
               return Void_Result.Ok (True);
            end if;
         end;
      end if;
   end Test_Unsafe_Function_Call;

   function Test_Function_With_Param_Safe return Void_Result.Result is
      use Integer_Domain_Error_Safe_Call;

      function Wrap_With_Param is new Safe_Call.Wrap_Call_With_Param
        (Param_Type => Integer);
      Result : constant Domain_Error_Result.Result :=
        Wrap_With_Param (Test_Function_With_Param'Access, 21);
   begin
      if not Result.Is_Ok then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Safe call with param should succeed"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Function_With_Param_Safe")
         ));
      elsif Result.Get_Ok /= 42 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Expected value 42, got " & Result.Get_Ok'Image),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Function_With_Param_Safe")
         ));
      else
         return Void_Result.Ok (True);
      end if;
   end Test_Function_With_Param_Safe;

   function Test_Safe_Procedure_Call return Void_Result.Result is
      use Integer_Domain_Error_Safe_Call;

      function Wrap_Procedure is new Safe_Call.Wrap_Procedure
        (Proc => Test_Safe_Procedure);
      Initial_Counter : constant Natural := Counter;
      Result : constant Domain_Error_Result.Result :=
        Wrap_Procedure (Success_Value => 100);
   begin
      if not Result.Is_Ok then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Safe procedure should succeed"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Safe_Procedure_Call")
         ));
      elsif Result.Get_Ok /= 100 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Expected success value 100"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Safe_Procedure_Call")
         ));
      elsif Counter /= Initial_Counter + 1 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Procedure should have executed"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Safe_Procedure_Call")
         ));
      else
         return Void_Result.Ok (True);
      end if;
   end Test_Safe_Procedure_Call;

   function Test_Error_Context return Void_Result.Result is
      use Integer_Domain_Error_Safe_Call;

      Result : constant Domain_Error_Result.Result :=
        Wrap_Call (Test_Unsafe_Function'Access,
                   Error_Context => "Testing error context");
   begin
      if Result.Is_Ok then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Function should fail"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Error_Context")
         ));
      else
         declare
            Err : constant Domain_Error := Result.Get_Err;
         begin
            if Err.Severity /= Error then
               return Void_Result.Err (Test_Error'(
                  Kind        => Assertion_Failed,
                  Message     => To_Unbounded_String ("Error severity should be Error"),
                  Details     => Null_Unbounded_String,
                  Line_Number => 0,
                  Test_Name   => To_Unbounded_String ("Test_Error_Context")
               ));
            elsif Err.Category /= Business_Rule_Error then
               return Void_Result.Err (Test_Error'(
                  Kind        => Assertion_Failed,
                  Message     => To_Unbounded_String ("Error should be Business_Rule_Error"),
                  Details     => Null_Unbounded_String,
                  Line_Number => 0,
                  Test_Name   => To_Unbounded_String ("Test_Error_Context")
               ));
            else
               return Void_Result.Ok (True);
            end if;
         end;
      end if;
   end Test_Error_Context;

--   ==========================================================================
--   Test Runner
--   ==========================================================================

   function Run_All_Tests
     (Output : access Test_Output_Port'Class)
      return Test_Stats_Result.Result
   is
   begin
      Output.Write_Line ("=== Running Safe Call Wrapper Unit Tests ===");
      Output.Write_Line ("");

      declare
         Results : Test_Results_Array (1 .. 5);
         Index : Positive := 1;
         pragma Unreferenced (Index);

         procedure Run_Single_Test
           (I : Positive;
            Name : String;
            Func : Test_Function_Access) is
            Result : constant Test_Result_Pkg.Result := Run_Test (Name, Func, Output);
         begin
            if Result.Is_Ok then
               Results (I) := Result.Get_Ok;
               Print_Test_Result (Results (I), Output);
            else
--  Handle test execution error
               declare
                  Error : constant Test_Error := Result.Get_Err;
               begin
                  Results (I) := Test_Result'(
                     Name           => To_Unbounded_String (Name),
                     Status         => Failed,
                     Message        => Error.Message,
                     Elapsed_Time   => 0.0,
                     Line_Number    => 0,
                     Correlation_ID => To_Unbounded_String ("TEST-" & Name)
                  );
                  Print_Test_Result (Results (I), Output);
               end;
            end if;
         end Run_Single_Test;
      begin
         Run_Single_Test (1, "Test_Safe_Function_Call", Test_Safe_Function_Call'Access);
         Run_Single_Test (2, "Test_Unsafe_Function_Call", Test_Unsafe_Function_Call'Access);
         Run_Single_Test (3, "Test_Function_With_Param_Safe", Test_Function_With_Param_Safe'Access);
         Run_Single_Test (4, "Test_Safe_Procedure_Call", Test_Safe_Procedure_Call'Access);
         Run_Single_Test (5, "Test_Error_Context", Test_Error_Context'Access);

         declare
            Stats_Result : constant Test_Stats_Result.Result :=
               Run_Test_Suite ("Safe Call Wrapper", Results, Output);
         begin
            if Stats_Result.Is_Ok then
               declare
                  Stats : constant Test_Statistics := Stats_Result.Get_Ok;
               begin
                  Output.Write_Line ("");
                  Print_Test_Summary ("Safe Call Wrapper Tests", Stats, Output);
                  return Test_Stats_Result.Ok (Stats);
               end;
            else
               return Stats_Result;
            end if;
         end;
      end;
   end Run_All_Tests;

end Test_Safe_Call;

pragma Warnings (On, "subprogram body has no previous spec");
