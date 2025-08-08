--   =============================================================================
--   Test_Result_Properties - Property-Based Tests for Result Type
--   =============================================================================
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--
--   Purpose:
--     Property-based tests for the Result type, ensuring monadic laws and
--     invariants hold across random inputs.
--   =============================================================================

pragma Ada_2022;
pragma Warnings (Off, "subprogram body has no previous spec");

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Numerics.Discrete_Random;
with Abohlib.Core.Domain.Result;
with Abohlib.Core.Domain.Errors;
with Abohlib.Infrastructure.Testing.Test_Framework;

procedure Test_Result_Properties is

   use Abohlib.Infrastructure.Testing.Test_Framework;
   use Abohlib.Core.Domain.Errors;

   package Framework renames Abohlib.Infrastructure.Testing.Test_Framework;

--   Test types
   subtype Test_Integer is Integer range -1000 .. 1000;
   package Random_Int is new Ada.Numerics.Discrete_Random (Test_Integer);

   type Test_Error is record
      Code : Natural;
      Message : Unbounded_String;
   end record;

   package Int_Result is new Abohlib.Core.Domain.Result.Result_Package
     (Ok_Type  => Test_Integer,
      Err_Type => Test_Error);

--   Generators
   Gen : Random_Int.Generator;

   function Random_Integer return Test_Integer is
   begin
      return Random_Int.Random (Gen);
   end Random_Integer;

   function Random_Error return Test_Error is
      Code : constant Natural := Natural (abs (Random_Integer)) mod 100;
   begin
      return (Code => Code,
              Message => To_Unbounded_String ("Error" & Code'Image));
   end Random_Error;

   function Random_Result return Int_Result.Result is
      Is_Ok : constant Boolean := Random_Integer > 0;
   begin
      if Is_Ok then
         return Int_Result.Ok (Random_Integer);
      else
         return Int_Result.Err (Random_Error);
      end if;
   end Random_Result;

--   Property: Is_Ok and Is_Err are mutually exclusive
   function Property_Mutual_Exclusion return Boolean is
      R : constant Int_Result.Result := Random_Result;
   begin
      return (Int_Result.Is_Ok (R) and not Int_Result.Is_Err (R)) or
             (Int_Result.Is_Err (R) and not Int_Result.Is_Ok (R));
   end Property_Mutual_Exclusion;

--   Property: Map preserves structure (functor law)
   function Property_Map_Structure return Boolean is
      R : constant Int_Result.Result := Random_Result;

      function Double (X : Test_Integer) return Test_Integer is
         (if X > Test_Integer'Last / 2 then Test_Integer'Last else X * 2);

      function Identity (X : Test_Integer) return Test_Integer is (X);
   begin
      if Int_Result.Is_Ok (R) then
         declare
            Original : constant Test_Integer := Int_Result.Get_Ok (R);
            Mapped : constant Int_Result.Result := Int_Result.Map (R, Double'Access);
         begin
--  Mapping over Ok should produce Ok with transformed value
            return Int_Result.Is_Ok (Mapped) and then
                   Int_Result.Get_Ok (Mapped) = Double (Original);
         end;
      else
         declare
            Mapped : constant Int_Result.Result := Int_Result.Map (R, Double'Access);
         begin
--  Mapping over Err should preserve the error
            return Int_Result.Is_Err (Mapped) and then
                   Int_Result.Get_Err (Mapped).Code = Int_Result.Get_Err (R).Code;
         end;
      end if;
   end Property_Map_Structure;

--   Property: And_Then chains correctly (monad law)
   function Property_And_Then_Chaining return Boolean is
      R : constant Int_Result.Result := Random_Result;

      function Safe_Divide_By_2 (X : Test_Integer) return Int_Result.Result is
      begin
         return Int_Result.Ok (X / 2);
      end Safe_Divide_By_2;

      function Always_Error (X : Test_Integer) return Int_Result.Result is
         pragma Unreferenced (X);
      begin
         return Int_Result.Err ((Code => 42, Message => To_Unbounded_String ("Always fails")));
      end Always_Error;
   begin
      if Int_Result.Is_Ok (R) then
         declare
            Chained1 : constant Int_Result.Result := Int_Result.And_Then (R, Safe_Divide_By_2'Access);
            Chained2 : constant Int_Result.Result := Int_Result.And_Then (R, Always_Error'Access);
         begin
--  And_Then with Ok should apply the function
            return Int_Result.Is_Ok (Chained1) and
                   Int_Result.Is_Err (Chained2);
         end;
      else
         declare
            Chained : constant Int_Result.Result := Int_Result.And_Then (R, Safe_Divide_By_2'Access);
         begin
--  And_Then with Err should propagate the error
            return Int_Result.Is_Err (Chained) and then
                   Int_Result.Get_Err (Chained).Code = Int_Result.Get_Err (R).Code;
         end;
      end if;
   end Property_And_Then_Chaining;

--   Property: Or_Else provides fallback correctly
   function Property_Or_Else_Fallback return Boolean is
      R : constant Int_Result.Result := Random_Result;
      Fallback : constant Test_Integer := 999;

      function Provide_Fallback return Int_Result.Result is
      begin
         return Int_Result.Ok (Fallback);
      end Provide_Fallback;
   begin
      if Int_Result.Is_Ok (R) then
         declare
            Result : constant Int_Result.Result := Int_Result.Or_Else (R, Provide_Fallback'Access);
         begin
--  Or_Else with Ok should preserve the original value
            return Int_Result.Is_Ok (Result) and then
                   Int_Result.Get_Ok (Result) = Int_Result.Get_Ok (R);
         end;
      else
         declare
            Result : constant Int_Result.Result := Int_Result.Or_Else (R, Provide_Fallback'Access);
         begin
--  Or_Else with Err should use the fallback
            return Int_Result.Is_Ok (Result) and then
                   Int_Result.Get_Ok (Result) = Fallback;
         end;
      end if;
   end Property_Or_Else_Fallback;

--   Property: Unwrap_Or provides default correctly
   function Property_Unwrap_Or_Default return Boolean is
      R : constant Int_Result.Result := Random_Result;
      Default : constant Test_Integer := 777;
   begin
      if Int_Result.Is_Ok (R) then
         return Int_Result.Unwrap_Or (R, Default) = Int_Result.Get_Ok (R);
      else
         return Int_Result.Unwrap_Or (R, Default) = Default;
      end if;
   end Property_Unwrap_Or_Default;

--   Run all property tests
   procedure Run_Property_Tests is
      Test_Count : constant := 1000;
      All_Passed : Boolean := True;
   begin
      Random_Int.Reset (Gen);
      Framework.Test_Suite_Begin ("Result Type Property Tests");

--  Test: Mutual Exclusion Property
      declare
         Passed : Boolean := True;
      begin
         for I in 1 .. Test_Count loop
            if not Property_Mutual_Exclusion then
               Passed := False;
               exit;
            end if;
         end loop;

         if Passed then
            Framework.Test_Pass ("Result Mutual Exclusion Property");
         else
            Framework.Test_Fail ("Result Mutual Exclusion", "Is_Ok and Is_Err not exclusive");
            All_Passed := False;
         end if;
      end;

--  Test: Map Structure Property
      declare
         Passed : Boolean := True;
      begin
         for I in 1 .. Test_Count loop
            if not Property_Map_Structure then
               Passed := False;
               exit;
            end if;
         end loop;

         if Passed then
            Framework.Test_Pass ("Result Map Structure Property (Functor Law)");
         else
            Framework.Test_Fail ("Result Map Structure", "Map doesn't preserve structure");
            All_Passed := False;
         end if;
      end;

--  Test: And_Then Chaining Property
      declare
         Passed : Boolean := True;
      begin
         for I in 1 .. Test_Count loop
            if not Property_And_Then_Chaining then
               Passed := False;
               exit;
            end if;
         end loop;

         if Passed then
            Framework.Test_Pass ("Result And_Then Chaining Property (Monad Law)");
         else
            Framework.Test_Fail ("Result And_Then", "Chaining doesn't work correctly");
            All_Passed := False;
         end if;
      end;

--  Test: Or_Else Fallback Property
      declare
         Passed : Boolean := True;
      begin
         for I in 1 .. Test_Count loop
            if not Property_Or_Else_Fallback then
               Passed := False;
               exit;
            end if;
         end loop;

         if Passed then
            Framework.Test_Pass ("Result Or_Else Fallback Property");
         else
            Framework.Test_Fail ("Result Or_Else", "Fallback not applied correctly");
            All_Passed := False;
         end if;
      end;

--  Test: Unwrap_Or Default Property
      declare
         Passed : Boolean := True;
      begin
         for I in 1 .. Test_Count loop
            if not Property_Unwrap_Or_Default then
               Passed := False;
               exit;
            end if;
         end loop;

         if Passed then
            Framework.Test_Pass ("Result Unwrap_Or Default Property");
         else
            Framework.Test_Fail ("Result Unwrap_Or", "Default value not used correctly");
            All_Passed := False;
         end if;
      end;

      Framework.Test_Suite_End (All_Passed);
   end Run_Property_Tests;

begin
   Run_Property_Tests;
end Test_Result_Properties;

pragma Warnings (On, "subprogram body has no previous spec");
