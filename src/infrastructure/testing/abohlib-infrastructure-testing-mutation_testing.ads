--  =============================================================================
--  Abohlib.Infrastructure.Testing.Mutation_Testing - Mutation Testing Framework
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Purpose:
--    Mutation testing framework to evaluate test suite effectiveness by
--    introducing controlled defects (mutants) and verifying that tests
--    catch them. Helps identify weak spots in test coverage.
--
--  Features:
--    - Multiple mutation operators for Ada code
--    - Configurable mutation strategies
--    - Mutation score calculation
--    - Integration with existing test framework
--    - Result-based error handling
--  =============================================================================

pragma Ada_2022;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Abohlib.Core.Domain.Result;
with Abohlib.Core.Domain.Types.Counts; use Abohlib.Core.Domain.Types.Counts;
with Abohlib.Core.Domain.Types.Time; use Abohlib.Core.Domain.Types.Time;
with Abohlib.Core.Domain.Types.Testing; use Abohlib.Core.Domain.Types.Testing;

package Abohlib.Infrastructure.Testing.Mutation_Testing is

   --  ==========================================================================
   --  Error Types
   --  ==========================================================================

   type Mutation_Error_Kind is
     (Mutation_Application_Failed,
      Test_Execution_Failed,
      File_Operation_Failed,
      Compilation_Failed,
      Invalid_Mutation);

   type Mutation_Error is record
      Kind        : Mutation_Error_Kind;
      Message     : Unbounded_String;
      File_Path   : Unbounded_String;
      Line_Number : Natural := 0;
      Mutation_Id : Unbounded_String;
   end record;

   --  Result types
   package Mutation_Result is new
     Abohlib.Core.Domain.Result.Result_Package
       (Ok_Type  => Boolean,
        Err_Type => Mutation_Error);

   --  ==========================================================================
   --  Mutation Operators
   --  ==========================================================================

   type Mutation_Operator_Kind is
     (Conditional_Negation,      -- Change if A then to if not A then
      Arithmetic_Replacement,    -- Change + to -, * to /, etc.
      Relational_Replacement,    -- Change > to >=, = to /=, etc.
      Boolean_Replacement,       -- Change and to or, not A to A
      Constant_Replacement,      -- Change literal values
      Statement_Deletion,        -- Remove non-critical statements
      Return_Value_Mutation,     -- Alter function return values
      Boundary_Condition,        -- Change < to <=, >= to >
      Null_Replacement,          -- Replace expressions with null/default
      Type_Cast_Removal);        -- Remove type conversions

   type Mutation_Operator is record
      Kind        : Mutation_Operator_Kind;
      Name        : Unbounded_String;
      Description : Unbounded_String;
      Enabled     : Boolean := True;
   end record;

   type Mutation_Operator_Array is
     array (Positive range <>) of Mutation_Operator;

   --  Default mutation operators
   Default_Operators : constant Mutation_Operator_Array :=
     [
        (Kind        => Conditional_Negation,
         Name        => To_Unbounded_String ("COND_NEG"),
         Description => To_Unbounded_String ("Negate conditional expressions"),
         Enabled     => True),

        (Kind        => Arithmetic_Replacement,
         Name        => To_Unbounded_String ("ARITH_REPL"),
         Description => To_Unbounded_String ("Replace arithmetic operators"),
         Enabled     => True),

        (Kind        => Relational_Replacement,
         Name        => To_Unbounded_String ("REL_REPL"),
         Description => To_Unbounded_String ("Replace relational operators"),
         Enabled     => True),

        (Kind        => Boolean_Replacement,
         Name        => To_Unbounded_String ("BOOL_REPL"),
         Description => To_Unbounded_String ("Replace boolean operators"),
         Enabled     => True),

        (Kind        => Constant_Replacement,
         Name        => To_Unbounded_String ("CONST_REPL"),
         Description => To_Unbounded_String ("Replace constant values"),
         Enabled     => True)];

   --  ==========================================================================
   --  Mutation Configuration
   --  ==========================================================================

   type Mutation_Config is record
      Operators              : Mutation_Operator_Array (1 .. 10);
      Operator_Count         : Element_Count_Type := 0;
      Max_Mutations_Per_File : Max_Mutations_Type := 100;
      Timeout_Seconds        : Timeout_Ms_Type := 60_000;  -- Changed to milliseconds
      Parallel_Execution     : Boolean := True;
      Save_Mutants           : Boolean := False;
      Verbose                : Boolean := False;
   end record;

   --  ==========================================================================
   --  Mutation Target
   --  ==========================================================================

   type Mutation_Target is record
      File_Path     : Unbounded_String;
      Line_Number   : Natural;
      Column_Start  : Natural;
      Column_End    : Natural;
      Original_Text : Unbounded_String;
      Mutated_Text  : Unbounded_String;
      Operator      : Mutation_Operator_Kind;
   end record;

   type Mutation_Target_Array is array (Positive range <>) of Mutation_Target;

   --  ==========================================================================
   --  Mutation Results
   --  ==========================================================================

   type Mutation_Status is
     (Not_Tested,     -- Mutation not yet tested
      Killed,         -- Test suite detected the mutation
      Survived,       -- Test suite missed the mutation
      Equivalent,     -- Mutation is functionally equivalent
      Timeout,        -- Test execution timed out
      Compile_Error); -- Mutation caused compilation error

   type Mutant_Result is record
      Target         : Mutation_Target;
      Status         : Mutation_Status;
      Test_Output    : Unbounded_String;
      Execution_Time : Duration := 0.0;
      Error_Message  : Unbounded_String;
   end record;

   type Mutant_Result_Array is array (Positive range <>) of Mutant_Result;

   --  ==========================================================================
   --  Mutation Score
   --  ==========================================================================

   type Mutation_Score is record
      Total_Mutants         : Natural := 0;
      Killed_Mutants        : Natural := 0;
      Survived_Mutants      : Natural := 0;
      Equivalent_Mutants    : Natural := 0;
      Timeout_Mutants       : Natural := 0;
      Compile_Error_Mutants : Natural := 0;
      Score_Percentage      : Float := 0.0;
   end record;

   --  Calculate mutation score
   function Calculate_Score
     (Results : Mutant_Result_Array) return Mutation_Score;

   --  ==========================================================================
   --  Mutation Testing Engine
   --  ==========================================================================

   type Mutation_Engine is tagged private;

   --  Initialize mutation engine with configuration
   function Create_Engine (Config : Mutation_Config) return Mutation_Engine;

   --  Add file to mutation testing
   procedure Add_File (Engine : in out Mutation_Engine; File_Path : String)
   with Pre => File_Path'Length > 0;

   --  Generate mutations for all added files
   function Generate_Mutations
     (Engine : Mutation_Engine) return Mutation_Target_Array;

   --  Apply a specific mutation
   function Apply_Mutation
     (Engine : Mutation_Engine; Target : Mutation_Target)
      return Mutation_Result.Result;

   --  Revert a mutation (restore original)
   function Revert_Mutation
     (Engine : Mutation_Engine; Target : Mutation_Target)
      return Mutation_Result.Result;

   --  Run test suite against a mutant
   function Run_Tests_Against_Mutant
     (Engine       : Mutation_Engine;
      Target       : Mutation_Target;
      Test_Command : String) return Mutant_Result;

   --  Run complete mutation testing
   function Run_Mutation_Testing
     (Engine : Mutation_Engine; Test_Command : String) return Mutation_Score;

   --  ==========================================================================
   --  Ada-Specific Mutation Operators
   --  ==========================================================================

   --  Find conditional expressions for negation
   function Find_Conditional_Targets
     (File_Content : String) return Mutation_Target_Array;

   --  Find arithmetic operators for replacement
   function Find_Arithmetic_Targets
     (File_Content : String) return Mutation_Target_Array;

   --  Find relational operators for replacement
   function Find_Relational_Targets
     (File_Content : String) return Mutation_Target_Array;

   --  Find boolean operators for replacement
   function Find_Boolean_Targets
     (File_Content : String) return Mutation_Target_Array;

   --  Find constants for replacement
   function Find_Constant_Targets
     (File_Content : String) return Mutation_Target_Array;

   --  ==========================================================================
   --  Integration with Test Framework
   --  ==========================================================================

   --  Run mutation testing on source files
   function Test_Suite_Mutation_Analysis
     (Source_Files : String;
      -- Glob pattern for source files
      Test_Command : String;
      -- Command to run tests
      Config       : Mutation_Config := (others => <>)) return Mutation_Score;

   --  Generate mutation testing report
   procedure Generate_Report
     (Score       : Mutation_Score;
      Results     : Mutant_Result_Array;
      Output_File : String);

private

   type String_Array is array (Positive range <>) of Unbounded_String;

   type Mutation_Engine is tagged record
      Config     : Mutation_Config;
      Files      : String_Array (1 .. 100);
      File_Count : Natural := 0;
   end record;

   --  Internal helper functions
   function Read_File (File_Path : String) return String;
   function Write_File (File_Path : String; Content : String) return Boolean;
   function Backup_File (File_Path : String) return String;
   function Restore_File
     (File_Path : String; Backup_Path : String) return Boolean;
   function Execute_Command
     (Command : String; Timeout : Duration) return String;

end Abohlib.Infrastructure.Testing.Mutation_Testing;
