--  =============================================================================
--  Abohlib.Infrastructure.Testing.Property_Testing - Property-Based Testing
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Purpose:
--    Property-based testing framework for validating invariants and contracts
--    across randomly generated inputs. Supports automatic test case generation,
--    shrinking, and domain-specific generators.
--
--  Features:
--    - Generic property validation with configurable test counts
--    - Automatic shrinking of failing test cases to minimal examples
--    - Domain-specific generators for various types
--    - Contract validation for aggregate roots and repositories
--    - Result-based error handling (no exceptions)
--  =============================================================================

pragma Ada_2022;

with Ada.Numerics.Discrete_Random;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Abohlib.Core.Domain.Result;
with Abohlib.Core.Domain.Types.Testing; use Abohlib.Core.Domain.Types.Testing;

package Abohlib.Infrastructure.Testing.Property_Testing is

   --  ==========================================================================
   --  Error Types
   --  ==========================================================================

   type Property_Error_Kind is
     (Property_Violation,
      Generator_Failed,
      Shrinking_Failed,
      Test_Limit_Exceeded,
      Invalid_Property);

   type Property_Error is record
      Kind         : Property_Error_Kind;
      Message      : Unbounded_String;
      Test_Number  : Natural := 0;
      Shrink_Steps : Natural := 0;
      Example_Data : Unbounded_String;
   end record;

   --  Result types
   package Property_Result is new
     Abohlib.Core.Domain.Result.Result_Package
       (Ok_Type  => Boolean,
        Err_Type => Property_Error);

   --  ==========================================================================
   --  Property Test Configuration
   --  ==========================================================================

   type Test_Config is record
      Max_Tests   : Test_Count_Type := 100;    -- Number of test cases to generate
      Max_Shrinks : Shrink_Count_Type := 100;  -- Maximum shrinking attempts
      Seed        : Random_Seed_Type := 42;    -- Random seed for reproducibility
      Verbose     : Boolean := False;          -- Enable detailed output
   end record;

   Default_Config : constant Test_Config := (others => <>);

   --  ==========================================================================
   --  Generic Property Testing Framework
   --  ==========================================================================

   generic
      type Test_Data_Type is private;
      with function Generate return Test_Data_Type;
      with function Shrink (Data : Test_Data_Type) return Test_Data_Type;
      with function To_String (Data : Test_Data_Type) return String;
   package Property_Framework is

      --  Property function type - returns True if property holds
      type Property_Function is
        access function (Data : Test_Data_Type) return Boolean;

      --  Run property test with configuration
      function Check_Property
        (Property : Property_Function; Config : Test_Config := Default_Config)
         return Property_Result.Result
      with Pre => Property /= null;

      --  Check multiple properties simultaneously
      type Property_Array is array (Positive range <>) of Property_Function;

      function Check_Properties
        (Properties : Property_Array; Config : Test_Config := Default_Config)
         return Property_Result.Result
      with
        Pre =>
          Properties'Length > 0
          and then (for all P of Properties => P /= null);

   end Property_Framework;

   --  ==========================================================================
   --  Built-in Generators
   --  ==========================================================================

   --  Integer generator
   package Integer_Generator is
      function Generate_Integer (Min, Max : Integer) return Integer
      with Pre => Min <= Max;

      function Shrink_Integer (Value, Target : Integer) return Integer;
   end Integer_Generator;

   --  Natural generator
   package Natural_Generator is
      function Generate_Natural (Max : Natural := Natural'Last) return Natural;
      function Shrink_Natural (Value : Natural) return Natural;
   end Natural_Generator;

   --  String generator
   package String_Generator is
      function Generate_String (Min_Length, Max_Length : Natural) return String
      with Pre => Min_Length <= Max_Length;

      function Shrink_String (Value : String) return String;
   end String_Generator;

   --  Boolean generator
   package Boolean_Generator is
      function Generate_Boolean return Boolean;
      function Shrink_Boolean (Value : Boolean) return Boolean;
   end Boolean_Generator;

   --  ==========================================================================
   --  Domain-Specific Property Generators
   --  ==========================================================================

   --  ULID property testing
   package ULID_Properties is

      --  Generate valid ULID string
      function Generate_Valid_ULID_String return String;

      --  Generate invalid ULID string for negative testing
      function Generate_Invalid_ULID_String return String;

      --  Property: ULID ordering is maintained over time
      function ULID_Ordering_Property (First_ULID : String) return Boolean;

      --  Property: ULID validation is consistent
      function ULID_Validation_Property (ULID_String : String) return Boolean;

   end ULID_Properties;

   --  File Path property testing
   package File_Path_Properties is

      --  Generate valid file path
      function Generate_Valid_Path return String;

      --  Generate invalid file path for security testing
      function Generate_Invalid_Path return String;

      --  Property: Path normalization is idempotent
      function Path_Normalization_Property (Path : String) return Boolean;

      --  Property: Path security validation works
      function Path_Security_Property (Path : String) return Boolean;

   end File_Path_Properties;

   --  Aggregate property testing (placeholder - to be implemented)
   --  generic
   --     type Aggregate_Type is tagged private;
   --     type Id_Type is private;
   --     with function Generate_Aggregate return Aggregate_Type;
   --     with function Get_Version (A : Aggregate_Type) return Natural;
   --     with function Is_Valid (A : Aggregate_Type) return Boolean;
   --  package Aggregate_Properties is
   --
   --     --  Property: Aggregate invariants are maintained
   --     function Invariant_Property (Aggregate : Aggregate_Type) return Boolean;
   --
   --     --  Property: Version monotonicity
   --     function Version_Monotonic_Property (Aggregate : Aggregate_Type) return Boolean;
   --
   --     --  Property: Valid aggregates remain valid after operations
   --     function Validity_Preserved_Property (Aggregate : Aggregate_Type) return Boolean;
   --
   --  end Aggregate_Properties;

   --  ==========================================================================
   --  Contract Property Testing
   --  ==========================================================================

   --  Repository contract property testing (placeholder - to be implemented)
   --  generic
   --     type Repository_Type is limited interface;
   --     type Entity_Type is tagged private;
   --     type Id_Type is private;
   --     with function Generate_Entity return Entity_Type;
   --     with function Generate_Id return Id_Type;
   --     with function Get_Id (E : Entity_Type) return Id_Type;
   --  package Repository_Contract_Properties is
   --
   --     --  Property: Save then Load returns same entity
   --     function Save_Load_Roundtrip_Property
   --       (Repo : Repository_Type'Class) return Boolean;
   --
   --     --  Property: Delete removes entity
   --     function Delete_Removes_Property
   --       (Repo : Repository_Type'Class) return Boolean;
   --
   --     --  Property: Exists is consistent with operations
   --     function Exists_Consistency_Property
   --       (Repo : Repository_Type'Class) return Boolean;
   --
   --  end Repository_Contract_Properties;

   --  ==========================================================================
   --  Common Properties
   --  ==========================================================================

   --  Mathematical properties that can be tested

   generic
      type T is private;
      with function Op (Left, Right : T) return T;
      with function "=" (Left, Right : T) return Boolean is <>;
   function Commutativity_Property (A, B : T) return Boolean;

   generic
      type T is private;
      with function Op (Left, Right : T) return T;
      with function "=" (Left, Right : T) return Boolean is <>;
   function Associativity_Property (A, B, C : T) return Boolean;

   generic
      type T is private;
      with function Op (Left, Right : T) return T;
      with function Identity return T;
      with function "=" (Left, Right : T) return Boolean is <>;
   function Identity_Property (A : T) return Boolean;

   generic
      type T is private;
      with function F (Input : T) return T;
      with function "=" (Left, Right : T) return Boolean is <>;
   function Idempotency_Property (A : T) return Boolean;

private

   --  Internal random number generation
   subtype Random_Seed is Natural range 1 .. Natural'Last;

   package Random_Natural is new Ada.Numerics.Discrete_Random (Natural);
   package Random_Integer is new Ada.Numerics.Discrete_Random (Integer);

   type Random_State is record
      Natural_Gen : Random_Natural.Generator;
      Integer_Gen : Random_Integer.Generator;
      Initialized : Boolean := False;
   end record;

   protected Random_Manager is
      procedure Initialize (Seed : Natural);
      procedure Get_Natural
        (Max : Natural := Natural'Last; Result : out Natural);
      procedure Get_Integer (Min, Max : Integer; Result : out Integer);
      procedure Get_Boolean (Result : out Boolean);
   private
      State : Random_State;
   end Random_Manager;

end Abohlib.Infrastructure.Testing.Property_Testing;
