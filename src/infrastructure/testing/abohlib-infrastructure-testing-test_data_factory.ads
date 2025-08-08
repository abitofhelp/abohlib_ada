--  =============================================================================
--  Abohlib.Infrastructure.Testing.Test_Data_Factory - Test Data Factory Pattern
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================
--  Purpose:
--    Provides factory pattern for creating test data with various configurations.
--    Supports fluent interface for building complex test data scenarios.
--  =============================================================================

pragma Ada_2022;

with Ada.Streams; use Ada.Streams;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Abohlib.Core.Domain.Result;
with Abohlib.Infrastructure.Testing.Test_Data;

package Abohlib.Infrastructure.Testing.Test_Data_Factory is

   --  Stream element array access type
   type Stream_Element_Array_Access is access all Stream_Element_Array;

   --  ==========================================================================
   --  Constants
   --  ==========================================================================
   
   --  Predefined sizes
   Tiny_Size    : constant := 16;        -- 16 bytes
   Small_Size   : constant := 1_024;     -- 1 KB
   Medium_Size  : constant := 65_536;    -- 64 KB
   Large_Size   : constant := 1_048_576; -- 1 MB
   Huge_Size    : constant := 10_485_760; -- 10 MB
   
   --  Predefined patterns
   type Pattern_Type is 
     (All_Zeros,
      All_Ones,
      Alternating_Bits,    -- 0xAA, 0x55, 0xAA, 0x55...
      Sequential,          -- 0, 1, 2, 3, ...
      Random,
      Custom);

   --  ==========================================================================
   --  Error Types
   --  ==========================================================================
   
   subtype Factory_Error is Test_Data.Test_Data_Error;
   
   --  Result types
   package Data_Result is new Abohlib.Core.Domain.Result.Result_Package
     (Ok_Type => Stream_Element_Array_Access, Err_Type => Factory_Error);
     
   package File_Result renames Test_Data.Void_Result;

   --  ==========================================================================
   --  Factory Types
   --  ==========================================================================
   
   --  Builder for configuring test data generation
   type Test_Data_Builder is tagged private;
   
   --  File builder for configuring test file generation
   type Test_File_Builder is tagged private;

   --  ==========================================================================
   --  Data Builder Operations (Fluent Interface)
   --  ==========================================================================
   
   --  Create a new data builder
   function Create_Data_Builder return Test_Data_Builder;
   
   --  Set the size of data to generate
   function With_Size 
     (Builder : Test_Data_Builder; 
      Size    : Natural) return Test_Data_Builder
   with
     Pre => Size > 0,
     Post => With_Size'Result.Get_Size = Size;
   
   --  Use a predefined size
   function With_Predefined_Size
     (Builder : Test_Data_Builder;
      Size    : Natural) return Test_Data_Builder
   with
     Pre => Size in Tiny_Size | Small_Size | Medium_Size | Large_Size | Huge_Size,
     Post => With_Predefined_Size'Result.Get_Size = Size;
   
   --  Set the pattern to use
   function With_Pattern
     (Builder : Test_Data_Builder;
      Pattern : Pattern_Type) return Test_Data_Builder
   with
     Post => With_Pattern'Result.Get_Pattern = Pattern;
   
   --  Set a custom pattern
   function With_Custom_Pattern
     (Builder : Test_Data_Builder;
      Pattern : Stream_Element_Array) return Test_Data_Builder
   with
     Pre => Pattern'Length > 0,
     Post => With_Custom_Pattern'Result.Get_Pattern = Custom;
   
   --  Set random seed
   function With_Seed
     (Builder : Test_Data_Builder;
      Seed    : Natural) return Test_Data_Builder
   with
     Post => With_Seed'Result.Get_Seed = Seed;
   
   --  Build the test data
   function Build (Builder : Test_Data_Builder) return Data_Result.Result
   with
     Pre => Builder.Get_Size > 0;
   
   --  Query methods
   function Get_Size (Builder : Test_Data_Builder) return Natural;
   function Get_Pattern (Builder : Test_Data_Builder) return Pattern_Type;
   function Get_Seed (Builder : Test_Data_Builder) return Natural;

   --  ==========================================================================
   --  File Builder Operations (Fluent Interface)
   --  ==========================================================================
   
   --  Create a new file builder
   function Create_File_Builder return Test_File_Builder;
   
   --  Set the file path
   function With_Path
     (Builder : Test_File_Builder;
      Path    : String) return Test_File_Builder
   with
     Pre => Path'Length > 0,
     Post => With_Path'Result.Get_Path = Path;
   
   --  Use auto-generated unique filename
   function With_Unique_Name
     (Builder : Test_File_Builder;
      Prefix  : String := "test_";
      Suffix  : String := ".dat") return Test_File_Builder
   with
     Post => With_Unique_Name'Result.Get_Path'Length > 0;
   
   --  Set file size
   function With_Size
     (Builder : Test_File_Builder;
      Size    : Natural) return Test_File_Builder
   with
     Pre => Size > 0,
     Post => With_Size'Result.Get_Size = Size;
   
   --  Use predefined size
   function With_Predefined_Size
     (Builder : Test_File_Builder;
      Size    : Natural) return Test_File_Builder
   with
     Pre => Size in Tiny_Size | Small_Size | Medium_Size | Large_Size | Huge_Size,
     Post => With_Predefined_Size'Result.Get_Size = Size;
   
   --  Set content pattern
   function With_Content
     (Builder : Test_File_Builder;
      Pattern : Pattern_Type) return Test_File_Builder
   with
     Post => With_Content'Result.Get_Pattern = Pattern;
   
   --  Set random seed for content
   function With_Seed
     (Builder : Test_File_Builder;
      Seed    : Natural) return Test_File_Builder
   with
     Post => With_Seed'Result.Get_Seed = Seed;
   
   --  Build the test file
   function Build (Builder : Test_File_Builder) return File_Result.Result
   with
     Pre => Builder.Get_Path'Length > 0 and then Builder.Get_Size > 0;
   
   --  Query methods
   function Get_Path (Builder : Test_File_Builder) return String;
   function Get_Size (Builder : Test_File_Builder) return Natural;
   function Get_Pattern (Builder : Test_File_Builder) return Pattern_Type;
   function Get_Seed (Builder : Test_File_Builder) return Natural;

   --  ==========================================================================
   --  Convenience Factory Functions
   --  ==========================================================================
   
   --  Create common test data patterns
   function Create_Zero_Data (Size : Natural) return Data_Result.Result
   with Pre => Size > 0;
   
   function Create_Random_Data 
     (Size : Natural; 
      Seed : Natural := 0) return Data_Result.Result
   with Pre => Size > 0;
   
   function Create_Sequential_Data (Size : Natural) return Data_Result.Result
   with Pre => Size > 0;
   
   function Create_Pattern_Data
     (Size    : Natural;
      Pattern : Stream_Element_Array) return Data_Result.Result
   with Pre => Size > 0 and then Pattern'Length > 0;
   
   --  Create common test files
   function Create_Temp_Test_File
     (Size    : Natural;
      Pattern : Pattern_Type := Random) return File_Result.Result
   with Pre => Size > 0;
   
   function Create_Named_Test_File
     (Path    : String;
      Size    : Natural;
      Pattern : Pattern_Type := Random) return File_Result.Result
   with Pre => Path'Length > 0 and then Size > 0;

private

   --  ==========================================================================
   --  Private Types
   --  ==========================================================================
   
   type Test_Data_Builder is tagged record
      Size           : Natural := 0;
      Pattern        : Pattern_Type := Random;
      Custom_Pattern : Unbounded_String;
      Seed           : Natural := 0;
   end record;
   
   type Test_File_Builder is tagged record
      Path    : Unbounded_String;
      Size    : Natural := 0;
      Pattern : Pattern_Type := Random;
      Seed    : Natural := 0;
   end record;

end Abohlib.Infrastructure.Testing.Test_Data_Factory;