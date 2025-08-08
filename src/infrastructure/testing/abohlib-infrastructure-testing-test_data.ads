--  =============================================================================
--  Abohlib.Infrastructure.Testing.Test_Data - Simple Test Data Generation
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================
--  Purpose:
--    Provides simple utilities for generating test data patterns.
--    Focused on being easy to use and compile.
--  =============================================================================

pragma Ada_2022;

with Ada.Streams;
with Ada.Directories;
with Abohlib.Core.Domain.Errors;
with Abohlib.Core.Domain.Result;

package Abohlib.Infrastructure.Testing.Test_Data is

   use Ada.Streams;

   --  ==========================================================================
   --  Constants
   --  ==========================================================================
   
   --  Buffer sizes
   Default_Chunk_Size : constant := 4_096;  -- 4KB chunks for file operations
   
   --  Byte patterns
   Byte_Modulus : constant := 256;  -- For byte value calculations (0-255)
   
   --  Random number generation (Linear Congruential Generator)
   LCG_Multiplier : constant := 1_103_515_245;
   LCG_Increment  : constant := 12_345;
   LCG_Modulus    : constant := 2_147_483_648; -- 2^31

   --  ==========================================================================
   --  Error Types
   --  ==========================================================================
   
   type Test_Data_Error is record
      Base : Abohlib.Core.Domain.Errors.Domain_Error;
   end record;
   
   --  Result types
   package Void_Result is new Abohlib.Core.Domain.Result.Result_Package
     (Ok_Type => Boolean, Err_Type => Test_Data_Error);

   --  ==========================================================================
   --  Data Patterns
   --  ==========================================================================

   --  Generate buffer filled with zeros
   procedure Fill_Zero
     (Buffer : out Stream_Element_Array)
   with
     Post => (for all I in Buffer'Range => Buffer (I) = 0);

   --  Generate buffer with sequential values (0, 1, 2, ..., 255, 0, 1, ...)
   procedure Fill_Sequential
     (Buffer : out Stream_Element_Array;
      Start  : Stream_Element := 0)
   with
     Post => (for all I in Buffer'Range => 
                Buffer (I) = Stream_Element ((Natural (Start) + Natural (I - Buffer'First)) mod Byte_Modulus));

   --  Generate buffer with repeating pattern
   procedure Fill_Pattern
     (Buffer  : out Stream_Element_Array;
      Pattern : Stream_Element_Array)
   with
     Pre => Pattern'Length > 0,
     Post => (for all I in Buffer'Range => 
                Buffer (I) = Pattern (Pattern'First + 
                  Stream_Element_Offset ((Natural (I - Buffer'First) mod Pattern'Length))));

   --  Generate buffer with random data (using simple LCG)
   procedure Fill_Random
     (Buffer : out Stream_Element_Array;
      Seed   : Natural := 0)
   with
     Post => Buffer'Length = Buffer'Length'Old;

   --  ==========================================================================
   --  File Generation
   --  ==========================================================================

   --  Generate a test file with zeros
   function Generate_Zero_File
     (Path : String;
      Size : Natural) return Void_Result.Result
   with
     Pre => Path'Length > 0 and then Size > 0,
     Post => (if Void_Result.Is_Ok (Generate_Zero_File'Result) then
                Ada.Directories.Exists (Path));

   --  Generate a test file with random data
   function Generate_Random_File
     (Path : String;
      Size : Natural;
      Seed : Natural := 0) return Void_Result.Result
   with
     Pre => Path'Length > 0 and then Size > 0,
     Post => (if Void_Result.Is_Ok (Generate_Random_File'Result) then
                Ada.Directories.Exists (Path));

   --  ==========================================================================
   --  Utilities
   --  ==========================================================================

   --  Get a unique test filename
   function Unique_Test_Filename
     (Prefix    : String := "test_";
      Extension : String := ".dat") return String
   with
     Post => Unique_Test_Filename'Result'Length >= Prefix'Length + Extension'Length + 1;

   --  Delete test files matching pattern
   function Cleanup_Test_Files
     (Directory : String;
      Pattern   : String := "test_*.dat") return Void_Result.Result
   with
     Pre => Directory'Length > 0 and then Pattern'Length > 0;

end Abohlib.Infrastructure.Testing.Test_Data;