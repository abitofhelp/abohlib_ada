--  =============================================================================
--  Abohlib.Infrastructure.Testing.Test_Framework - Implementation
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

with Ada.Directories;
with Ada.Real_Time;
with Ada.Exceptions;
with Ada.Streams; use Ada.Streams;
with Ada.Streams.Stream_IO;
with Abohlib.Core.Domain.Constants.Bytes;

package body Abohlib.Infrastructure.Testing.Test_Framework is

   use Ada.Real_Time;

   --  Constants
   Line_Separator_Length : constant := 60;  -- Length of separator lines in test output
   Test_Buffer_Size : constant := 1024;      -- Buffer size for test file operations

   --  ==========================================================================
   --  Error Creation Helpers
   --  ==========================================================================

   function Create_Test_Error
     (Kind      : Test_Error_Kind;
      Message   : String;
      Details   : String := "";
      Line      : Natural := 0;
      Test_Name : String := "") return Test_Error is
   begin
      return
        Test_Error'
          (Kind        => Kind,
           Message     => To_Unbounded_String (Message),
           Details     => To_Unbounded_String (Details),
           Line_Number => Line,
           Test_Name   => To_Unbounded_String (Test_Name));
   end Create_Test_Error;

   --  ==========================================================================
   --  Assertion Functions Implementation
   --  ==========================================================================

   function Assert_True
     (Condition : Boolean; Message : String; Line : Natural := 0)
      return Void_Result.Result is
   begin
      if Condition then
         return Void_Result.Ok (True);
      else
         return
           Void_Result.Err
             (Create_Test_Error
                (Kind    => Assertion_Failed,
                 Message => Message,
                 Details => "Expected: True, Actual: False",
                 Line    => Line));
      end if;
   end Assert_True;

   function Assert_False
     (Condition : Boolean; Message : String; Line : Natural := 0)
      return Void_Result.Result is
   begin
      if not Condition then
         return Void_Result.Ok (True);
      else
         return
           Void_Result.Err
             (Create_Test_Error
                (Kind    => Assertion_Failed,
                 Message => Message,
                 Details => "Expected: False, Actual: True",
                 Line    => Line));
      end if;
   end Assert_False;

   function Assert_Equal
     (Expected : Integer;
      Actual   : Integer;
      Message  : String;
      Line     : Natural := 0) return Void_Result.Result is
   begin
      if Expected = Actual then
         return Void_Result.Ok (True);
      else
         return
           Void_Result.Err
             (Create_Test_Error
                (Kind    => Assertion_Failed,
                 Message => Message,
                 Details =>
                   "Expected: " & Expected'Image & ", Actual: " & Actual'Image,
                 Line    => Line));
      end if;
   end Assert_Equal;

   function Assert_Equal
     (Expected : String;
      Actual   : String;
      Message  : String;
      Line     : Natural := 0) return Void_Result.Result is
   begin
      if Expected = Actual then
         return Void_Result.Ok (True);
      else
         return
           Void_Result.Err
             (Create_Test_Error
                (Kind    => Assertion_Failed,
                 Message => Message,
                 Details =>
                   "Expected: '" & Expected & "', Actual: '" & Actual & "'",
                 Line    => Line));
      end if;
   end Assert_Equal;

   function Assert_Equal
     (Expected  : Duration;
      Actual    : Duration;
      Tolerance : Duration;
      Message   : String;
      Line      : Natural := 0) return Void_Result.Result is
   begin
      if abs (Expected - Actual) <= Tolerance then
         return Void_Result.Ok (True);
      else
         return
           Void_Result.Err
             (Create_Test_Error
                (Kind    => Assertion_Failed,
                 Message => Message,
                 Details =>
                   "Expected: "
                   & Expected'Image
                   & " +/- "
                   & Tolerance'Image
                   & ", Actual: "
                   & Actual'Image,
                 Line    => Line));
      end if;
   end Assert_Equal;

   function Assert_Not_Equal
     (Unexpected : Integer;
      Actual     : Integer;
      Message    : String;
      Line       : Natural := 0) return Void_Result.Result is
   begin
      if Unexpected /= Actual then
         return Void_Result.Ok (True);
      else
         return
           Void_Result.Err
             (Create_Test_Error
                (Kind    => Assertion_Failed,
                 Message => Message,
                 Details =>
                   "Unexpected: "
                   & Unexpected'Image
                   & ", Actual: "
                   & Actual'Image,
                 Line    => Line));
      end if;
   end Assert_Not_Equal;

   function Assert_Greater_Than
     (Expected : Integer;
      Actual   : Integer;
      Message  : String;
      Line     : Natural := 0) return Void_Result.Result is
   begin
      if Actual > Expected then
         return Void_Result.Ok (True);
      else
         return
           Void_Result.Err
             (Create_Test_Error
                (Kind    => Assertion_Failed,
                 Message => Message,
                 Details =>
                   "Expected > "
                   & Expected'Image
                   & ", Actual: "
                   & Actual'Image,
                 Line    => Line));
      end if;
   end Assert_Greater_Than;

   function Assert_Less_Than
     (Expected : Integer;
      Actual   : Integer;
      Message  : String;
      Line     : Natural := 0) return Void_Result.Result is
   begin
      if Actual < Expected then
         return Void_Result.Ok (True);
      else
         return
           Void_Result.Err
             (Create_Test_Error
                (Kind    => Assertion_Failed,
                 Message => Message,
                 Details =>
                   "Expected < "
                   & Expected'Image
                   & ", Actual: "
                   & Actual'Image,
                 Line    => Line));
      end if;
   end Assert_Less_Than;

   --  ==========================================================================
   --  Stream Element Array Assertions
   --  ==========================================================================

   function Assert_Arrays_Equal
     (Expected : Ada.Streams.Stream_Element_Array;
      Actual   : Ada.Streams.Stream_Element_Array;
      Message  : String;
      Line     : Natural := 0) return Void_Result.Result is
   begin
      if Expected'Length /= Actual'Length then
         return
           Void_Result.Err
             (Create_Test_Error
                (Kind    => Assertion_Failed,
                 Message => Message,
                 Details =>
                   "Array lengths differ - Expected: "
                   & Expected'Length'Image
                   & ", Actual: "
                   & Actual'Length'Image,
                 Line    => Line));
      end if;

      for I in Expected'Range loop
         if Expected (I) /= Actual (I - Expected'First + Actual'First) then
            return
              Void_Result.Err
                (Create_Test_Error
                   (Kind    => Assertion_Failed,
                    Message => Message,
                    Details => "Arrays differ at index " & I'Image,
                    Line    => Line));
         end if;
      end loop;

      return Void_Result.Ok (True);
   end Assert_Arrays_Equal;

   --  ==========================================================================
   --  Generic Result Assertions
   --  ==========================================================================

   function Generic_Assert_Result_Ok
     (Result : Result_Type.Result; Message : String; Line : Natural := 0)
      return Void_Result.Result is
   begin
      if Result.Is_Ok then
         return Void_Result.Ok (True);
      else
         return
           Void_Result.Err
             (Create_Test_Error
                (Kind    => Assertion_Failed,
                 Message => Message,
                 Details => "Expected Ok result, got Err",
                 Line    => Line));
      end if;
   end Generic_Assert_Result_Ok;

   function Generic_Assert_Result_Err
     (Result : Result_Type.Result; Message : String; Line : Natural := 0)
      return Void_Result.Result is
   begin
      if Result.Is_Err then
         return Void_Result.Ok (True);
      else
         return
           Void_Result.Err
             (Create_Test_Error
                (Kind    => Assertion_Failed,
                 Message => Message,
                 Details => "Expected Err result, got Ok",
                 Line    => Line));
      end if;
   end Generic_Assert_Result_Err;

   --  ==========================================================================
   --  Test Execution
   --  ==========================================================================

   function Run_Test
     (Test_Name : String;
      Test_Func : Test_Function_Access;
      Output    : access Test_Output_Port'Class) return Test_Result_Pkg.Result
   is
      pragma Unreferenced (Output);
      Start_Time : constant Time := Clock;
      Result     : Test_Result;
   begin
      Result.Name := To_Unbounded_String (Test_Name);
      Result.Line_Number := 0;
      Result.Correlation_ID := To_Unbounded_String ("TEST-" & Test_Name);

      begin
         declare
            Test_Outcome : constant Void_Result.Result := Test_Func.all;
         begin
            if Test_Outcome.Is_Ok then
               Result.Status := Passed;
               Result.Message := To_Unbounded_String ("Test passed");
            else
               Result.Status := Failed;
               declare
                  Error : constant Test_Error := Test_Outcome.Get_Err;
               begin
                  Result.Message := Error.Message;
                  Result.Line_Number := Error.Line_Number;
               end;
            end if;
         end;
      exception
         when E : others =>
            Result.Status := Error;
            Result.Message :=
              To_Unbounded_String
                ("Unexpected exception: "
                 & Ada.Exceptions.Exception_Message (E));
      end;

      Result.Elapsed_Time := To_Duration (Clock - Start_Time);
      return Test_Result_Pkg.Ok (Result);
   end Run_Test;

   function Run_Test_Suite
     (Suite_Name : String;
      Tests      : Test_Results_Array;
      Output     : access Test_Output_Port'Class)
      return Test_Stats_Result.Result
   is
      pragma Unreferenced (Suite_Name);
      pragma Unreferenced (Output);
      Stats : Test_Statistics;
   begin
      Stats.Total_Tests := Tests'Length;

      for Test of Tests loop
         Stats.Total_Duration := Stats.Total_Duration + Test.Elapsed_Time;

         case Test.Status is
            when Passed =>
               Stats.Passed_Tests := Stats.Passed_Tests + 1;

            when Failed =>
               Stats.Failed_Tests := Stats.Failed_Tests + 1;

            when Skipped =>
               Stats.Skipped_Tests := Stats.Skipped_Tests + 1;

            when Error =>
               Stats.Error_Tests := Stats.Error_Tests + 1;
         end case;
      end loop;

      return Test_Stats_Result.Ok (Stats);
   end Run_Test_Suite;

   --  ==========================================================================
   --  Test Reporting
   --  ==========================================================================

   procedure Print_Test_Result
     (Result : Test_Result; Output : access Test_Output_Port'Class)
   is
      Status_Str : constant String :=
        (case Result.Status is
           when Passed => "[PASS]",
           when Failed => "[FAIL]",
           when Skipped => "[SKIP]",
           when Error => "[ERR ]");
   begin
      Output.Write_Line
        (Status_Str
         & " "
         & To_String (Result.Name)
         & " ("
         & Result.Elapsed_Time'Image
         & "s)");

      if Result.Status /= Passed then
         Output.Write_Error ("  " & To_String (Result.Message));
         if Result.Line_Number > 0 then
            Output.Write_Error ("  at line " & Result.Line_Number'Image);
         end if;
      end if;
   end Print_Test_Result;

   procedure Print_Test_Statistics
     (Statistics : Test_Statistics; Output : access Test_Output_Port'Class) is
   begin
      Output.Write_Line ("Test Results:");
      Output.Write_Line ("  Total:   " & Statistics.Total_Tests'Image);
      Output.Write_Line ("  Passed:  " & Statistics.Passed_Tests'Image);
      Output.Write_Line ("  Failed:  " & Statistics.Failed_Tests'Image);
      Output.Write_Line ("  Skipped: " & Statistics.Skipped_Tests'Image);
      Output.Write_Line ("  Errors:  " & Statistics.Error_Tests'Image);
      Output.Write_Line
        ("  Time:    " & Statistics.Total_Duration'Image & "s");
   end Print_Test_Statistics;

   procedure Print_Test_Summary
     (Suite_Name : String;
      Statistics : Test_Statistics;
      Output     : access Test_Output_Port'Class) is
   begin
      Output.Write_Line (String'(1 .. Line_Separator_Length => '='));
      Output.Write_Line ("Test Suite: " & Suite_Name);
      Output.Write_Line (String'(1 .. Line_Separator_Length => '='));
      Print_Test_Statistics (Statistics, Output);

      if Statistics.Failed_Tests = 0 and Statistics.Error_Tests = 0 then
         Output.Write_Line ("Result: ALL TESTS PASSED");
      else
         Output.Write_Error ("Result: TESTS FAILED");
      end if;
   end Print_Test_Summary;

   --  ==========================================================================
   --  Performance Testing
   --  ==========================================================================

   function Benchmark_Operation
     (Name : String; Iterations : Positive; Operation : Test_Procedure_Access)
      return Benchmark_Result.Result
   is
      Benchmark  : Performance_Benchmark;
      Total_Time : Time_Span := Time_Span_Zero;
      Min_Time   : Time_Span := Time_Span_Last;
      Max_Time   : Time_Span := Time_Span_Zero;
   begin
      Benchmark.Name := To_Unbounded_String (Name);
      Benchmark.Iterations := Iterations;

      begin
         for I in 1 .. Iterations loop
            declare
               Start : constant Time := Clock;
            begin
               Operation.all;
               declare
                  Elapsed : constant Time_Span := Clock - Start;
               begin
                  Total_Time := Total_Time + Elapsed;
                  if Elapsed < Min_Time then
                     Min_Time := Elapsed;
                  end if;
                  if Elapsed > Max_Time then
                     Max_Time := Elapsed;
                  end if;
               end;
            end;
         end loop;
      exception
         when E : others =>
            return
              Benchmark_Result.Err
                (Create_Test_Error
                   (Kind      => Unexpected_Exception,
                    Message   => "Benchmark failed",
                    Details   => Ada.Exceptions.Exception_Message (E),
                    Test_Name => Name));
      end;

      Benchmark.Total_Duration := To_Duration (Total_Time);
      Benchmark.Min_Duration := To_Duration (Min_Time);
      Benchmark.Max_Duration := To_Duration (Max_Time);
      Benchmark.Avg_Duration :=
        To_Duration (Total_Time) / Duration (Iterations);

      return Benchmark_Result.Ok (Benchmark);
   end Benchmark_Operation;

   procedure Print_Benchmark_Result
     (Benchmark : Performance_Benchmark;
      Output    : access Test_Output_Port'Class) is
   begin
      Output.Write_Line ("Benchmark: " & To_String (Benchmark.Name));
      Output.Write_Line ("  Iterations: " & Benchmark.Iterations'Image);
      Output.Write_Line
        ("  Total time: " & Benchmark.Total_Duration'Image & "s");
      Output.Write_Line
        ("  Min time:   " & Benchmark.Min_Duration'Image & "s");
      Output.Write_Line
        ("  Max time:   " & Benchmark.Max_Duration'Image & "s");
      Output.Write_Line
        ("  Avg time:   " & Benchmark.Avg_Duration'Image & "s");
   end Print_Benchmark_Result;

   --  ==========================================================================
   --  Memory Testing
   --  ==========================================================================

   function Check_Memory_Leaks
     (Test_Name : String; Test_Proc : Test_Procedure_Access)
      return Void_Result.Result
   is
      pragma Unreferenced (Test_Name);
   begin
      -- Note: In a real implementation, this would use GNAT debug pools
      -- or valgrind integration. For now, just run the test.
      begin
         Test_Proc.all;
         return Void_Result.Ok (True);
      exception
         when E : others =>
            return
              Void_Result.Err
                (Create_Test_Error
                   (Kind    => Unexpected_Exception,
                    Message => "Memory test failed",
                    Details => Ada.Exceptions.Exception_Message (E)));
      end;
   end Check_Memory_Leaks;

   --  ==========================================================================
   --  Test Data Generation
   --  ==========================================================================

   function Generate_Test_File
     (File_Path : String; Size_MB : Positive; Pattern : String := "ABCDEFGH")
      return Void_Result.Result
   is
      use Ada.Streams.Stream_IO;
      File          : File_Type;
      Bytes_Written : Natural := 0;
      Target_Bytes  : constant Natural := Size_MB * Natural(Abohlib.Core.Domain.Constants.Bytes.SI_MB);
   begin
      begin
         Create (File, Out_File, File_Path);
      exception
         when others =>
            return
              Void_Result.Err
                (Create_Test_Error
                   (Kind    => Setup_Failed,
                    Message => "Failed to create test file",
                    Details => File_Path));
      end;

      begin
         while Bytes_Written < Target_Bytes loop
            String'Write (Stream (File), Pattern);
            Bytes_Written := Bytes_Written + Pattern'Length;
         end loop;
         Close (File);
      exception
         when E : others =>
            begin
               Close (File);
            exception
               when others =>
                  null;
            end;
            return
              Void_Result.Err
                (Create_Test_Error
                   (Kind    => Setup_Failed,
                    Message => "Failed to write test file",
                    Details => Ada.Exceptions.Exception_Message (E)));
      end;

      return Void_Result.Ok (True);
   end Generate_Test_File;

   function Generate_Binary_Test_File
     (File_Path : String; Size_MB : Positive) return Void_Result.Result
   is
      use Ada.Streams.Stream_IO;
      File          : File_Type;
      Buffer        : Stream_Element_Array (1 .. Test_Buffer_Size);
      Bytes_Written : Natural := 0;
      Target_Bytes  : constant Natural := Size_MB * Natural(Abohlib.Core.Domain.Constants.Bytes.SI_MB);
   begin
      -- Fill buffer with binary pattern
      for I in Buffer'Range loop
         Buffer (I) := Stream_Element (I mod 256);
      end loop;

      begin
         Create (File, Out_File, File_Path);
      exception
         when others =>
            return
              Void_Result.Err
                (Create_Test_Error
                   (Kind    => Setup_Failed,
                    Message => "Failed to create binary test file",
                    Details => File_Path));
      end;

      begin
         while Bytes_Written < Target_Bytes loop
            Stream_Element_Array'Write (Stream (File), Buffer);
            Bytes_Written := Bytes_Written + Buffer'Length;
         end loop;
         Close (File);
      exception
         when E : others =>
            begin
               Close (File);
            exception
               when others =>
                  null;
            end;
            return
              Void_Result.Err
                (Create_Test_Error
                   (Kind    => Setup_Failed,
                    Message => "Failed to write binary test file",
                    Details => Ada.Exceptions.Exception_Message (E)));
      end;

      return Void_Result.Ok (True);
   end Generate_Binary_Test_File;

   function Cleanup_Test_File (File_Path : String) return Void_Result.Result is
   begin
      if Ada.Directories.Exists (File_Path) then
         begin
            Ada.Directories.Delete_File (File_Path);
         exception
            when E : others =>
               return
                 Void_Result.Err
                   (Create_Test_Error
                      (Kind    => Cleanup_Failed,
                       Message => "Failed to delete test file",
                       Details => Ada.Exceptions.Exception_Message (E)));
         end;
      end if;
      return Void_Result.Ok (True);
   end Cleanup_Test_File;

   --  ==========================================================================
   --  Test Framework Service
   --  ==========================================================================

   function Create
     (Output : not null access Test_Output_Port'Class)
      return Test_Framework_Service is
   begin
      return (Output => Output);
   end Create;

   function Run_Test
     (Self      : Test_Framework_Service;
      Test_Name : String;
      Test_Func : Test_Function_Access) return Test_Result_Pkg.Result is
   begin
      return Run_Test (Test_Name, Test_Func, Self.Output);
   end Run_Test;

   function Run_Suite
     (Self       : Test_Framework_Service;
      Suite_Name : String;
      Tests      : Test_Results_Array) return Test_Stats_Result.Result is
   begin
      return Run_Test_Suite (Suite_Name, Tests, Self.Output);
   end Run_Suite;

end Abohlib.Infrastructure.Testing.Test_Framework;
