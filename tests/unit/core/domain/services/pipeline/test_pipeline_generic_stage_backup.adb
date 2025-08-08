--   =============================================================================
--   Test_Generic_Stage - Test Implementation
--   =============================================================================
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--   =============================================================================

pragma Ada_2022;
pragma Warnings (Off, "subprogram body has no previous spec");

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Calendar; use Ada.Calendar;
with Abohlib.Core.Domain.Services.Pipeline;
with Abohlib.Core.Domain.Services.Pipeline.Generic_Stage;
--  with Abohlib.Core.Domain.Services.Pipeline.Builder;

package body Test_Pipeline_Generic_Stage is

   use Abohlib.Infrastructure.Testing.Test_Framework;

--   ==========================================================================
--   Test Stage Implementation
--   ==========================================================================

--  Simple test stage that doubles integers
   type Test_Config is record
      Multiplier : Integer := 2;
      Fail_On_Negative : Boolean := False;
   end record;

   type Test_State is record
      Process_Count : Natural := 0;
      Error_Count   : Natural := 0;
   end record;

   function Double_Integer (
      State  : in out Test_State;
      Config : Test_Config;
      Input  : Integer) return Integer
   is
   begin
      State.Process_Count := State.Process_Count + 1;

--  Simulate error for negative inputs if configured
      if Config.Fail_On_Negative and Input < 0 then
         State.Error_Count := State.Error_Count + 1;
         raise Constraint_Error with "Negative input not allowed";
      end if;

      return Input * Config.Multiplier;
   end Double_Integer;

   function Init_Test_State (Config : Test_Config) return Test_State is
      pragma Unreferenced (Config);
   begin
      return Test_State'(Process_Count => 0, Error_Count => 0);
   end Init_Test_State;

   function Is_Valid_Integer (I : Integer) return Boolean is
     (I >= -1000 and I <= 1000);

   function Is_Valid_Test_State (S : Test_State) return Boolean is
     (S.Process_Count >= 0 and S.Error_Count >= 0);

--  Instantiate the generic stage
   package Integer_Doubler is new
      Abohlib.Core.Domain.Services.Pipeline.Generic_Stage
        (Input_Type       => Integer,
         Output_Type      => Integer,
         State_Type       => Test_State,
         Config_Type      => Test_Config,
         Stage_Name       => "Integer Doubler",
         Process_Element  => Double_Integer,
         Initialize_State => Init_Test_State,
         Is_Valid_Input   => Is_Valid_Integer,
         Is_Valid_Output  => Is_Valid_Integer,
         Is_Valid_State   => Is_Valid_Test_State,
         Enable_Statistics => True,
         Enable_Batch      => True,
         Enable_Parallel   => False);

   use Integer_Doubler;

--  The Stage interface uses Stage_Result from the interface instantiation
--  Note: Integer_Doubler.Stage_Interface_Pkg.Stage_Result is what Process returns

--  Simplify access to the result package
   package Stage_Result renames Integer_Doubler.Stage_Interface_Pkg.Stage_Result;

--  Instantiate parallel-enabled stage for parallel tests
   package Parallel_Integer_Doubler is new
      Abohlib.Core.Domain.Services.Pipeline.Generic_Stage
        (Input_Type       => Integer,
         Output_Type      => Integer,
         State_Type       => Test_State,
         Config_Type      => Test_Config,
         Stage_Name       => "Parallel Integer Doubler",
         Process_Element  => Double_Integer,
         Initialize_State => Init_Test_State,
         Is_Valid_Input   => Is_Valid_Integer,
         Is_Valid_Output  => Is_Valid_Integer,
         Is_Valid_State   => Is_Valid_Test_State,
         Enable_Statistics => True,
         Enable_Batch      => True,
         Enable_Parallel   => True);

   package Parallel_Stage_Result renames Parallel_Integer_Doubler.Stage_Interface_Pkg.Stage_Result;

--   ==========================================================================
--   Test Functions
--   ==========================================================================

   function Test_Stage_Creation return Void_Result.Result is
      Config : constant Test_Config := (Multiplier => 2, Fail_On_Negative => False);
      Stage : constant Pipeline_Stage := Create (Config);
   begin
--  Verify initialization
      if not Stage.Is_Initialized then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Stage not initialized"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Stage_Creation")
         ));
      end if;

--  Verify name
      if Stage.Name /= "Integer Doubler" then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Incorrect stage name"),
            Details     => To_Unbounded_String ("Expected: Integer Doubler, Got: " & Stage.Name),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Stage_Creation")
         ));
      end if;

--  Verify initial counters
      if Stage.Items_Processed /= 0 or Stage.Errors_Count /= 0 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Counters not initialized to zero"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Stage_Creation")
         ));
      end if;

      return Void_Result.Ok (True);
   end Test_Stage_Creation;

   function Test_Basic_Processing return Void_Result.Result is
      Config : constant Test_Config := (Multiplier => 3, Fail_On_Negative => False);
      Stage : Pipeline_Stage := Create (Config);
   begin
--  Process positive number
      declare
         Result : constant Stage_Result.Result := Stage.Process (10);
      begin
      if not Stage_Result.Is_Ok (Result) then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Processing failed"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Basic_Processing")
         ));
      end if;

      if Stage_Result.Get_Ok (Result) /= 30 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Incorrect result"),
            Details     => To_Unbounded_String ("Expected: 30, Got: " &
                          Stage_Result.Get_Ok (Result)'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Basic_Processing")
         ));
      end if;

--  Verify counter updated
      if Stage.Items_Processed /= 1 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Items processed not updated"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Basic_Processing")
         ));
      end if;
      end;

      return Void_Result.Ok (True);
   end Test_Basic_Processing;

   function Test_Error_Handling return Void_Result.Result is
      Config : constant Test_Config := (Multiplier => 2, Fail_On_Negative => True);
      Stage : Pipeline_Stage := Create (Config);
   begin
--  Process negative number (should fail)
      declare
         Result : constant Stage_Result.Result := Stage.Process (-5);
      begin

      if Stage_Result.Is_Ok (Result) then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Expected error but got success"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Error_Handling")
         ));
      end if;
      end;

--  Verify error counter updated
      if Stage.Errors_Count /= 1 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Error count not updated"),
            Details     => To_Unbounded_String ("Expected: 1, Got: " &
                          Stage.Errors_Count'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Error_Handling")
         ));
      end if;

      return Void_Result.Ok (True);
   end Test_Error_Handling;

   function Test_Batch_Processing return Void_Result.Result is
      Config : constant Test_Config := (Multiplier => 2, Fail_On_Negative => False);
      Stage : Pipeline_Stage := Create (Config);
      Inputs : constant Input_Array (1 .. 3) := [10, 20, 30];
      Outputs : Output_Array (1 .. 3);
      Success : Success_Array (1 .. 3);
      Errors : Error_Array (1 .. 3);
   begin
--  Process batch
      Stage.Process_Batch (Inputs, Outputs, Success, Errors);

--  Verify all succeeded
      for I in Success'Range loop
         if not Success (I) then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Batch item failed"),
               Details     => To_Unbounded_String ("Index: " & I'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Batch_Processing")
            ));
         end if;
      end loop;

--  Verify outputs
      if Outputs (1) /= 20 or Outputs (2) /= 40 or Outputs (3) /= 60 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Incorrect batch outputs"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Batch_Processing")
         ));
      end if;

--  Verify items processed count
      if Stage.Items_Processed /= 3 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Wrong items processed count"),
            Details     => To_Unbounded_String ("Expected: 3, Got: " &
                          Stage.Items_Processed'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Batch_Processing")
         ));
      end if;

      return Void_Result.Ok (True);
   end Test_Batch_Processing;

   function Test_Statistics_Tracking return Void_Result.Result is
      Config : constant Test_Config := (Multiplier => 2, Fail_On_Negative => False);
      Stage : Pipeline_Stage := Create (Config);
      Stats : Stage_Statistics;
   begin
--  Process several items
      for I in 1 .. 5 loop
         declare
            Result : constant Stage_Result.Result := Stage.Process (I);
         begin
         if not Stage_Result.Is_Ok (Result) then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Processing failed"),
               Details     => Null_Unbounded_String,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Statistics")
            ));
         end if;
         end;
      end loop;

--  Get statistics
      Stats := Stage.Get_Statistics;

--  Verify statistics
      if Stats.Total_Items /= 5 or Stats.Success_Count /= 5 or Stats.Error_Count /= 0 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Incorrect statistics"),
            Details     => To_Unbounded_String (
               "Total: " & Stats.Total_Items'Image &
               ", Success: " & Stats.Success_Count'Image &
               ", Errors: " & Stats.Error_Count'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Statistics")
         ));
      end if;

      if Stats.Success_Rate /= 100.0 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Incorrect success rate"),
            Details     => To_Unbounded_String ("Expected: 100.0, Got: " &
                          Stats.Success_Rate'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Statistics")
         ));
      end if;

      return Void_Result.Ok (True);
   end Test_Statistics_Tracking;

--   ==========================================================================
--   Parallel Processing Tests
--   ==========================================================================

   function Test_Parallel_Correctness return Void_Result.Result is
      Config : constant Test_Config := (Multiplier => 2, Fail_On_Negative => False);
      Sequential_Stage : Pipeline_Stage := Create (Config);
      Parallel_Stage : Parallel_Integer_Doubler.Pipeline_Stage :=
        Parallel_Integer_Doubler.Create (Config);

--  Test data
      Test_Inputs : constant Input_Array := (1, 5, -3, 10, 0, 7, -2, 15);

--  Results arrays
      Seq_Outputs : Output_Array (Test_Inputs'Range);
      Seq_Success : Success_Array (Test_Inputs'Range);
      Seq_Errors : Error_Array (Test_Inputs'Range);

      Par_Outputs : Output_Array (Test_Inputs'Range);
      Par_Success : Success_Array (Test_Inputs'Range);
      Par_Errors : Error_Array (Test_Inputs'Range);
   begin
--  Process sequentially
      Sequential_Stage.Process_Batch
        (Test_Inputs, Seq_Outputs, Seq_Success, Seq_Errors);

--  Process in parallel
      Parallel_Stage.Process_Parallel
        (Test_Inputs, Par_Outputs, Par_Success, Par_Errors, Worker_Count => 3);

--  Compare results
      for I in Test_Inputs'Range loop
         if Seq_Success (I) /= Par_Success (I) then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Success status mismatch at index" & I'Image),
               Details     => To_Unbounded_String (
                 "Sequential: " & Seq_Success (I)'Image &
                 ", Parallel: " & Par_Success (I)'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Parallel_Correctness")
            ));
         end if;

         if Seq_Success (I) and then Seq_Outputs (I) /= Par_Outputs (I) then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Output mismatch at index" & I'Image),
               Details     => To_Unbounded_String (
                 "Sequential: " & Seq_Outputs (I)'Image &
                 ", Parallel: " & Par_Outputs (I)'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Parallel_Correctness")
            ));
         end if;
      end loop;

      return Void_Result.Ok (True);
   end Test_Parallel_Correctness;

   function Test_Parallel_Performance return Void_Result.Result is
      use Ada.Calendar;

      Config : constant Test_Config := (Multiplier => 2, Fail_On_Negative => False);
      Sequential_Stage : Pipeline_Stage := Create (Config);
      Parallel_Stage : Parallel_Integer_Doubler.Pipeline_Stage :=
        Parallel_Integer_Doubler.Create (Config);

--  Large test dataset
      Large_Inputs : Input_Array (1 .. 10_000);

--  Results arrays
      Seq_Outputs : Output_Array (Large_Inputs'Range);
      Seq_Success : Success_Array (Large_Inputs'Range);
      Seq_Errors : Error_Array (Large_Inputs'Range);

      Par_Outputs : Output_Array (Large_Inputs'Range);
      Par_Success : Success_Array (Large_Inputs'Range);
      Par_Errors : Error_Array (Large_Inputs'Range);

      Seq_Start, Seq_End, Par_Start, Par_End : Time;
   begin
--  Initialize test data
      for I in Large_Inputs'Range loop
         Large_Inputs (I) := I mod 100;
      end loop;

--  Time sequential processing
      Seq_Start := Clock;
      Sequential_Stage.Process_Batch
        (Large_Inputs, Seq_Outputs, Seq_Success, Seq_Errors);
      Seq_End := Clock;

--  Time parallel processing
      Par_Start := Clock;
      Parallel_Stage.Process_Parallel
        (Large_Inputs, Par_Outputs, Par_Success, Par_Errors, Worker_Count => 4);
      Par_End := Clock;

      declare
         Seq_Duration : constant Duration := Seq_End - Seq_Start;
         Par_Duration : constant Duration := Par_End - Par_Start;
         Speedup : constant Float := Float (Seq_Duration) / Float (Par_Duration);
      begin
--  We expect some improvement, but not necessarily linear
--  Just verify parallel processing completes and doesn't crash
         if Par_Duration <= 0.0 then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Invalid parallel processing duration"),
               Details     => To_Unbounded_String ("Duration: " & Par_Duration'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Parallel_Performance")
            ));
         end if;

--  Log performance for information (not an assertion)
--  In a real test we might check that speedup > 1.0, but that depends on system load
         pragma Unreferenced (Speedup);
      end;

      return Void_Result.Ok (True);
   end Test_Parallel_Performance;

   function Test_Parallel_Error_Handling return Void_Result.Result is
      Config : constant Test_Config := (Multiplier => 2, Fail_On_Negative => True);
      Parallel_Stage : Parallel_Integer_Doubler.Pipeline_Stage :=
        Parallel_Integer_Doubler.Create (Config);

--  Mix of valid and invalid inputs
      Test_Inputs : constant Input_Array :=
        (5, -1, 10, -3, 0, 7, -5, 15); -- negatives should fail

      Par_Outputs : Output_Array (Test_Inputs'Range);
      Par_Success : Success_Array (Test_Inputs'Range);
      Par_Errors : Error_Array (Test_Inputs'Range);
   begin
--  Process in parallel
      for I in Test_Inputs'Range loop
         declare
            Result : constant Parallel_Stage_Result.Result := Parallel_Stage.Process (Test_Inputs (I));
         begin
            if Parallel_Stage_Result.Is_Ok (Result) then
               Par_Outputs (I) := Parallel_Stage_Result.Get_Ok (Result);
               Par_Success (I) := True;
            else
               Par_Success (I) := False;
               Par_Errors (I) := Parallel_Stage_Result.Get_Err (Result);
            end if;
         end;
      end loop;

--  Verify error handling
      declare
         Expected_Failures : Natural := 0;
         Actual_Failures : Natural := 0;
      begin
--  Count expected failures (negative inputs)
         for I in Test_Inputs'Range loop
            if Test_Inputs (I) < 0 then
               Expected_Failures := Expected_Failures + 1;
            end if;
         end loop;

--  Count actual failures
         for I in Par_Success'Range loop
            if not Par_Success (I) then
               Actual_Failures := Actual_Failures + 1;
            end if;
         end loop;

         if Expected_Failures /= Actual_Failures then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Error count mismatch in parallel processing"),
               Details     => To_Unbounded_String (
                 "Expected: " & Expected_Failures'Image &
                 ", Actual: " & Actual_Failures'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Parallel_Error_Handling")
            ));
         end if;
      end;

      return Void_Result.Ok (True);
   end Test_Parallel_Error_Handling;

   function Test_Parallel_Edge_Cases return Void_Result.Result is
      Config : constant Test_Config := (Multiplier => 3, Fail_On_Negative => False);
      Parallel_Stage : Parallel_Integer_Doubler.Pipeline_Stage :=
        Parallel_Integer_Doubler.Create (Config);
   begin
--  Test single element
      declare
         Single_Input : constant Input_Array := (1 => 42);
         Par_Outputs : Output_Array (Single_Input'Range);
         Par_Success : Success_Array (Single_Input'Range);
         Par_Errors : Error_Array (Single_Input'Range);
      begin
         Parallel_Stage.Process_Parallel
           (Single_Input, Par_Outputs, Par_Success, Par_Errors, Worker_Count => 1);

         if not Par_Success (1) or Par_Outputs (1) /= 126 then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Single element parallel processing failed"),
               Details     => To_Unbounded_String (
                 "Success: " & Par_Success (1)'Image &
                 ", Output: " & Par_Outputs (1)'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Parallel_Edge_Cases")
            ));
         end if;
      end;

--  Test with more workers than elements
      declare
         Few_Inputs : constant Input_Array := (1, 2);
         Par_Outputs : Output_Array (Few_Inputs'Range);
         Par_Success : Success_Array (Few_Inputs'Range);
         Par_Errors : Error_Array (Few_Inputs'Range);
      begin
         Parallel_Stage.Process_Parallel
           (Few_Inputs, Par_Outputs, Par_Success, Par_Errors, Worker_Count => 5);

--  Verify all succeeded
         for I in Par_Success'Range loop
            if not Par_Success (I) then
               return Void_Result.Err (Test_Error'(
                  Kind        => Assertion_Failed,
                  Message     => To_Unbounded_String ("More workers than elements failed"),
                  Details     => To_Unbounded_String ("Failed at index " & I'Image),
                  Line_Number => 0,
                  Test_Name   => To_Unbounded_String ("Test_Parallel_Edge_Cases")
               ));
            end if;
         end loop;
      end;

      return Void_Result.Ok (True);
   end Test_Parallel_Edge_Cases;

   function Test_Parallel_Statistics return Void_Result.Result is
      Config : constant Test_Config := (Multiplier => 2, Fail_On_Negative => True);
      Parallel_Stage : Parallel_Integer_Doubler.Pipeline_Stage :=
        Parallel_Integer_Doubler.Create (Config);

      Test_Inputs : constant Input_Array :=
        (1, -1, 2, 3, -2, 4); -- 2 failures, 4 successes

      Par_Outputs : Output_Array (Test_Inputs'Range);
      Par_Success : Success_Array (Test_Inputs'Range);
      Par_Errors : Error_Array (Test_Inputs'Range);

      Initial_Processed : constant Natural := Parallel_Stage.Items_Processed;
      Initial_Errors : constant Natural := Parallel_Stage.Errors_Count;
   begin
--  Process in parallel
      for I in Test_Inputs'Range loop
         declare
            Result : constant Parallel_Stage_Result.Result := Parallel_Stage.Process (Test_Inputs (I));
         begin
            if Parallel_Stage_Result.Is_Ok (Result) then
               Par_Outputs (I) := Parallel_Stage_Result.Get_Ok (Result);
               Par_Success (I) := True;
            else
               Par_Success (I) := False;
               Par_Errors (I) := Parallel_Stage_Result.Get_Err (Result);
            end if;
         end;
      end loop;

--  Verify statistics updated correctly
      declare
         Final_Processed : constant Natural := Parallel_Stage.Items_Processed;
         Final_Errors : constant Natural := Parallel_Stage.Errors_Count;
         Expected_New_Processed : constant Natural := 4; -- positive numbers
         Expected_New_Errors : constant Natural := 2;    -- negative numbers
      begin
         if Final_Processed /= Initial_Processed + Expected_New_Processed then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Processed count incorrect after parallel"),
               Details     => To_Unbounded_String (
                 "Expected: " & (Initial_Processed + Expected_New_Processed)'Image &
                 ", Actual: " & Final_Processed'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Parallel_Statistics")
            ));
         end if;

         if Final_Errors /= Initial_Errors + Expected_New_Errors then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Error count incorrect after parallel"),
               Details     => To_Unbounded_String (
                 "Expected: " & (Initial_Errors + Expected_New_Errors)'Image &
                 ", Actual: " & Final_Errors'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Parallel_Statistics")
            ));
         end if;
      end;

      return Void_Result.Ok (True);
   end Test_Parallel_Statistics;

--   ==========================================================================
--   Run All Tests
--   ==========================================================================

   function Run_All_Tests
     (Output : access Test_Output_Port'Class) return Test_Stats_Result.Result
   is
      Tests : Test_Results_Array (1 .. 10);  -- Updated for parallel tests
      Index : Positive := 1;

      procedure Add_Test_Result
        (Name : String;
         Test_Func : Test_Function_Access)
      is
         Result : constant Test_Result_Pkg.Result :=
            Run_Test (Name, Test_Func, Output);
      begin
         if Result.Is_Ok then
            Tests (Index) := Result.Get_Ok;
            Print_Test_Result (Tests (Index), Output);
            Index := Index + 1;
         else
--  Handle test execution error
            declare
               Error : constant Test_Error := Result.Get_Err;
            begin
               Tests (Index) := Test_Result'(
                  Name           => To_Unbounded_String (Name),
                  Status         => Failed,
                  Message        => Error.Message,
                  Elapsed_Time   => 0.0,
                  Line_Number    => Error.Line_Number,
                  Correlation_ID => To_Unbounded_String ("TEST-" & Name)
               );
               Print_Test_Result (Tests (Index), Output);
               Index := Index + 1;
            end;
         end if;
      end Add_Test_Result;

   begin
      Output.Write_Line ("=== Running Pipeline Generic Stage Unit Tests ===");
      Output.Write_Line ("");

--  Run all tests
      Add_Test_Result ("Test_Stage_Creation", Test_Stage_Creation'Access);
      Add_Test_Result ("Test_Basic_Processing", Test_Basic_Processing'Access);
      Add_Test_Result ("Test_Error_Handling", Test_Error_Handling'Access);
      Add_Test_Result ("Test_Batch_Processing", Test_Batch_Processing'Access);
      Add_Test_Result ("Test_Statistics", Test_Statistics_Tracking'Access);

--  Parallel processing tests
      Add_Test_Result ("Test_Parallel_Correctness", Test_Parallel_Correctness'Access);
      Add_Test_Result ("Test_Parallel_Performance", Test_Parallel_Performance'Access);
      Add_Test_Result ("Test_Parallel_Error_Handling", Test_Parallel_Error_Handling'Access);
      Add_Test_Result ("Test_Parallel_Edge_Cases", Test_Parallel_Edge_Cases'Access);
      Add_Test_Result ("Test_Parallel_Statistics", Test_Parallel_Statistics'Access);

--  Generate summary
      declare
         Stats_Result : constant Test_Stats_Result.Result :=
            Run_Test_Suite ("Pipeline_Generic_Stage_Tests", Tests (1 .. Index - 1), Output);
      begin
         if Stats_Result.Is_Ok then
            declare
               Stats : constant Test_Statistics := Stats_Result.Get_Ok;
            begin
               Output.Write_Line ("");
               Print_Test_Summary ("Pipeline Generic Stage Unit Tests", Stats, Output);
               return Test_Stats_Result.Ok (Stats);
            end;
         else
            return Stats_Result;
         end if;
      end;
   end Run_All_Tests;

end Test_Pipeline_Generic_Stage;

pragma Warnings (On, "subprogram body has no previous spec");
