--  =============================================================================
--  Abohlib.Core.Domain.Services.Pipeline.Generic_Stage - Unified Pipeline Stage
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Purpose:
--    Unified generic pipeline stage implementation that consolidates features
--    from abohlib, pipelib, and pipeline projects. Provides comprehensive
--    pipeline processing capabilities with Ada 2022 features.
--
--  Design:
--    - Supports both interface and tagged type approaches
--    - Comprehensive error handling with Result pattern
--    - Optional statistics collection
--    - Batch and parallel processing support
--    - Strong contracts for safety
--  =============================================================================

pragma Ada_2022;

with Ada.Strings.Bounded;
with Abohlib.Core.Domain.Result;
with Abohlib.Core.Domain.Services.Pipeline.Interfaces;

generic
   --  Core type parameters
   type Input_Type is private;
   type Output_Type is private;
   type State_Type is private;
   type Config_Type is private;
   
   --  Stage identification
   Stage_Name : String := "Generic Stage";
   
   --  Required processing function
   with function Process_Element
     (State  : in out State_Type; 
      Config : Config_Type; 
      Input  : Input_Type) return Output_Type;
   
   --  State initialization
   with function Initialize_State (Config : Config_Type) return State_Type;
   
   --  Validation functions
   with function Is_Valid_Input (Input : Input_Type) return Boolean;
   with function Is_Valid_Output (Output : Output_Type) return Boolean;
   with function Is_Valid_State (State : State_Type) return Boolean;
   
   --  Optional features
   Enable_Statistics : Boolean := True;
   Enable_Batch : Boolean := True;
   Enable_Parallel : Boolean := False;
   
package Abohlib.Core.Domain.Services.Pipeline.Generic_Stage is
   pragma Elaborate_Body;

   --  ==========================================================================
   --  Bounded String Types
   --  ==========================================================================
   
   Max_Error_Message_Length : constant := 500;
   Max_Stage_Name_Length : constant := 100;
   
   package Error_Message_Strings is new
      Ada.Strings.Bounded.Generic_Bounded_Length (Max_Error_Message_Length);
   package Stage_Name_Strings_Public is new
      Ada.Strings.Bounded.Generic_Bounded_Length (Max_Stage_Name_Length);

   --  ==========================================================================
   --  Error Types
   --  ==========================================================================
   
   type Stage_Error_Kind is
     (Invalid_Input,
      Invalid_State,
      Processing_Failed,
      Configuration_Error,
      Validation_Failed,
      Not_Initialized);
   
   type Stage_Error is record
      Kind       : Stage_Error_Kind;
      Message    : Error_Message_Strings.Bounded_String;
      Stage_Name : Stage_Name_Strings_Public.Bounded_String;
      Item_Index : Natural := 0;  -- For batch operations
   end record;
   
   --  ==========================================================================
   --  Result Types
   --  ==========================================================================
   
   package Output_Result is new
     Abohlib.Core.Domain.Result.Result_Package
       (Ok_Type  => Output_Type,
        Err_Type => Stage_Error);
   
   package Status_Result is new
     Abohlib.Core.Domain.Result.Result_Package
       (Ok_Type  => Boolean,
        Err_Type => Stage_Error);
   
   --  ==========================================================================
   --  Interface Support
   --  ==========================================================================
   
   --  Instantiate interface with our error type
   package Stage_Interface_Pkg is new 
      Interfaces.Generic_Stage_Interface
        (Input_Type  => Input_Type,
         Output_Type => Output_Type,
         Error_Type  => Stage_Error);
   
   use Stage_Interface_Pkg;
   
   --  ==========================================================================
   --  Pipeline Stage Type
   --  ==========================================================================
   
   type Pipeline_Stage is new Stage_Interface with private;
   
   --  Constructor
   function Create (Config : Config_Type) return Pipeline_Stage
     with Post => 
       Create'Result.Is_Initialized
       and then Create'Result.Name = Stage_Name
       and then Create'Result.Items_Processed = 0
       and then Create'Result.Errors_Count = 0;
   
   --  ==========================================================================
   --  Core Operations (Interface Implementation)
   --  ==========================================================================
   
   overriding
   function Process
     (Stage : in out Pipeline_Stage;
      Input : Input_Type) return Stage_Result.Result
     with Pre => Stage.Is_Initialized and then Is_Valid_Input (Input),
          Post => (if Stage_Result.Is_Ok (Process'Result)
                   then Stage.Items_Processed > Stage'Old.Items_Processed
                   else Stage.Errors_Count > Stage'Old.Errors_Count);
   
   overriding
   function Name (Stage : Pipeline_Stage) return String;
   
   overriding
   function Is_Ready (Stage : Pipeline_Stage) return Boolean;
   
   overriding
   procedure Reset (Stage : in out Pipeline_Stage)
     with Post => Stage.Items_Processed = 0 and Stage.Errors_Count = 0;
   
   --  ==========================================================================
   --  Extended Operations
   --  ==========================================================================
   
   --  Query operations
   function Is_Initialized (Stage : Pipeline_Stage) return Boolean
     with Inline;
   
   function Items_Processed (Stage : Pipeline_Stage) return Natural
     with Inline;
   
   function Errors_Count (Stage : Pipeline_Stage) return Natural
     with Inline;
   
   function Get_State (Stage : Pipeline_Stage) return State_Type
     with Pre => Stage.Is_Initialized;
   
   function Get_Config (Stage : Pipeline_Stage) return Config_Type
     with Pre => Stage.Is_Initialized;
   
   --  State management
   procedure Update_State (Stage : in out Pipeline_Stage; New_State : State_Type)
     with Pre => Stage.Is_Initialized and then Is_Valid_State (New_State),
          Post => Stage.Get_State = New_State;
   
   --  ==========================================================================
   --  Batch Processing (if enabled)
   --  ==========================================================================
   
   type Input_Array is array (Positive range <>) of Input_Type;
   type Output_Array is array (Positive range <>) of Output_Type;
   type Success_Array is array (Positive range <>) of Boolean;
   type Error_Array is array (Positive range <>) of Stage_Error;
   
   --  Check if parallel processing is safe and beneficial
   function Is_Parallel_Safe
     (Stage : Pipeline_Stage; 
      Input_Count : Natural) return Boolean
     with Pre => Stage.Is_Initialized;
   
   procedure Process_Batch
     (Stage   : in out Pipeline_Stage;
      Inputs  : Input_Array;
      Outputs : out Output_Array;
      Success : out Success_Array;
      Errors  : out Error_Array)
     with Pre => Enable_Batch
                 and then Stage.Is_Initialized
                 and then Inputs'Length > 0
                 and then Outputs'Length = Inputs'Length
                 and then Success'Length = Inputs'Length
                 and then Errors'Length = Inputs'Length,
          Post => Is_Valid_State (Stage.Get_State)
                  and then Stage.Items_Processed >= Stage'Old.Items_Processed
                  and then (Stage.Items_Processed + Stage.Errors_Count) = 
                           (Stage'Old.Items_Processed + Stage'Old.Errors_Count + Inputs'Length);
   
   --  Fail-fast batch processing (uses parallel blocks when safe)
   procedure Process_Batch_Fail_Fast
     (Stage      : in out Pipeline_Stage;
      Inputs     : Input_Array;
      Outputs    : out Output_Array;
      Error_Info : out Stage_Error;
      Success    : out Boolean)
     with Pre => Enable_Batch
                 and then Stage.Is_Initialized
                 and then Inputs'Length > 0
                 and then Outputs'Length = Inputs'Length,
          Post => Is_Valid_State (Stage.Get_State)
                  and then (if Success 
                           then Stage.Items_Processed = Stage'Old.Items_Processed + Inputs'Length
                           else Stage.Errors_Count > Stage'Old.Errors_Count);
   
   --  ==========================================================================
   --  Statistics (if enabled)
   --  ==========================================================================
   
   type Stage_Statistics is record
      Total_Items    : Natural := 0;
      Success_Count  : Natural := 0;
      Error_Count    : Natural := 0;
      Success_Rate   : Float := 0.0;
      Total_Time     : Duration := 0.0;
      Average_Time   : Duration := 0.0;
      Min_Time       : Duration := Duration'Last;
      Max_Time       : Duration := 0.0;
   end record;
   
   function Get_Statistics (Stage : Pipeline_Stage) return Stage_Statistics
     with Pre => Enable_Statistics and then Stage.Is_Initialized;
   
   procedure Reset_Statistics (Stage : in out Pipeline_Stage)
     with Pre => Enable_Statistics and then Stage.Is_Initialized,
          Post => Stage.Get_Statistics.Total_Items = 0;
   
   --  ==========================================================================
   --  Parallel Processing (if enabled)
   --  ==========================================================================
   
   procedure Process_Parallel
     (Stage        : in out Pipeline_Stage;
      Inputs       : Input_Array;
      Outputs      : out Output_Array;
      Success      : out Success_Array;
      Errors       : out Error_Array;
      Worker_Count : Positive := 4)
     with Pre => Enable_Parallel
                 and then Stage.Is_Initialized
                 and then Inputs'Length > 0
                 and then Outputs'Length = Inputs'Length
                 and then Success'Length = Inputs'Length
                 and then Errors'Length = Inputs'Length;

private
   
   --  Private type definition with conditional components
   type Pipeline_Stage is new Stage_Interface with record
      Name           : Stage_Name_Strings_Public.Bounded_String;
      Config         : Config_Type;
      State          : State_Type;
      Is_Initialized : Boolean := False;
      Items_Processed: Natural := 0;
      Errors_Count   : Natural := 0;
      
      --  Statistics fields (only if enabled)
      Stats_Enabled  : Boolean := Enable_Statistics;
      Total_Time     : Duration := 0.0;
      Min_Time       : Duration := Duration'Last;
      Max_Time       : Duration := 0.0;
   end record;

end Abohlib.Core.Domain.Services.Pipeline.Generic_Stage;