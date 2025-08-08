--  =============================================================================
--  Abohlib.Core.Domain.Services.Pipeline.Interfaces - Pipeline Interfaces
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Purpose:
--    Defines interface types for pipeline stages to support maximum flexibility
--    and different implementation strategies. These interfaces enable the
--    interface-based approach used by the pipeline project.
--
--  Design:
--    - Interface types for stages, pipelines, and builders
--    - Support for composition and chaining
--    - No implementation details in interfaces
--  =============================================================================

pragma Ada_2022;

with Abohlib.Core.Domain.Result;

package Abohlib.Core.Domain.Services.Pipeline.Interfaces is
   pragma Preelaborate;

   --  ==========================================================================
   --  Generic Stage Interface
   --  ==========================================================================
   
   generic
      type Input_Type is private;
      type Output_Type is private;
      type Error_Type is private;
   package Generic_Stage_Interface is
      
      --  Result type for stage operations
      package Stage_Result is new 
         Abohlib.Core.Domain.Result.Result_Package
           (Ok_Type  => Output_Type,
            Err_Type => Error_Type);
      
      --  Base stage interface
      type Stage_Interface is interface;
      
      --  Process a single input
      function Process
        (Stage : in out Stage_Interface;
         Input : Input_Type) return Stage_Result.Result is abstract;
      
      --  Get stage name for debugging/logging
      function Name (Stage : Stage_Interface) return String is abstract;
      
      --  Check if stage can process more items
      function Is_Ready (Stage : Stage_Interface) return Boolean is abstract;
      
      --  Reset stage to initial state
      procedure Reset (Stage : in out Stage_Interface) is abstract;
      
   end Generic_Stage_Interface;
   
   --  ==========================================================================
   --  Generic Pipeline Interface
   --  ==========================================================================
   
   generic
      type Input_Type is private;
      type Output_Type is private;
      type Error_Type is private;
   package Generic_Pipeline_Interface is
      
      --  Reuse stage interface
      package Stage_Pkg is new Generic_Stage_Interface
        (Input_Type  => Input_Type,
         Output_Type => Output_Type,
         Error_Type  => Error_Type);
      
      use Stage_Pkg;
      
      --  Result type for pipeline operations
      package Pipeline_Result is new 
         Abohlib.Core.Domain.Result.Result_Package
           (Ok_Type  => Output_Type,
            Err_Type => Error_Type);
      
      --  Pipeline interface that manages stages
      type Pipeline_Interface is interface;
      
      --  Add a stage to the pipeline
      procedure Add_Stage
        (Pipeline : in out Pipeline_Interface;
         Stage    : access Stage_Interface'Class) is abstract;
      
      --  Process input through all stages
      function Process
        (Pipeline : in out Pipeline_Interface;
         Input    : Input_Type) return Pipeline_Result.Result is abstract;
      
      --  Get number of stages
      function Stage_Count (Pipeline : Pipeline_Interface) return Natural is abstract;
      
      --  Reset all stages
      procedure Reset (Pipeline : in out Pipeline_Interface) is abstract;
      
   end Generic_Pipeline_Interface;
   
   --  ==========================================================================
   --  Builder Interface
   --  ==========================================================================
   
   generic
      type Input_Type is private;
      type Output_Type is private;
      type Error_Type is private;
   package Generic_Builder_Interface is
      
      --  Reuse pipeline interface
      package Pipeline_Pkg is new Generic_Pipeline_Interface
        (Input_Type  => Input_Type,
         Output_Type => Output_Type,
         Error_Type  => Error_Type);
      
      use Pipeline_Pkg;
      use Pipeline_Pkg.Stage_Pkg;
      
      --  Builder interface for fluent pipeline construction
      type Builder_Interface is interface;
      
      --  Add a stage (returns builder for chaining)
      function Add_Stage
        (Builder : in out Builder_Interface;
         Stage   : access Stage_Interface'Class) 
         return Builder_Interface'Class is abstract;
      
      --  Build the final pipeline
      function Build
        (Builder : in out Builder_Interface) 
         return access Pipeline_Interface'Class is abstract;
      
      --  Reset builder to start fresh
      procedure Reset (Builder : in out Builder_Interface) is abstract;
      
   end Generic_Builder_Interface;

end Abohlib.Core.Domain.Services.Pipeline.Interfaces;