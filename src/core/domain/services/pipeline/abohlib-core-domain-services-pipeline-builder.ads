--  =============================================================================
--  Abohlib.Core.Domain.Services.Pipeline.Builder - Pipeline Builder Pattern
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Purpose:
--    Provides a fluent builder pattern for composing pipelines from stages.
--    This supports the builder-based approach used by the pipeline project.
--
--  Design:
--    - Fluent interface for chaining operations
--    - Type-safe stage composition
--    - Supports both interface and concrete stages
--  =============================================================================

pragma Ada_2022;

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Abohlib.Core.Domain.Services.Pipeline.Interfaces;

generic
   type Input_Type is private;
   type Output_Type is private;
   type Error_Type is private;
   
package Abohlib.Core.Domain.Services.Pipeline.Builder is
   pragma Elaborate_Body;

   --  Use interface package (single instantiation)
   package Pipeline_Interfaces is new Interfaces.Generic_Pipeline_Interface
     (Input_Type  => Input_Type,
      Output_Type => Output_Type,
      Error_Type  => Error_Type);
   
   subtype Stage_Interface is Pipeline_Interfaces.Stage_Pkg.Stage_Interface;
   subtype Pipeline_Interface is Pipeline_Interfaces.Pipeline_Interface;
   
   package Stage_Result renames Pipeline_Interfaces.Stage_Pkg.Stage_Result;
   package Pipeline_Result renames Pipeline_Interfaces.Pipeline_Result;
   
   --  ==========================================================================
   --  Pipeline Implementation
   --  ==========================================================================
   
   type Pipeline is new Pipeline_Interface with private;
   type Pipeline_Access is access all Pipeline;
   
   overriding
   procedure Add_Stage
     (P     : in out Pipeline;
      Stage : access Stage_Interface'Class);
   
   overriding
   function Process
     (P     : in out Pipeline;
      Input : Input_Type) return Pipeline_Result.Result;
   
   overriding
   function Stage_Count (P : Pipeline) return Natural;
   
   overriding
   procedure Reset (P : in out Pipeline);
   
   --  Additional operations
   function Name (P : Pipeline) return String;
   procedure Set_Name (P : in out Pipeline; Name : String);
   
   --  ==========================================================================
   --  Builder Type
   --  ==========================================================================
   
   --  Error handler type
   type Error_Handler_Access is access procedure (E : Error_Type);
   
   --  Builder type (not using interface to avoid multiple instantiation issues)
   type Pipeline_Builder is tagged private;
   type Pipeline_Builder_Access is access all Pipeline_Builder;
   
   --  Create a new builder
   function Create_Builder (Name : String := "Pipeline") return Pipeline_Builder;
   
   function Add_Stage
     (Builder : in out Pipeline_Builder;
      Stage   : access Stage_Interface'Class) 
      return Pipeline_Builder'Class;
   
   function Build
     (Builder : in out Pipeline_Builder) 
      return access Pipeline_Interface'Class;
   
   procedure Reset (Builder : in out Pipeline_Builder);
   
   --  Additional builder operations
   function With_Name 
     (Builder : in out Pipeline_Builder; 
      Name : String) return Pipeline_Builder'Class;
   
   function With_Error_Handler
     (Builder : in out Pipeline_Builder;
      Handler : Error_Handler_Access) 
      return Pipeline_Builder'Class;

private
   
   --  Stage reference type
   type Stage_Reference is access all Stage_Interface'Class;
   
   --  Vector of stages
   package Stage_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Stage_Reference);
   
   --  Pipeline implementation
   type Pipeline is new Pipeline_Interface with record
      Name   : Unbounded_String;
      Stages : Stage_Vectors.Vector;
   end record;
   
   --  Builder implementation
   type Pipeline_Builder is tagged record
      Name          : Unbounded_String;
      Stages        : Stage_Vectors.Vector;
      Error_Handler : Error_Handler_Access;
      Built         : Boolean := False;
   end record;

end Abohlib.Core.Domain.Services.Pipeline.Builder;