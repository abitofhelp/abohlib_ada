--  =============================================================================
--  Abohlib.Core.Application.Errors - Application Layer Errors
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Purpose:
--    Provides error types specific to the application layer, including
--    use case failures, workflow errors, and coordination issues.
--
--  Usage:
--    Application layer operations return Result types containing either
--    success values or errors from this hierarchy.
--  =============================================================================

pragma Ada_2022;

with Abohlib.Core.Domain.Errors;
with Ada.Strings.Bounded;

package Abohlib.Core.Application.Errors is

   use Abohlib.Core.Domain.Errors;

   --  Maximum lengths for application-specific strings
   Max_Use_Case_Name_Length : constant := 100;
   Max_Step_Name_Length : constant := 100;

   package Use_Case_Strings is new
     Ada.Strings.Bounded.Generic_Bounded_Length (Max_Use_Case_Name_Length);
   package Step_Strings is new
     Ada.Strings.Bounded.Generic_Bounded_Length (Max_Step_Name_Length);

   --  ==========================================================================
   --  Application Error Types
   --  ==========================================================================

   --  Use case execution errors
   type Use_Case_Error_Kind is
     (Precondition_Not_Met,
      Step_Failed,
      Coordination_Failed,
      Transaction_Failed,
      Compensation_Failed);

   type Use_Case_Error is record
      Base             : Domain_Error;
      Kind             : Use_Case_Error_Kind;
      Use_Case_Name    : Use_Case_Strings.Bounded_String;
      Failed_Step      : Step_Strings.Bounded_String;
      Underlying_Error : Domain_Error;  -- Underlying domain error if any
   end record;

   --  Workflow errors
   type Workflow_Error is record
      Base            : Domain_Error;
      Workflow_Name   : Use_Case_Strings.Bounded_String;
      Current_Step    : Step_Strings.Bounded_String;
      Steps_Completed : Natural;
      Total_Steps     : Natural;
   end record;

   --  ==========================================================================
   --  Error Constructors
   --  ==========================================================================

   function Make_Use_Case_Error
     (Kind             : Use_Case_Error_Kind;
      Use_Case_Name    : String;
      Failed_Step      : String := "";
      Underlying_Error : Domain_Error := Default_Domain_Error;
      Message          : String := "";
      Recovery         : String := "") return Use_Case_Error;

   function Make_Workflow_Error
     (Workflow_Name   : String;
      Current_Step    : String;
      Steps_Completed : Natural;
      Total_Steps     : Natural;
      Message         : String := "";
      Recovery        : String := "") return Workflow_Error;

   --  ==========================================================================
   --  Error Utilities
   --  ==========================================================================

   function To_String (Error : Use_Case_Error) return String;
   function To_String (Error : Workflow_Error) return String;

end Abohlib.Core.Application.Errors;
