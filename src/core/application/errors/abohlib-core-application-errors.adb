--  =============================================================================
--  Abohlib.Core.Application.Errors - Implementation
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

with Ada.Calendar;
with System;

package body Abohlib.Core.Application.Errors is

   use Ada.Calendar;

   --  ==========================================================================
   --  Error Constructors
   --  ==========================================================================

   function Make_Use_Case_Error
     (Kind             : Use_Case_Error_Kind;
      Use_Case_Name    : String;
      Failed_Step      : String := "";
      Underlying_Error : Domain_Error := Default_Domain_Error;
      Message          : String := "";
      Recovery         : String := "") return Use_Case_Error
   is
      Error_Message    : Error_Strings.Bounded_String;
      Recovery_Message : Recovery_Strings.Bounded_String;
   begin
      --  Build error message
      if Message = "" then
         case Kind is
            when Precondition_Not_Met =>
               Error_Message :=
                 Error_Strings.To_Bounded_String
                   ("Use case '" & Use_Case_Name & "' preconditions not met");

            when Step_Failed =>
               Error_Message :=
                 Error_Strings.To_Bounded_String
                   ("Use case '"
                    & Use_Case_Name
                    & "' failed at step: "
                    & Failed_Step);

            when Coordination_Failed =>
               Error_Message :=
                 Error_Strings.To_Bounded_String
                   ("Use case '"
                    & Use_Case_Name
                    & "' coordination between services failed");

            when Transaction_Failed =>
               Error_Message :=
                 Error_Strings.To_Bounded_String
                   ("Use case '"
                    & Use_Case_Name
                    & "' transaction could not be completed");

            when Compensation_Failed =>
               Error_Message :=
                 Error_Strings.To_Bounded_String
                   ("Use case '"
                    & Use_Case_Name
                    & "' compensation/rollback failed");
         end case;
      else
         Error_Message := Error_Strings.To_Bounded_String (Message);
      end if;

      --  Build recovery suggestion
      if Recovery = "" then
         case Kind is
            when Precondition_Not_Met =>
               Recovery_Message :=
                 Recovery_Strings.To_Bounded_String
                   ("Ensure all required conditions are met before executing");

            when Step_Failed =>
               Recovery_Message :=
                 Recovery_Strings.To_Bounded_String
                   ("Check logs for specific step failure details");

            when Coordination_Failed =>
               Recovery_Message :=
                 Recovery_Strings.To_Bounded_String
                   ("Verify all services are available and properly configured");

            when Transaction_Failed =>
               Recovery_Message :=
                 Recovery_Strings.To_Bounded_String
                   ("Review transaction requirements and retry if appropriate");

            when Compensation_Failed =>
               Recovery_Message :=
                 Recovery_Strings.To_Bounded_String
                   ("Manual intervention may be required to restore consistency");
         end case;
      else
         Recovery_Message := Recovery_Strings.To_Bounded_String (Recovery);
      end if;

      return
        (Base             =>
           (Category            => Business_Rule_Error,
            Severity            => Error,
            Message             => Error_Message,
            Occurred_At         => Clock,
            Recovery_Suggestion => Recovery_Message,
            Correlation_ID      => Generate_Correlation_ID,
            Error_Context       => Context_Strings.Null_Bounded_String,
            Source_Location     => System.Null_Address),
         Kind             => Kind,
         Use_Case_Name    =>
           Use_Case_Strings.To_Bounded_String (Use_Case_Name),
         Failed_Step      => Step_Strings.To_Bounded_String (Failed_Step),
         Underlying_Error => Underlying_Error);
   end Make_Use_Case_Error;

   function Make_Workflow_Error
     (Workflow_Name   : String;
      Current_Step    : String;
      Steps_Completed : Natural;
      Total_Steps     : Natural;
      Message         : String := "";
      Recovery        : String := "") return Workflow_Error
   is
      Error_Message    : Error_Strings.Bounded_String;
      Recovery_Message : Recovery_Strings.Bounded_String;
   begin
      --  Build error message
      if Message = "" then
         Error_Message :=
           Error_Strings.To_Bounded_String
             ("Workflow '"
              & Workflow_Name
              & "' failed at step '"
              & Current_Step
              & "' ("
              & Steps_Completed'Image
              & " of"
              & Total_Steps'Image
              & " completed)");
      else
         Error_Message := Error_Strings.To_Bounded_String (Message);
      end if;

      --  Build recovery suggestion
      if Recovery = "" then
         Recovery_Message :=
           Recovery_Strings.To_Bounded_String
             ("Review workflow state and consider resuming from last successful step");
      else
         Recovery_Message := Recovery_Strings.To_Bounded_String (Recovery);
      end if;

      return
        (Base            =>
           (Category            => Business_Rule_Error,
            Severity            => Error,
            Message             => Error_Message,
            Occurred_At         => Clock,
            Recovery_Suggestion => Recovery_Message,
            Correlation_ID      => Generate_Correlation_ID,
            Error_Context       => Context_Strings.Null_Bounded_String,
            Source_Location     => System.Null_Address),
         Workflow_Name   => Use_Case_Strings.To_Bounded_String (Workflow_Name),
         Current_Step    => Step_Strings.To_Bounded_String (Current_Step),
         Steps_Completed => Steps_Completed,
         Total_Steps     => Total_Steps);
   end Make_Workflow_Error;

   --  ==========================================================================
   --  Error Utilities
   --  ==========================================================================

   function To_String (Error : Use_Case_Error) return String is
      Base_String : constant String := To_String (Error.Base);
   begin
      return
        Base_String
        & " [Use Case: "
        & Use_Case_Strings.To_String (Error.Use_Case_Name)
        & (if Step_Strings.Length (Error.Failed_Step) > 0
           then ", Step: " & Step_Strings.To_String (Error.Failed_Step)
           else "")
        & "]";
   end To_String;

   function To_String (Error : Workflow_Error) return String is
      Base_String : constant String := To_String (Error.Base);
   begin
      return
        Base_String
        & " [Workflow: "
        & Use_Case_Strings.To_String (Error.Workflow_Name)
        & ", Step: "
        & Step_Strings.To_String (Error.Current_Step)
        & ", Progress:"
        & Error.Steps_Completed'Image
        & "/"
        & Error.Total_Steps'Image
        & "]";
   end To_String;

end Abohlib.Core.Application.Errors;
