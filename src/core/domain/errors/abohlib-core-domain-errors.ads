--  =============================================================================
--  Abohlib.Core.Domain.Errors - Domain Error Hierarchy
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Purpose:
--    Provides a comprehensive error hierarchy for domain-level errors.
--    These errors represent business rule violations and domain invariants
--    that have been broken.
--
--  Design Principles:
--    - Errors are values, not exceptions
--    - Rich error context for debugging
--    - Error categories for proper handling
--    - Recovery suggestions where applicable
--
--  Usage:
--    Domain operations return Result types containing either success values
--    or error values from this hierarchy. This enables explicit error handling
--    without exceptions in the public API.
--  =============================================================================

pragma Ada_2022;

with Ada.Strings.Bounded;
with Ada.Calendar;
with System;

package Abohlib.Core.Domain.Errors is

   --  Maximum lengths for error strings
   Max_Error_Message_Length : constant := 500;
   Max_Field_Name_Length : constant := 100;
   Max_Recovery_Suggestion_Length : constant := 200;
   Max_Correlation_ID_Length : constant := 36;  --  UUID string length
   Max_Error_Context_Length : constant := 1000;

   package Error_Strings is new
     Ada.Strings.Bounded.Generic_Bounded_Length (Max_Error_Message_Length);
   package Field_Strings is new
     Ada.Strings.Bounded.Generic_Bounded_Length (Max_Field_Name_Length);
   package Recovery_Strings is new
     Ada.Strings.Bounded.Generic_Bounded_Length
       (Max_Recovery_Suggestion_Length);
   package Correlation_ID_Strings is new
     Ada.Strings.Bounded.Generic_Bounded_Length (Max_Correlation_ID_Length);
   package Context_Strings is new
     Ada.Strings.Bounded.Generic_Bounded_Length (Max_Error_Context_Length);

   use Error_Strings;
   use Field_Strings;
   use Recovery_Strings;
   use Correlation_ID_Strings;
   use Context_Strings;

   --  ==========================================================================
   --  Error Categories
   --  ==========================================================================

   type Error_Category is
     (Validation_Error,      -- Input validation failures
      Business_Rule_Error,   -- Business invariant violations
      State_Error,          -- Invalid state transitions
      Authorization_Error,   -- Permission/access violations
      Resource_Error,       -- Resource not found/unavailable
      Concurrency_Error,    -- Optimistic locking failures
      Integration_Error     -- External system errors
     );

   --  ==========================================================================
   --  Error Severity
   --  ==========================================================================

   type Error_Severity is
     (Warning,     -- Can proceed with caution
      Error,       -- Operation failed but recoverable
      Critical     -- System integrity at risk
     );

   --  ==========================================================================
   --  Correlation ID Generation (declared early for use in Default_Domain_Error)
   --  ==========================================================================

   function Generate_Correlation_ID
      return Correlation_ID_Strings.Bounded_String;

   --  ==========================================================================
   --  Base Error Type
   --  ==========================================================================

   type Domain_Error is record
      Category            : Error_Category;
      Severity            : Error_Severity;
      Message             : Error_Strings.Bounded_String;
      Occurred_At         : Ada.Calendar.Time;
      Recovery_Suggestion : Recovery_Strings.Bounded_String;
      Correlation_ID      : Correlation_ID_Strings.Bounded_String;
      Error_Context       : Context_Strings.Bounded_String;
      Source_Location     : System.Address := System.Null_Address;
   end record;

   function Default_Domain_Error return Domain_Error
   is (Category            => Validation_Error,
       Severity            => Error,
       Message             => Error_Strings.Null_Bounded_String,
       Occurred_At         => Ada.Calendar.Clock,
       Recovery_Suggestion => Recovery_Strings.Null_Bounded_String,
       Correlation_ID      => Generate_Correlation_ID,
       Error_Context       => Context_Strings.Null_Bounded_String,
       Source_Location     => System.Null_Address);

   --  ==========================================================================
   --  Specific Error Types
   --  ==========================================================================

   --  Validation errors for input data
   type Validation_Error_Kind is
     (Required_Field_Missing,
      Invalid_Format,
      Out_Of_Range,
      Invalid_Length,
      Pattern_Mismatch,
      Type_Mismatch);

   type Validation_Error_Type is record
      Base          : Domain_Error;
      Kind          : Validation_Error_Kind;
      Field_Name    : Field_Strings.Bounded_String;
      Invalid_Value : Error_Strings.Bounded_String;
   end record;

   --  Business rule violations
   type Business_Rule_Error_Kind is
     (Invariant_Violated,
      Precondition_Failed,
      Postcondition_Failed,
      Business_Logic_Failed);

   type Business_Rule_Error_Type is record
      Base      : Domain_Error;
      Kind      : Business_Rule_Error_Kind;
      Rule_Name : Field_Strings.Bounded_String;
      Context   : Error_Strings.Bounded_String;
   end record;

   --  State transition errors
   type State_Error_Type is record
      Base                 : Domain_Error;
      Current_State        : Field_Strings.Bounded_String;
      Attempted_Transition : Field_Strings.Bounded_String;
      Allowed_Transitions  : Error_Strings.Bounded_String;
   end record;

   --  Resource errors
   type Resource_Error_Kind is
     (Not_Found, Already_Exists, Unavailable, Exhausted);

   type Resource_Error_Type is record
      Base          : Domain_Error;
      Kind          : Resource_Error_Kind;
      Resource_Type : Field_Strings.Bounded_String;
      Resource_Id   : Error_Strings.Bounded_String;
   end record;

   --  ==========================================================================
   --  Error Constructors
   --  ==========================================================================

   function Make_Validation_Error
     (Kind          : Validation_Error_Kind;
      Field_Name    : String;
      Invalid_Value : String := "";
      Message       : String := "";
      Recovery      : String := "";
      Context       : String := "";
      Correlation   : String := "") return Validation_Error_Type
   with
     Pre =>
       Field_Name'Length <= Max_Field_Name_Length
       and then Invalid_Value'Length <= Max_Error_Message_Length
       and then Message'Length <= Max_Error_Message_Length
       and then Recovery'Length <= Max_Recovery_Suggestion_Length
       and then Context'Length <= Max_Error_Context_Length
       and then Correlation'Length <= Max_Correlation_ID_Length;

   function Make_Business_Rule_Error
     (Kind         : Business_Rule_Error_Kind;
      Rule_Name    : String;
      Rule_Context : String := "";
      Message      : String := "";
      Recovery     : String := "";
      Context      : String := "";
      Correlation  : String := "") return Business_Rule_Error_Type
   with
     Pre =>
       Rule_Name'Length <= Max_Field_Name_Length
       and then Rule_Context'Length <= Max_Error_Message_Length
       and then Message'Length <= Max_Error_Message_Length
       and then Recovery'Length <= Max_Recovery_Suggestion_Length
       and then Context'Length <= Max_Error_Context_Length
       and then Correlation'Length <= Max_Correlation_ID_Length;

   function Make_State_Error
     (Current_State        : String;
      Attempted_Transition : String;
      Allowed_Transitions  : String := "";
      Message              : String := "";
      Recovery             : String := "";
      Context              : String := "";
      Correlation          : String := "") return State_Error_Type
   with
     Pre =>
       Current_State'Length <= Max_Field_Name_Length
       and then Attempted_Transition'Length <= Max_Field_Name_Length
       and then Allowed_Transitions'Length <= Max_Error_Message_Length
       and then Message'Length <= Max_Error_Message_Length
       and then Recovery'Length <= Max_Recovery_Suggestion_Length
       and then Context'Length <= Max_Error_Context_Length
       and then Correlation'Length <= Max_Correlation_ID_Length;

   function Make_Resource_Error
     (Kind          : Resource_Error_Kind;
      Resource_Type : String;
      Resource_Id   : String := "";
      Message       : String := "";
      Recovery      : String := "";
      Context       : String := "";
      Correlation   : String := "") return Resource_Error_Type
   with
     Pre =>
       Resource_Type'Length <= Max_Field_Name_Length
       and then Resource_Id'Length <= Max_Error_Message_Length
       and then Message'Length <= Max_Error_Message_Length
       and then Recovery'Length <= Max_Recovery_Suggestion_Length
       and then Context'Length <= Max_Error_Context_Length
       and then Correlation'Length <= Max_Correlation_ID_Length;

   --  ==========================================================================
   --  Error Utilities
   --  ==========================================================================

   function To_String (Error : Domain_Error) return String;
   function To_String (Error : Validation_Error_Type) return String;
   function To_String (Error : Business_Rule_Error_Type) return String;
   function To_String (Error : State_Error_Type) return String;
   function To_String (Error : Resource_Error_Type) return String;

   function Category_Name (Cat : Error_Category) return String;
   function Severity_Name (Sev : Error_Severity) return String;

   --  Check if error is recoverable
   function Is_Recoverable (Error : Domain_Error) return Boolean
   is (Error.Severity /= Critical);

   --  Get a user-friendly message (without technical details)
   function User_Message (Error : Domain_Error) return String;

   --  Correlation ID utilities
   function Get_Correlation_ID (Error : Domain_Error) return String;

   --  Error context utilities
   function Add_Context
     (Error : Domain_Error; Additional_Context : String) return Domain_Error;
   function With_Correlation_ID
     (Error : Domain_Error; Correlation_ID : String) return Domain_Error;

   --  ==========================================================================
   --  Factory Pattern for Error Construction
   --  ==========================================================================

   --  Error factory for consistent error creation
   type Error_Factory is tagged private;

   function Create_Factory
     (Default_Context : String := ""; Correlation_ID : String := "")
      return Error_Factory
   with
     Pre =>
       Default_Context'Length <= Max_Error_Context_Length
       and then Correlation_ID'Length <= Max_Correlation_ID_Length;

   --  Factory methods for different error types
   function Validation_Error
     (Self          : Error_Factory;
      Kind          : Validation_Error_Kind;
      Field_Name    : String;
      Invalid_Value : String := "";
      Message       : String := "") return Validation_Error_Type
   with
     Pre =>
       Field_Name'Length <= Max_Field_Name_Length
       and then Invalid_Value'Length <= Max_Error_Message_Length
       and then Message'Length <= Max_Error_Message_Length;

   function Business_Rule_Error
     (Self         : Error_Factory;
      Kind         : Business_Rule_Error_Kind;
      Rule_Name    : String;
      Rule_Context : String := "";
      Message      : String := "") return Business_Rule_Error_Type
   with
     Pre =>
       Rule_Name'Length <= Max_Field_Name_Length
       and then Rule_Context'Length <= Max_Error_Message_Length
       and then Message'Length <= Max_Error_Message_Length;

   function State_Error
     (Self                 : Error_Factory;
      Current_State        : String;
      Attempted_Transition : String;
      Allowed_Transitions  : String := "";
      Message              : String := "") return State_Error_Type
   with
     Pre =>
       Current_State'Length <= Max_Field_Name_Length
       and then Attempted_Transition'Length <= Max_Field_Name_Length
       and then Allowed_Transitions'Length <= Max_Error_Message_Length
       and then Message'Length <= Max_Error_Message_Length;

   function Resource_Error
     (Self          : Error_Factory;
      Kind          : Resource_Error_Kind;
      Resource_Type : String;
      Resource_Id   : String := "";
      Message       : String := "") return Resource_Error_Type
   with
     Pre =>
       Resource_Type'Length <= Max_Field_Name_Length
       and then Resource_Id'Length <= Max_Error_Message_Length
       and then Message'Length <= Max_Error_Message_Length;

private

   type Error_Factory is tagged record
      Default_Context : Context_Strings.Bounded_String;
      Correlation_ID  : Correlation_ID_Strings.Bounded_String;
   end record;

end Abohlib.Core.Domain.Errors;
