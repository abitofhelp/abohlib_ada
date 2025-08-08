--  =============================================================================
--  Abohlib.Core.Domain.Errors - Implementation
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

with Ada.Calendar.Formatting;

package body Abohlib.Core.Domain.Errors is

   use Ada.Calendar;
   use Ada.Calendar.Formatting;

   --  Simple correlation ID counter for uniqueness
   Correlation_Counter : Natural := 0;

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
   is
      Error_Message    : Error_Strings.Bounded_String;
      Recovery_Message : Recovery_Strings.Bounded_String;
   begin
      --  Build error message
      if Message = "" then
         case Kind is
            when Required_Field_Missing =>
               Error_Message :=
                 To_Bounded_String
                   ("Required field '" & Field_Name & "' is missing");

            when Invalid_Format =>
               Error_Message :=
                 To_Bounded_String
                   ("Field '" & Field_Name & "' has invalid format");

            when Out_Of_Range =>
               Error_Message :=
                 To_Bounded_String
                   ("Field '" & Field_Name & "' is out of valid range");

            when Invalid_Length =>
               Error_Message :=
                 To_Bounded_String
                   ("Field '" & Field_Name & "' has invalid length");

            when Pattern_Mismatch =>
               Error_Message :=
                 To_Bounded_String
                   ("Field '"
                    & Field_Name
                    & "' does not match required pattern");

            when Type_Mismatch =>
               Error_Message :=
                 To_Bounded_String
                   ("Field '" & Field_Name & "' has incorrect type");
         end case;
      else
         Error_Message := To_Bounded_String (Message);
      end if;

      --  Build recovery suggestion
      if Recovery = "" then
         case Kind is
            when Required_Field_Missing =>
               Recovery_Message :=
                 To_Bounded_String ("Provide a value for the required field");

            when Invalid_Format =>
               Recovery_Message :=
                 To_Bounded_String
                   ("Check the format requirements for this field");

            when Out_Of_Range =>
               Recovery_Message :=
                 To_Bounded_String
                   ("Ensure the value is within the allowed range");

            when Invalid_Length =>
               Recovery_Message :=
                 To_Bounded_String ("Adjust the length to meet requirements");

            when Pattern_Mismatch =>
               Recovery_Message :=
                 To_Bounded_String
                   ("Ensure the value matches the required pattern");

            when Type_Mismatch =>
               Recovery_Message :=
                 To_Bounded_String ("Provide a value of the correct type");
         end case;
      else
         Recovery_Message := To_Bounded_String (Recovery);
      end if;

      return
        (Base          =>
           (Category            => Validation_Error,
            Severity            => Error,
            Message             => Error_Message,
            Occurred_At         => Clock,
            Recovery_Suggestion => Recovery_Message,
            Correlation_ID      =>
              (if Correlation /= "" then To_Bounded_String (Correlation)
               else Generate_Correlation_ID),
            Error_Context       => To_Bounded_String (Context),
            Source_Location     => System.Null_Address),
         Kind          => Kind,
         Field_Name    => To_Bounded_String (Field_Name),
         Invalid_Value => To_Bounded_String (Invalid_Value));
   end Make_Validation_Error;

   function Make_Business_Rule_Error
     (Kind         : Business_Rule_Error_Kind;
      Rule_Name    : String;
      Rule_Context : String := "";
      Message      : String := "";
      Recovery     : String := "";
      Context      : String := "";
      Correlation  : String := "") return Business_Rule_Error_Type
   is
      Error_Message    : Error_Strings.Bounded_String;
      Recovery_Message : Recovery_Strings.Bounded_String;
   begin
      --  Build error message
      if Message = "" then
         case Kind is
            when Invariant_Violated =>
               Error_Message :=
                 To_Bounded_String
                   ("Business invariant '" & Rule_Name & "' was violated");

            when Precondition_Failed =>
               Error_Message :=
                 To_Bounded_String ("Precondition '" & Rule_Name & "' failed");

            when Postcondition_Failed =>
               Error_Message :=
                 To_Bounded_String
                   ("Postcondition '" & Rule_Name & "' failed");

            when Business_Logic_Failed =>
               Error_Message :=
                 To_Bounded_String
                   ("Business rule '" & Rule_Name & "' failed");
         end case;
      else
         Error_Message := To_Bounded_String (Message);
      end if;

      --  Build recovery suggestion
      if Recovery = "" then
         Recovery_Message :=
           To_Bounded_String
             ("Review business rules and adjust input accordingly");
      else
         Recovery_Message := To_Bounded_String (Recovery);
      end if;

      return
        (Base      =>
           (Category            => Business_Rule_Error,
            Severity            => Error,
            Message             => Error_Message,
            Occurred_At         => Clock,
            Recovery_Suggestion => Recovery_Message,
            Correlation_ID      =>
              (if Correlation /= "" then To_Bounded_String (Correlation)
               else Generate_Correlation_ID),
            Error_Context       => To_Bounded_String (Context),
            Source_Location     => System.Null_Address),
         Kind      => Kind,
         Rule_Name => To_Bounded_String (Rule_Name),
         Context   => To_Bounded_String (Rule_Context));
   end Make_Business_Rule_Error;

   function Make_State_Error
     (Current_State        : String;
      Attempted_Transition : String;
      Allowed_Transitions  : String := "";
      Message              : String := "";
      Recovery             : String := "";
      Context              : String := "";
      Correlation          : String := "") return State_Error_Type
   is
      Error_Message    : Error_Strings.Bounded_String;
      Recovery_Message : Recovery_Strings.Bounded_String;
   begin
      --  Build error message
      if Message = "" then
         Error_Message :=
           To_Bounded_String
             ("Invalid state transition from '"
              & Current_State
              & "' to '"
              & Attempted_Transition
              & "'");
      else
         Error_Message := To_Bounded_String (Message);
      end if;

      --  Build recovery suggestion
      if Recovery = "" then
         if Allowed_Transitions /= "" then
            Recovery_Message :=
              To_Bounded_String ("Valid transitions: " & Allowed_Transitions);
         else
            Recovery_Message :=
              To_Bounded_String
                ("Check valid state transitions for current state");
         end if;
      else
         Recovery_Message := To_Bounded_String (Recovery);
      end if;

      return
        (Base                 =>
           (Category            => State_Error,
            Severity            => Error,
            Message             => Error_Message,
            Occurred_At         => Clock,
            Recovery_Suggestion => Recovery_Message,
            Correlation_ID      =>
              (if Correlation /= "" then To_Bounded_String (Correlation)
               else Generate_Correlation_ID),
            Error_Context       => To_Bounded_String (Context),
            Source_Location     => System.Null_Address),
         Current_State        => To_Bounded_String (Current_State),
         Attempted_Transition => To_Bounded_String (Attempted_Transition),
         Allowed_Transitions  => To_Bounded_String (Allowed_Transitions));
   end Make_State_Error;

   function Make_Resource_Error
     (Kind          : Resource_Error_Kind;
      Resource_Type : String;
      Resource_Id   : String := "";
      Message       : String := "";
      Recovery      : String := "";
      Context       : String := "";
      Correlation   : String := "") return Resource_Error_Type
   is
      Error_Message    : Error_Strings.Bounded_String;
      Recovery_Message : Recovery_Strings.Bounded_String;
   begin
      --  Build error message
      if Message = "" then
         case Kind is
            when Not_Found =>
               Error_Message :=
                 To_Bounded_String
                   (Resource_Type
                    & " not found"
                    & (if Resource_Id /= "" then ": " & Resource_Id else ""));

            when Already_Exists =>
               Error_Message :=
                 To_Bounded_String
                   (Resource_Type
                    & " already exists"
                    & (if Resource_Id /= "" then ": " & Resource_Id else ""));

            when Unavailable =>
               Error_Message :=
                 To_Bounded_String
                   (Resource_Type & " is currently unavailable");

            when Exhausted =>
               Error_Message :=
                 To_Bounded_String (Resource_Type & " has been exhausted");
         end case;
      else
         Error_Message := To_Bounded_String (Message);
      end if;

      --  Build recovery suggestion
      if Recovery = "" then
         case Kind is
            when Not_Found =>
               Recovery_Message :=
                 To_Bounded_String
                   ("Verify the resource identifier is correct");

            when Already_Exists =>
               Recovery_Message :=
                 To_Bounded_String
                   ("Use a different identifier or update existing");

            when Unavailable =>
               Recovery_Message :=
                 To_Bounded_String
                   ("Try again later or use alternative resource");

            when Exhausted =>
               Recovery_Message :=
                 To_Bounded_String ("Wait for resources to become available");
         end case;
      else
         Recovery_Message := To_Bounded_String (Recovery);
      end if;

      return
        (Base          =>
           (Category            => Resource_Error,
            Severity            => Error,
            Message             => Error_Message,
            Occurred_At         => Clock,
            Recovery_Suggestion => Recovery_Message,
            Correlation_ID      =>
              (if Correlation /= "" then To_Bounded_String (Correlation)
               else Generate_Correlation_ID),
            Error_Context       => To_Bounded_String (Context),
            Source_Location     => System.Null_Address),
         Kind          => Kind,
         Resource_Type => To_Bounded_String (Resource_Type),
         Resource_Id   => To_Bounded_String (Resource_Id));
   end Make_Resource_Error;

   --  ==========================================================================
   --  Error Utilities
   --  ==========================================================================

   function Category_Name (Cat : Error_Category) return String is
   begin
      case Cat is
         when Validation_Error =>
            return "Validation Error";

         when Business_Rule_Error =>
            return "Business Rule Violation";

         when State_Error =>
            return "Invalid State";

         when Authorization_Error =>
            return "Authorization Failed";

         when Resource_Error =>
            return "Resource Error";

         when Concurrency_Error =>
            return "Concurrency Conflict";

         when Integration_Error =>
            return "Integration Error";
      end case;
   end Category_Name;

   function Severity_Name (Sev : Error_Severity) return String is
   begin
      case Sev is
         when Warning =>
            return "Warning";

         when Error =>
            return "Error";

         when Critical =>
            return "Critical";
      end case;
   end Severity_Name;

   function To_String (Error : Domain_Error) return String is
      Time_Str : constant String := Image (Error.Occurred_At);
   begin
      return
        "["
        & Time_Str
        & "] ["
        & To_String (Error.Correlation_ID)
        & "] "
        & Severity_Name (Error.Severity)
        & " - "
        & Category_Name (Error.Category)
        & ": "
        & To_String (Error.Message)
        & (if Length (Error.Error_Context) > 0
           then " (Context: " & To_String (Error.Error_Context) & ")"
           else "")
        & (if Length (Error.Recovery_Suggestion) > 0
           then " (Suggestion: " & To_String (Error.Recovery_Suggestion) & ")"
           else "");
   end To_String;

   function To_String (Error : Validation_Error_Type) return String is
   begin
      return
        To_String (Error.Base)
        & " [Field: "
        & To_String (Error.Field_Name)
        & (if Length (Error.Invalid_Value) > 0
           then ", Value: '" & To_String (Error.Invalid_Value) & "'"
           else "")
        & "]";
   end To_String;

   function To_String (Error : Business_Rule_Error_Type) return String is
   begin
      return
        To_String (Error.Base)
        & " [Rule: "
        & To_String (Error.Rule_Name)
        & (if Length (Error.Context) > 0
           then ", Context: " & To_String (Error.Context)
           else "")
        & "]";
   end To_String;

   function To_String (Error : State_Error_Type) return String is
   begin
      return
        To_String (Error.Base)
        & " [From: "
        & To_String (Error.Current_State)
        & ", To: "
        & To_String (Error.Attempted_Transition)
        & (if Length (Error.Allowed_Transitions) > 0
           then ", Allowed: " & To_String (Error.Allowed_Transitions)
           else "")
        & "]";
   end To_String;

   function To_String (Error : Resource_Error_Type) return String is
   begin
      return
        To_String (Error.Base)
        & " [Type: "
        & To_String (Error.Resource_Type)
        & (if Length (Error.Resource_Id) > 0
           then ", ID: " & To_String (Error.Resource_Id)
           else "")
        & "]";
   end To_String;

   function User_Message (Error : Domain_Error) return String is
   begin
      --  Return a sanitized message without technical details
      return To_String (Error.Message);
   end User_Message;

   --  ==========================================================================
   --  Correlation ID and Context Utilities
   --  ==========================================================================

   function Generate_Correlation_ID
      return Correlation_ID_Strings.Bounded_String
   is
      --  Removed unused: use Interfaces.C;
      Counter_Str : constant String := Natural'Image (Correlation_Counter);
      Time_Str    : constant String := Duration'Image (Seconds (Clock));
   begin
      Correlation_Counter := Correlation_Counter + 1;
      --  Simple correlation ID: timestamp + counter
      --  In production, use proper UUID generation
      return
        To_Bounded_String
          ("CID-"
           & Time_Str (2 .. Time_Str'Last)
           & "-"
           & Counter_Str (2 .. Counter_Str'Last));
   end Generate_Correlation_ID;

   function Get_Correlation_ID (Error : Domain_Error) return String is
   begin
      return To_String (Error.Correlation_ID);
   end Get_Correlation_ID;

   function Add_Context
     (Error : Domain_Error; Additional_Context : String) return Domain_Error
   is
      Current_Context : constant String := To_String (Error.Error_Context);
      New_Context     : constant String :=
        (if Current_Context /= ""
         then Current_Context & "; " & Additional_Context
         else Additional_Context);
   begin
      -- Use Ada 2022 delta aggregate
      return (Error with delta Error_Context => To_Bounded_String (New_Context));
   end Add_Context;

   function With_Correlation_ID
     (Error : Domain_Error; Correlation_ID : String) return Domain_Error is
   begin
      -- Use Ada 2022 delta aggregate
      return (Error with delta Correlation_ID => To_Bounded_String (Correlation_ID));
   end With_Correlation_ID;

   --  ==========================================================================
   --  Error Factory Implementation
   --  ==========================================================================

   function Create_Factory
     (Default_Context : String := ""; Correlation_ID : String := "")
      return Error_Factory is
   begin
      return
        (Default_Context => To_Bounded_String (Default_Context),
         Correlation_ID  =>
           (if Correlation_ID /= "" then To_Bounded_String (Correlation_ID)
            else Generate_Correlation_ID));
   end Create_Factory;

   function Validation_Error
     (Self          : Error_Factory;
      Kind          : Validation_Error_Kind;
      Field_Name    : String;
      Invalid_Value : String := "";
      Message       : String := "") return Validation_Error_Type is
   begin
      return
        Make_Validation_Error
          (Kind          => Kind,
           Field_Name    => Field_Name,
           Invalid_Value => Invalid_Value,
           Message       => Message,
           Recovery      => "",
           Context       => To_String (Self.Default_Context),
           Correlation   => To_String (Self.Correlation_ID));
   end Validation_Error;

   function Business_Rule_Error
     (Self         : Error_Factory;
      Kind         : Business_Rule_Error_Kind;
      Rule_Name    : String;
      Rule_Context : String := "";
      Message      : String := "") return Business_Rule_Error_Type is
   begin
      return
        Make_Business_Rule_Error
          (Kind         => Kind,
           Rule_Name    => Rule_Name,
           Rule_Context => Rule_Context,
           Message      => Message,
           Recovery     => "",
           Context      => To_String (Self.Default_Context),
           Correlation  => To_String (Self.Correlation_ID));
   end Business_Rule_Error;

   function State_Error
     (Self                 : Error_Factory;
      Current_State        : String;
      Attempted_Transition : String;
      Allowed_Transitions  : String := "";
      Message              : String := "") return State_Error_Type is
   begin
      return
        Make_State_Error
          (Current_State        => Current_State,
           Attempted_Transition => Attempted_Transition,
           Allowed_Transitions  => Allowed_Transitions,
           Message              => Message,
           Recovery             => "",
           Context              => To_String (Self.Default_Context),
           Correlation          => To_String (Self.Correlation_ID));
   end State_Error;

   function Resource_Error
     (Self          : Error_Factory;
      Kind          : Resource_Error_Kind;
      Resource_Type : String;
      Resource_Id   : String := "";
      Message       : String := "") return Resource_Error_Type is
   begin
      return
        Make_Resource_Error
          (Kind          => Kind,
           Resource_Type => Resource_Type,
           Resource_Id   => Resource_Id,
           Message       => Message,
           Recovery      => "",
           Context       => To_String (Self.Default_Context),
           Correlation   => To_String (Self.Correlation_ID));
   end Resource_Error;

end Abohlib.Core.Domain.Errors;
