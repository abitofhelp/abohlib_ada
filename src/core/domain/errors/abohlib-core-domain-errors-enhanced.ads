--  =============================================================================
--  Abohlib.Core.Domain.Errors.Enhanced - Enhanced Error Types with Context
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================
--  Purpose:
--    Provides enhanced error types with rich context information for better
--    debugging and error tracking. Builds upon the base error types with
--    additional diagnostic capabilities.
--  =============================================================================

pragma Ada_2022;

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Finalization;
with Ada.Real_Time;
with GNAT.Source_Info;

package Abohlib.Core.Domain.Errors.Enhanced is

   --  ==========================================================================
   --  Constants
   --  ==========================================================================
   
   --  Maximum stack trace depth
   Max_Stack_Depth : constant := 20;
   
   --  Maximum number of context values
   Max_Context_Values : constant := 10;

   --  ==========================================================================
   --  Context Types
   --  ==========================================================================
   
   --  Key-value context for error details
   package Error_Context_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => String,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");
   
   --  Stack frame information
   type Stack_Frame is record
      Subprogram : Context_Strings.Bounded_String;
      File       : Context_Strings.Bounded_String;
      Line       : Natural;
      Column     : Natural;
   end record;
   
   type Stack_Trace is array (1 .. Max_Stack_Depth) of Stack_Frame;
   
   --  Cause chain for nested errors
   type Error_Cause;
   type Error_Cause_Access is access Error_Cause;
   
   type Error_Cause is record
      Error : Domain_Error;
      Next  : Error_Cause_Access;
   end record;

   --  ==========================================================================
   --  Enhanced Error Type
   --  ==========================================================================
   
   type Enhanced_Error is new Ada.Finalization.Controlled with record
      Base_Error    : Domain_Error;
      Context_Map   : Error_Context_Maps.Map;
      Trace         : Stack_Trace;  -- Renamed to avoid forward reference
      Stack_Depth   : Natural := 0;
      Root_Cause    : Error_Cause_Access := null;
      Error_Chain   : Error_Cause_Access := null;
      Machine_State : Context_Strings.Bounded_String;  -- CPU, memory state
   end record;
   
   --  RAII cleanup
   overriding procedure Initialize (Error : in out Enhanced_Error);
   overriding procedure Finalize (Error : in out Enhanced_Error);
   overriding procedure Adjust (Error : in out Enhanced_Error);

   --  ==========================================================================
   --  Error Construction
   --  ==========================================================================
   
   --  Create enhanced error with automatic context capture
   function Make_Enhanced_Error
     (Category    : Error_Category;
      Severity    : Error_Severity;
      Message     : String;
      Recovery    : String := "";
      File        : String := GNAT.Source_Info.File;
      Line        : Natural := GNAT.Source_Info.Line;
      Enclosing   : String := GNAT.Source_Info.Enclosing_Entity) return Enhanced_Error
   with
     Pre => Message'Length <= Max_Error_Message_Length and then
            Recovery'Length <= Max_Recovery_Suggestion_Length;
   
   --  Create from existing domain error
   function From_Domain_Error
     (Error : Domain_Error;
      File  : String := GNAT.Source_Info.File;
      Line  : Natural := GNAT.Source_Info.Line) return Enhanced_Error;
   
   --  Create with cause chain
   function With_Cause
     (Error : Enhanced_Error;
      Cause : Enhanced_Error) return Enhanced_Error
   with
     Post => Has_Cause (With_Cause'Result);

   --  ==========================================================================
   --  Context Management
   --  ==========================================================================
   
   --  Add context key-value pair
   function With_Context
     (Error : Enhanced_Error;
      Key   : String;
      Value : String) return Enhanced_Error
   with
     Pre => Key'Length <= Max_Field_Name_Length and then
            Value'Length <= Max_Error_Context_Length,
     Post => Has_Context (With_Context'Result, Key);
   
   --  Add multiple context values
   function With_Contexts
     (Error    : Enhanced_Error;
      Contexts : Error_Context_Maps.Map) return Enhanced_Error
   with
     Pre => Natural (Contexts.Length) <= Max_Context_Values;
   
   --  Common context helpers
   function With_Operation (Error : Enhanced_Error; Op : String) return Enhanced_Error
   is (With_Context (Error, "operation", Op));
   
   function With_Parameter (Error : Enhanced_Error; Name, Value : String) return Enhanced_Error
   is (With_Context (Error, "param_" & Name, Value));
   
   function With_File_Info (Error : Enhanced_Error; Path : String) return Enhanced_Error
   is (With_Context (Error, "file_path", Path));
   
   function With_User_Info (Error : Enhanced_Error; User_ID : String) return Enhanced_Error
   is (With_Context (Error, "user_id", User_ID));
   
   function With_Request_ID (Error : Enhanced_Error; Request_ID : String) return Enhanced_Error
   is (With_Context (Error, "request_id", Request_ID));
   
   function With_Duration (Error : Enhanced_Error; Duration : Ada.Real_Time.Time_Span) return Enhanced_Error
   is (With_Context (Error, "duration_ms", 
                     Natural'Image (Natural (Ada.Real_Time.To_Duration (Duration) * 1000.0))));

   --  ==========================================================================
   --  Query Operations
   --  ==========================================================================
   
   --  Check if error has specific context
   function Has_Context (Error : Enhanced_Error; Key : String) return Boolean;
   
   --  Get context value
   function Get_Context 
     (Error : Enhanced_Error; 
      Key : String; 
      Default : String := "") return String;
   
   --  Check if error has a cause
   function Has_Cause (Error : Enhanced_Error) return Boolean
   is (Error.Root_Cause /= null);
   
   --  Get root cause
   function Get_Root_Cause (Error : Enhanced_Error) return Domain_Error
   with Pre => Has_Cause (Error);
   
   --  Get cause chain depth
   function Get_Chain_Depth (Error : Enhanced_Error) return Natural;
   
   --  Get stack trace depth
   function Get_Stack_Depth (Error : Enhanced_Error) return Natural
   is (Error.Stack_Depth);

   --  ==========================================================================
   --  Formatting and Output
   --  ==========================================================================
   
   --  Format error as detailed string
   function To_Detailed_String (Error : Enhanced_Error) return String;
   
   --  Format error as JSON
   function To_JSON (Error : Enhanced_Error) return String;
   
   --  Format error for logging
   function To_Log_Format (Error : Enhanced_Error) return String;
   
   --  Format stack trace
   function Format_Stack_Trace (Error : Enhanced_Error) return String;
   
   --  Format cause chain
   function Format_Cause_Chain (Error : Enhanced_Error) return String;

   --  ==========================================================================
   --  Error Analysis
   --  ==========================================================================
   
   --  Check if errors are related (same correlation ID or cause)
   function Are_Related (Error1, Error2 : Enhanced_Error) return Boolean;
   
   --  Check if error matches pattern
   function Matches_Pattern
     (Error    : Enhanced_Error;
      Category : Error_Category;
      Pattern  : String := "") return Boolean;
   
   --  Get error fingerprint for deduplication
   function Get_Fingerprint (Error : Enhanced_Error) return String;

   --  ==========================================================================
   --  Error Builders (Fluent Interface)
   --  ==========================================================================
   
   type Error_Builder is tagged private;
   
   function Create_Error return Error_Builder;
   
   function With_Category (B : Error_Builder; Cat : Error_Category) return Error_Builder'Class;
   function With_Severity (B : Error_Builder; Sev : Error_Severity) return Error_Builder'Class;
   function With_Message (B : Error_Builder; Msg : String) return Error_Builder'Class;
   function Add_Context (B : Error_Builder; Key, Value : String) return Error_Builder'Class;
   function Add_Cause (B : Error_Builder; Cause : Enhanced_Error'Class) return Error_Builder'Class;
   function Build (B : Error_Builder'Class) return Enhanced_Error;

private

   type Error_Builder is tagged record
      Category     : Error_Category := Validation_Error;
      Severity     : Error_Severity := Error;
      Message      : Unbounded_String;
      Recovery     : Unbounded_String;
      Context_Map  : Error_Context_Maps.Map;
      Cause        : Error_Cause_Access := null;
   end record;

end Abohlib.Core.Domain.Errors.Enhanced;