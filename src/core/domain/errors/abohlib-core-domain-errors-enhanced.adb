--  =============================================================================
--  Abohlib.Core.Domain.Errors.Enhanced - Implementation
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

with Ada.Strings.Fixed;
with Ada.Calendar.Formatting;
with Ada.Unchecked_Deallocation;
with GNAT.Traceback;

package body Abohlib.Core.Domain.Errors.Enhanced is

   procedure Free is new Ada.Unchecked_Deallocation (Error_Cause, Error_Cause_Access);

   --  ==========================================================================
   --  RAII Implementation
   --  ==========================================================================
   
   overriding procedure Initialize (Error : in out Enhanced_Error) is
   begin
      Error.Context_Map.Clear;
      Error.Stack_Depth := 0;
      Error.Root_Cause := null;
      Error.Error_Chain := null;
   end Initialize;
   
   overriding procedure Finalize (Error : in out Enhanced_Error) is
      Current, Next : Error_Cause_Access;
   begin
      --  Free the cause chain
      Current := Error.Root_Cause;
      while Current /= null loop
         Next := Current.Next;
         Free (Current);
         Current := Next;
      end loop;
      
      Error.Context_Map.Clear;
   end Finalize;
   
   overriding procedure Adjust (Error : in out Enhanced_Error) is
      Source_Current : Error_Cause_Access := Error.Root_Cause;
      Target_Current : Error_Cause_Access;
      Target_Previous : Error_Cause_Access := null;
   begin
      --  Deep copy the cause chain
      Error.Root_Cause := null;
      Error.Error_Chain := null;
      
      while Source_Current /= null loop
         Target_Current := new Error_Cause'(Error => Source_Current.Error,
                                           Next => null);
         
         if Error.Root_Cause = null then
            Error.Root_Cause := Target_Current;
         else
            Target_Previous.Next := Target_Current;
         end if;
         
         Target_Previous := Target_Current;
         Source_Current := Source_Current.Next;
      end loop;
      
      Error.Error_Chain := Target_Previous;
   end Adjust;

   --  ==========================================================================
   --  Error Construction
   --  ==========================================================================
   
   function Capture_Stack_Trace return Stack_Trace is
      Result : Stack_Trace;
      Traceback : GNAT.Traceback.Tracebacks_Array (1 .. Max_Stack_Depth);
      Len    : Natural;
   begin
      --  Get raw traceback
      GNAT.Traceback.Call_Chain (Traceback, Len);
      
      --  Convert to our format (simplified for now)
      for I in 1 .. Natural'Min (Len, Max_Stack_Depth) loop
         Result (I).Subprogram := Context_Strings.To_Bounded_String ("<subprogram>");
         Result (I).File := Context_Strings.To_Bounded_String ("<file>");
         Result (I).Line := I;
         Result (I).Column := 0;
      end loop;
      
      return Result;
   end Capture_Stack_Trace;
   
   function Make_Enhanced_Error
     (Category    : Error_Category;
      Severity    : Error_Severity;
      Message     : String;
      Recovery    : String := "";
      File        : String := GNAT.Source_Info.File;
      Line        : Natural := GNAT.Source_Info.Line;
      Enclosing   : String := GNAT.Source_Info.Enclosing_Entity) return Enhanced_Error
   is
      Result : Enhanced_Error;
   begin
      --  Set base error
      Result.Base_Error := Default_Domain_Error;
      Result.Base_Error.Category := Category;
      Result.Base_Error.Severity := Severity;
      Result.Base_Error.Message := Error_Strings.To_Bounded_String (Message);
      Result.Base_Error.Recovery_Suggestion := Recovery_Strings.To_Bounded_String (Recovery);
      
      --  Add automatic context
      Result.Context_Map.Insert ("source_file", File);
      Result.Context_Map.Insert ("source_line", Line'Image);
      Result.Context_Map.Insert ("enclosing_entity", Enclosing);
      
      --  Capture stack trace
      Result.Trace := Capture_Stack_Trace;
      Result.Stack_Depth := Max_Stack_Depth;  -- Simplified
      
      return Result;
   end Make_Enhanced_Error;
   
   function From_Domain_Error
     (Error : Domain_Error;
      File  : String := GNAT.Source_Info.File;
      Line  : Natural := GNAT.Source_Info.Line) return Enhanced_Error
   is
      Result : Enhanced_Error;
   begin
      Result.Base_Error := Error;
      Result.Context_Map.Insert ("enhanced_at_file", File);
      Result.Context_Map.Insert ("enhanced_at_line", Line'Image);
      Result.Trace := Capture_Stack_Trace;
      Result.Stack_Depth := Max_Stack_Depth;
      return Result;
   end From_Domain_Error;
   
   function With_Cause
     (Error : Enhanced_Error;
      Cause : Enhanced_Error) return Enhanced_Error
   is
      Result : Enhanced_Error := Error;
      New_Cause : constant Error_Cause_Access := 
        new Error_Cause'(Error => Cause.Base_Error, Next => null);
   begin
      if Result.Root_Cause = null then
         Result.Root_Cause := New_Cause;
         Result.Error_Chain := New_Cause;
      else
         Result.Error_Chain.Next := New_Cause;
         Result.Error_Chain := New_Cause;
      end if;
      return Result;
   end With_Cause;

   --  ==========================================================================
   --  Context Management
   --  ==========================================================================
   
   function With_Context
     (Error : Enhanced_Error;
      Key   : String;
      Value : String) return Enhanced_Error
   is
      Result : Enhanced_Error := Error;
   begin
      Result.Context_Map.Include (Key, Value);
      return Result;
   end With_Context;
   
   function With_Contexts
     (Error    : Enhanced_Error;
      Contexts : Error_Context_Maps.Map) return Enhanced_Error
   is
      Result : Enhanced_Error := Error;
   begin
      for C in Contexts.Iterate loop
         Result.Context_Map.Include 
           (Error_Context_Maps.Key (C), 
            Error_Context_Maps.Element (C));
      end loop;
      return Result;
   end With_Contexts;

   --  ==========================================================================
   --  Query Operations
   --  ==========================================================================
   
   function Has_Context (Error : Enhanced_Error; Key : String) return Boolean is
   begin
      return Error.Context_Map.Contains (Key);
   end Has_Context;
   
   function Get_Context 
     (Error : Enhanced_Error; 
      Key : String; 
      Default : String := "") return String
   is
   begin
      if Error.Context_Map.Contains (Key) then
         return Error.Context_Map.Element (Key);
      else
         return Default;
      end if;
   end Get_Context;
   
   function Get_Root_Cause (Error : Enhanced_Error) return Domain_Error is
   begin
      return Error.Root_Cause.Error;
   end Get_Root_Cause;
   
   function Get_Chain_Depth (Error : Enhanced_Error) return Natural is
      Current : Error_Cause_Access := Error.Root_Cause;
      Depth : Natural := 0;
   begin
      while Current /= null loop
         Depth := Depth + 1;
         Current := Current.Next;
      end loop;
      return Depth;
   end Get_Chain_Depth;

   --  ==========================================================================
   --  Formatting and Output
   --  ==========================================================================
   
   function To_Detailed_String (Error : Enhanced_Error) return String is
      Result : Unbounded_String;
   begin
      --  Header
      Append (Result, "=== ENHANCED ERROR REPORT ===" & ASCII.LF);
      Append (Result, "Time: " & 
              Ada.Calendar.Formatting.Image (Error.Base_Error.Occurred_At) & ASCII.LF);
      Append (Result, "Category: " & Error.Base_Error.Category'Image & ASCII.LF);
      Append (Result, "Severity: " & Error.Base_Error.Severity'Image & ASCII.LF);
      Append (Result, "Message: " & 
              Error_Strings.To_String (Error.Base_Error.Message) & ASCII.LF);
      
      --  Context
      if not Error.Context_Map.Is_Empty then
         Append (Result, ASCII.LF & "Context:" & ASCII.LF);
         for C in Error.Context_Map.Iterate loop
            Append (Result, "  " & Error_Context_Maps.Key (C) & ": " &
                    Error_Context_Maps.Element (C) & ASCII.LF);
         end loop;
      end if;
      
      --  Cause chain
      if Error.Root_Cause /= null then
         Append (Result, ASCII.LF & "Cause Chain:" & ASCII.LF);
         declare
            Current : Error_Cause_Access := Error.Root_Cause;
            Index : Natural := 1;
         begin
            while Current /= null loop
               Append (Result, "  [" & Index'Image & "] " &
                       Error_Strings.To_String (Current.Error.Message) & ASCII.LF);
               Current := Current.Next;
               Index := Index + 1;
            end loop;
         end;
      end if;
      
      --  Recovery suggestion
      if Recovery_Strings.Length (Error.Base_Error.Recovery_Suggestion) > 0 then
         Append (Result, ASCII.LF & "Recovery: " &
                 Recovery_Strings.To_String (Error.Base_Error.Recovery_Suggestion) & ASCII.LF);
      end if;
      
      return To_String (Result);
   end To_Detailed_String;
   
   function To_JSON (Error : Enhanced_Error) return String is
      Result : Unbounded_String;
   begin
      Append (Result, "{");
      Append (Result, """timestamp"":""" & 
              Ada.Calendar.Formatting.Image (Error.Base_Error.Occurred_At) & """,");
      Append (Result, """category"":""" & Error.Base_Error.Category'Image & """,");
      Append (Result, """severity"":""" & Error.Base_Error.Severity'Image & """,");
      Append (Result, """message"":""" & 
              Error_Strings.To_String (Error.Base_Error.Message) & """,");
      Append (Result, """correlation_id"":""" &
              Correlation_ID_Strings.To_String (Error.Base_Error.Correlation_ID) & """,");
      
      --  Context as nested object
      Append (Result, """context"":{");
      declare
         First : Boolean := True;
      begin
         for C in Error.Context_Map.Iterate loop
            if not First then
               Append (Result, ",");
            end if;
            Append (Result, """" & Error_Context_Maps.Key (C) & """:""" &
                    Error_Context_Maps.Element (C) & """");
            First := False;
         end loop;
      end;
      Append (Result, "}");
      
      Append (Result, "}");
      return To_String (Result);
   end To_JSON;
   
   function To_Log_Format (Error : Enhanced_Error) return String is
      Result : Unbounded_String;
   begin
      --  Compact single-line format for logs
      Append (Result, "[" & Error.Base_Error.Severity'Image & "] ");
      Append (Result, Error.Base_Error.Category'Image & ": ");
      Append (Result, Error_Strings.To_String (Error.Base_Error.Message));
      Append (Result, " (id=" & 
              Correlation_ID_Strings.To_String (Error.Base_Error.Correlation_ID) & ")");
      
      --  Add key context values
      if Error.Context_Map.Contains ("operation") then
         Append (Result, " op=" & Error.Context_Map.Element ("operation"));
      end if;
      if Error.Context_Map.Contains ("user_id") then
         Append (Result, " user=" & Error.Context_Map.Element ("user_id"));
      end if;
      
      return To_String (Result);
   end To_Log_Format;
   
   function Format_Stack_Trace (Error : Enhanced_Error) return String is
      Result : Unbounded_String;
   begin
      Append (Result, "Stack Trace:" & ASCII.LF);
      for I in 1 .. Error.Stack_Depth loop
         Append (Result, "  #" & I'Image & " at " &
                 Context_Strings.To_String (Error.Trace (I).File) & ":" &
                 Error.Trace (I).Line'Image & ASCII.LF);
      end loop;
      return To_String (Result);
   end Format_Stack_Trace;
   
   function Format_Cause_Chain (Error : Enhanced_Error) return String is
      Result : Unbounded_String;
      Current : Error_Cause_Access := Error.Root_Cause;
   begin
      while Current /= null loop
         Append (Result, "Caused by: " &
                 Error_Strings.To_String (Current.Error.Message) & ASCII.LF);
         Current := Current.Next;
      end loop;
      return To_String (Result);
   end Format_Cause_Chain;

   --  ==========================================================================
   --  Error Analysis
   --  ==========================================================================
   
   function Are_Related (Error1, Error2 : Enhanced_Error) return Boolean is
   begin
      --  Same correlation ID
      if Error1.Base_Error.Correlation_ID = Error2.Base_Error.Correlation_ID then
         return True;
      end if;
      
      --  Check if one is in the cause chain of the other
      declare
         Current : Error_Cause_Access := Error1.Root_Cause;
      begin
         while Current /= null loop
            if Current.Error.Correlation_ID = Error2.Base_Error.Correlation_ID then
               return True;
            end if;
            Current := Current.Next;
         end loop;
      end;
      
      return False;
   end Are_Related;
   
   function Matches_Pattern
     (Error    : Enhanced_Error;
      Category : Error_Category;
      Pattern  : String := "") return Boolean
   is
   begin
      if Error.Base_Error.Category /= Category then
         return False;
      end if;
      
      if Pattern'Length > 0 then
         declare
            Message : constant String := 
              Error_Strings.To_String (Error.Base_Error.Message);
         begin
            return Ada.Strings.Fixed.Index (Message, Pattern) > 0;
         end;
      end if;
      
      return True;
   end Matches_Pattern;
   
   function Get_Fingerprint (Error : Enhanced_Error) return String is
      Hash_Value : constant Ada.Containers.Hash_Type := Ada.Strings.Hash 
        (Error.Base_Error.Category'Image & "|" &
         Error_Strings.To_String (Error.Base_Error.Message));
   begin
      --  Create a unique fingerprint based on category, key context, and message pattern
      return Hash_Value'Image;
   end Get_Fingerprint;

   --  ==========================================================================
   --  Error Builder Implementation
   --  ==========================================================================
   
   function Create_Error return Error_Builder is
   begin
      return (Category => Validation_Error,
              Severity => Error,
              Message => Null_Unbounded_String,
              Recovery => Null_Unbounded_String,
              Context_Map => Error_Context_Maps.Empty_Map,
              Cause => null);
   end Create_Error;
   
   function With_Category (B : Error_Builder; Cat : Error_Category) return Error_Builder'Class is
      Result : Error_Builder := B;
   begin
      Result.Category := Cat;
      return Result;
   end With_Category;
   
   function With_Severity (B : Error_Builder; Sev : Error_Severity) return Error_Builder'Class is
      Result : Error_Builder := B;
   begin
      Result.Severity := Sev;
      return Result;
   end With_Severity;
   
   function With_Message (B : Error_Builder; Msg : String) return Error_Builder'Class is
      Result : Error_Builder := B;
   begin
      Result.Message := To_Unbounded_String (Msg);
      return Result;
   end With_Message;
   
   function Add_Context (B : Error_Builder; Key, Value : String) return Error_Builder'Class is
      Result : Error_Builder := B;
   begin
      Result.Context_Map.Include (Key, Value);
      return Result;
   end Add_Context;
   
   function Add_Cause (B : Error_Builder; Cause : Enhanced_Error'Class) return Error_Builder'Class is
      Result : Error_Builder := B;
   begin
      Result.Cause := new Error_Cause'(Error => Cause.Base_Error, Next => Result.Cause);
      return Result;
   end Add_Cause;
   
   function Build (B : Error_Builder'Class) return Enhanced_Error is
   begin
      return Make_Enhanced_Error
        (Category => B.Category,
         Severity => B.Severity,
         Message => To_String (B.Message),
         Recovery => To_String (B.Recovery))
        .With_Contexts (B.Context_Map);
   end Build;

end Abohlib.Core.Domain.Errors.Enhanced;