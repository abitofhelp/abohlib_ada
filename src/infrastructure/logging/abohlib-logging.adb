--  =============================================================================
--  Abohlib.Logging - Implementation
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

with Ada.Text_IO;
with Ada.Calendar.Formatting;

package body Abohlib.Logging is

   --  ==========================================================================
   --  Internal State (Thread-Safe)
   --  ==========================================================================

   protected Logger_State is
      procedure Set (Logger : Logger_Procedure);
      function Get return Logger_Procedure;
      procedure Set_Level (Level : Log_Level);
      function Get_Level return Log_Level;
   private
      Current_Logger : Logger_Procedure := null;
      Min_Level : Log_Level := Debug;
   end Logger_State;

   protected body Logger_State is
      procedure Set (Logger : Logger_Procedure) is
      begin
         Current_Logger := Logger;
      end Set;

      function Get return Logger_Procedure is
      begin
         return Current_Logger;
      end Get;

      procedure Set_Level (Level : Log_Level) is
      begin
         Min_Level := Level;
      end Set_Level;

      function Get_Level return Log_Level is
      begin
         return Min_Level;
      end Get_Level;
   end Logger_State;

   --  ==========================================================================
   --  Core Logging Interface Implementation
   --  ==========================================================================

   procedure Set_Logger (Logger : Logger_Procedure) is
   begin
      Logger_State.Set (Logger);
   end Set_Logger;

   function Get_Logger return Logger_Procedure is
   begin
      return Logger_State.Get;
   end Get_Logger;

   function Is_Enabled return Boolean is
   begin
      return Logger_State.Get /= null;
   end Is_Enabled;

   procedure Set_Min_Level (Level : Log_Level) is
   begin
      Logger_State.Set_Level (Level);
   end Set_Min_Level;

   function Get_Min_Level return Log_Level is
   begin
      return Logger_State.Get_Level;
   end Get_Min_Level;

   --  ==========================================================================
   --  Logging Functions Implementation
   --  ==========================================================================

   procedure Log (Level : Log_Level; Message : String) is
      Logger    : constant Logger_Procedure := Logger_State.Get;
      Min_Level : constant Log_Level := Logger_State.Get_Level;
   begin
      if Logger /= null and then Level >= Min_Level then
         Logger (Level, Message);
      end if;
   end Log;

   procedure Log_Debug (Message : String) is
   begin
      Log (Debug, Message);
   end Log_Debug;

   procedure Log_Info (Message : String) is
   begin
      Log (Info, Message);
   end Log_Info;

   procedure Log_Warning (Message : String) is
   begin
      Log (Warning, Message);
   end Log_Warning;

   procedure Log_Error (Message : String) is
   begin
      Log (Error, Message);
   end Log_Error;

   procedure Log_Critical (Message : String) is
   begin
      Log (Critical, Message);
   end Log_Critical;

   --  ==========================================================================
   --  Structured Logging Support Implementation
   --  ==========================================================================

   function New_Context return Log_Context is
   begin
      return
        (Pairs =>
           [others =>
              (Key          => Context_Key'(others => ' '),
               Key_Length   => 0,
               Value        => Context_Value'(others => ' '),
               Value_Length => 0)],
         Count => 0);
   end New_Context;

   procedure Add_Context
     (Ctx : in out Log_Context; Key : String; Value : String) is
   begin
      if Ctx.Count < Max_Context_Pairs then
         Ctx.Count := Ctx.Count + 1;
         declare
            Pair : Context_Pair renames Ctx.Pairs (Ctx.Count);
         begin
            --  Copy key
            Pair.Key_Length := Log_Key_Length_Type(Natural'Min (Key'Length, Natural(Log_Key_Length_Type'Last_Valid)));
            for I in 1 .. Natural(Pair.Key_Length) loop
               Pair.Key (I) := Key (Key'First + I - 1);
            end loop;

            --  Copy value
            Pair.Value_Length := 
              Log_Value_Length_Type(Natural'Min (Value'Length, 
                                                Natural(Log_Value_Length_Type'Last_Valid)));
            for I in 1 .. Natural(Pair.Value_Length) loop
               Pair.Value (I) := Value (Value'First + I - 1);
            end loop;
         end;
      end if;
   end Add_Context;

   procedure Log_With_Context
     (Level : Log_Level; Message : String; Context : Log_Context)
   is
      Full_Message : constant String :=
        Message & " " & Format_Context (Context);
   begin
      Log (Level, Full_Message);
   end Log_With_Context;

   --  ==========================================================================
   --  Built-in Loggers Implementation
   --  ==========================================================================

   procedure Console_Logger (Level : Log_Level; Message : String) is
   begin
      Ada.Text_IO.Put_Line (Format_Message (Level, Message));
   end Console_Logger;

   procedure Null_Logger (Level : Log_Level; Message : String) is
      pragma Unreferenced (Level, Message);
   begin
      null;
   end Null_Logger;

   procedure Timestamped_Console_Logger (Level : Log_Level; Message : String)
   is
   begin
      Ada.Text_IO.Put_Line (Format_Timestamped_Message (Level, Message));
   end Timestamped_Console_Logger;

   --  ==========================================================================
   --  Formatting Utilities Implementation
   --  ==========================================================================

   function Level_To_String (Level : Log_Level) return String is
   begin
      case Level is
         when Debug =>
            return "DEBUG";

         when Info =>
            return "INFO";

         when Warning =>
            return "WARNING";

         when Error =>
            return "ERROR";

         when Critical =>
            return "CRITICAL";
      end case;
   end Level_To_String;

   function Format_Message (Level : Log_Level; Message : String) return String
   is
   begin
      return "[" & Level_To_String (Level) & "] " & Message;
   end Format_Message;

   function Format_Timestamped_Message
     (Level   : Log_Level;
      Message : String;
      Time    : Ada.Calendar.Time := Ada.Calendar.Clock) return String
   is
      use Ada.Calendar.Formatting;
   begin
      return Image (Time) & " " & Format_Message (Level, Message);
   end Format_Timestamped_Message;

   function Format_Context (Context : Log_Context) return String is
      Result :
        String (1 .. Context.Count * 
                     (Natural(Log_Key_Length_Type'Last_Valid) + 
                      Natural(Log_Value_Length_Type'Last_Valid) + 4));
      Pos    : Natural := 1;
   begin
      if Context.Count = 0 then
         return "";
      end if;

      Result (Pos .. Pos) := "{";
      Pos := Pos + 1;

      for I in 1 .. Context.Count loop
         declare
            Pair      : Context_Pair renames Context.Pairs (I);
            Key_Str   : constant String :=
              String (Pair.Key (1 .. Natural(Pair.Key_Length)));
            Value_Str : constant String :=
              String (Pair.Value (1 .. Natural(Pair.Value_Length)));
            Pair_Str  : constant String := Key_Str & "=" & Value_Str;
         begin
            Result (Pos .. Pos + Pair_Str'Length - 1) := Pair_Str;
            Pos := Pos + Pair_Str'Length;

            if I < Context.Count then
               Result (Pos .. Pos + 1) := ", ";
               Pos := Pos + 2;
            end if;
         end;
      end loop;

      Result (Pos .. Pos) := "}";
      return Result (1 .. Pos);
   end Format_Context;

   --  ==========================================================================
   --  Logger Factory Implementation
   --  ==========================================================================

   function Create_Logger (Logger_Kind : Logger_Type) return Logger_Procedure
   is
   begin
      case Logger_Kind is
         when Null_Logger_Type =>
            return null;

         when Console =>
            return Console_Logger'Access;

         when Timestamped_Console =>
            return Timestamped_Console_Logger'Access;

         when Custom =>
            return null;  -- User must provide their own
      end case;
   end Create_Logger;

end Abohlib.Logging;
