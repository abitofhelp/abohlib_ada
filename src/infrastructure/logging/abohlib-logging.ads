--  =============================================================================
--  Abohlib.Logging - Generic Logging Interface
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Purpose:
--    Provides a generic, dependency-free logging interface that allows users
--    to plug in their preferred logging backend (simple_logging, elogs,
--    Ada.Text_IO, or custom solutions). Designed to be SPARK-compatible.
--
--  Usage:
--    Applications using this library can:
--    1. Use the default null logger (no output)
--    2. Set a simple Put_Line logger for basic needs
--    3. Integrate with simple_logging, elogs, or other logging libraries
--    4. Implement custom logging logic
--
--  Example - Basic Usage:
--    --  Use default (no logging)
--    null;
--
--    --  Use simple console logger
--    Abohlib.Logging.Set_Logger(Abohlib.Logging.Console_Logger'Access);
--
--    --  Log a message
--    Abohlib.Logging.Log(Info, "Application started");
--
--  Example - Integration with simple_logging:
--    procedure Simple_Logging_Bridge(Level : Log_Level; Message : String) is
--    begin
--       case Level is
--          when Debug => Simple_Logging.Debug(Message);
--          when Info => Simple_Logging.Info(Message);
--          when Warning => Simple_Logging.Warning(Message);
--          when Error => Simple_Logging.Error(Message);
--          when Critical => Simple_Logging.Critical(Message);
--       end case;
--    end Simple_Logging_Bridge;
--
--    Abohlib.Logging.Set_Logger(Simple_Logging_Bridge'Access);
--
--  Example - Integration with elogs (SPARK-proven):
--    procedure Elogs_Bridge(Level : Log_Level; Message : String) is
--    begin
--       --  Convert to elogs API (which is SPARK-proven)
--       case Level is
--          when Debug => Elogs.Log_Debug(Message);
--          when Info => Elogs.Log_Info(Message);
--          -- etc.
--       end case;
--    end Elogs_Bridge;
--  =============================================================================

pragma Ada_2022;
--  SPARK_Mode disabled due to protected type complexities

with Ada.Calendar;
with Abohlib.Core.Domain.Types.Strings; use Abohlib.Core.Domain.Types.Strings;

package Abohlib.Logging
  with Pure => False
is

   --  ==========================================================================
   --  Log Levels
   --  ==========================================================================

   type Log_Level is (Debug, Info, Warning, Error, Critical)
   with Default_Value => Info;

   --  Log level ordering for filtering
   overriding
   function "<" (Left, Right : Log_Level) return Boolean
   is (Log_Level'Pos (Left) < Log_Level'Pos (Right));

   overriding
   function "<=" (Left, Right : Log_Level) return Boolean
   is (Log_Level'Pos (Left) <= Log_Level'Pos (Right));

   --  ==========================================================================
   --  Core Logging Interface
   --  ==========================================================================

   --  Logger procedure type - the core abstraction
   type Logger_Procedure is
     access procedure (Level : Log_Level; Message : String);

   --  Set the global logger (null to disable logging)
   procedure Set_Logger (Logger : Logger_Procedure);
   pragma Inline (Set_Logger);

   --  Get current logger (may be null)
   function Get_Logger return Logger_Procedure;
   pragma Inline (Get_Logger);

   --  Check if logging is enabled
   function Is_Enabled return Boolean;
   pragma Inline (Is_Enabled);

   --  Set minimum log level (messages below this level are ignored)
   procedure Set_Min_Level (Level : Log_Level);
   pragma Inline (Set_Min_Level);

   --  Get current minimum log level
   function Get_Min_Level return Log_Level;
   pragma Inline (Get_Min_Level);

   --  ==========================================================================
   --  Logging Functions
   --  ==========================================================================

   --  Main logging procedure
   procedure Log (Level : Log_Level; Message : String);

   --  Convenience procedures for each level
   procedure Log_Debug (Message : String);
   pragma Inline (Log_Debug);

   procedure Log_Info (Message : String);
   pragma Inline (Log_Info);

   procedure Log_Warning (Message : String);
   pragma Inline (Log_Warning);

   procedure Log_Error (Message : String);
   pragma Inline (Log_Error);

   procedure Log_Critical (Message : String);
   pragma Inline (Log_Critical);

   --  ==========================================================================
   --  Structured Logging Support
   --  ==========================================================================

   --  Maximum number of context key-value pairs
   Max_Context_Pairs : constant := 10;
   
   --  Maximum lengths for logging context
   Max_Key_Length : constant Natural := Natural(Log_Key_Length_Type'Last_Valid);
   Max_Value_Length : constant Natural := Natural(Log_Value_Length_Type'Last_Valid);

   --  Bounded strings for context (to avoid dynamic allocation)
   --  Note: These are removed as we use the domain types directly

   type Context_Key is new String (1 .. Positive(Log_Key_Length_Type'Last_Valid));
   type Context_Value is new String (1 .. Positive(Log_Value_Length_Type'Last_Valid));

   type Context_Pair is record
      Key          : Context_Key;
      Key_Length   : Log_Key_Length_Type := 0;
      Value        : Context_Value;
      Value_Length : Log_Value_Length_Type := 0;
   end record;

   type Context_Array is array (1 .. Max_Context_Pairs) of Context_Pair;

   type Log_Context is record
      Pairs : Context_Array;
      Count : Natural := 0;
   end record;

   --  Create a new context
   function New_Context return Log_Context
   with Post => New_Context'Result.Count = 0;

   --  Add a key-value pair to context
   procedure Add_Context
     (Ctx : in out Log_Context; Key : String; Value : String)
   with
     Pre =>
       Ctx.Count < Max_Context_Pairs
       and then Key'Length <= Max_Key_Length
       and then Value'Length <= Max_Value_Length,
     Post => Ctx.Count = Ctx.Count'Old + 1;

   --  Log with context
   procedure Log_With_Context
     (Level : Log_Level; Message : String; Context : Log_Context);

   --  ==========================================================================
   --  Built-in Loggers
   --  ==========================================================================

   --  Simple console logger using Ada.Text_IO
   procedure Console_Logger (Level : Log_Level; Message : String);

   --  Null logger (discards all messages)
   procedure Null_Logger (Level : Log_Level; Message : String)
   with SPARK_Mode => On;

   --  Timestamped console logger
   procedure Timestamped_Console_Logger (Level : Log_Level; Message : String);

   --  ==========================================================================
   --  Formatting Utilities
   --  ==========================================================================

   --  Convert log level to string
   function Level_To_String (Level : Log_Level) return String
   with Post => Level_To_String'Result'Length <= 8;

   --  Format a log message with level prefix
   function Format_Message (Level : Log_Level; Message : String) return String;

   --  Format a log message with timestamp
   function Format_Timestamped_Message
     (Level   : Log_Level;
      Message : String;
      Time    : Ada.Calendar.Time := Ada.Calendar.Clock) return String;

   --  Format context as string
   function Format_Context (Context : Log_Context) return String;

   --  ==========================================================================
   --  Logger Factory Pattern (for common integrations)
   --  ==========================================================================

   type Logger_Type is
     (Null_Logger_Type, Console, Timestamped_Console, Custom);

   --  Create a logger of specified type
   function Create_Logger (Logger_Kind : Logger_Type) return Logger_Procedure
   with
     Post =>
       (if Logger_Kind = Null_Logger_Type then Create_Logger'Result = null);

private

   --  SPARK_Mode disabled due to protected type complexities

   --  Abstract state handled by SPARK

   --  Note: The actual implementation will use a protected object or
   --  atomic operations to ensure thread safety while maintaining
   --  SPARK compatibility where possible.

end Abohlib.Logging;
