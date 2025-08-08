--   =============================================================================
--   Test_Logging_Unit - Logging Infrastructure Unit Tests Implementation
--   =============================================================================

pragma Ada_2022;
pragma Warnings (Off, "subprogram body has no previous spec");

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Calendar;
with Abohlib.Logging;

package body Test_Logging_Unit is

   use Abohlib.Logging;

--   Test capture variables for custom loggers
   Test_Last_Level : Log_Level := Debug;
   Test_Last_Message : Unbounded_String := Null_Unbounded_String;
   Test_Call_Count : Natural := 0;

--   Custom test logger for capturing calls
   procedure Test_Logger (Level : Log_Level; Message : String) is
   begin
      Test_Last_Level := Level;
      Test_Last_Message := To_Unbounded_String (Message);
      Test_Call_Count := Test_Call_Count + 1;
   end Test_Logger;

--   Reset test state
   procedure Reset_Test_State is
   begin
      Test_Last_Level := Debug;
      Test_Last_Message := Null_Unbounded_String;
      Test_Call_Count := 0;
   end Reset_Test_State;

--   ==========================================================================
--   Logger State Management Tests
--   ==========================================================================

   function Test_Logger_Configuration return Void_Result.Result is
   begin
      Reset_Test_State;

--  Test initial state
      if Is_Enabled then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Logger should be disabled initially"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Logger_Configuration")
         ));
      end if;

--  Test setting a logger enables logging
      Set_Logger (Test_Logger'Access);

      if not Is_Enabled then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Logger should be enabled after setting"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Logger_Configuration")
         ));
      end if;

--  Test that we can retrieve the logger
      if Get_Logger /= Test_Logger'Access then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Retrieved logger should match set logger"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Logger_Configuration")
         ));
      end if;

--  Test disabling logger
      Set_Logger (null);

      if Is_Enabled then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Logger should be disabled after setting to null"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Logger_Configuration")
         ));
      end if;

      return Void_Result.Ok (True);
   end Test_Logger_Configuration;

   function Test_Logger_State_Management return Void_Result.Result is
   begin
      Reset_Test_State;

--  Test that state persists across multiple operations
      Set_Logger (Test_Logger'Access);
      Set_Min_Level (Warning);

      if Get_Min_Level /= Warning then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Min level should be preserved"),
            Details     => To_Unbounded_String ("Expected Warning, got: " & Get_Min_Level'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Logger_State_Management")
         ));
      end if;

--  Test that logger is still set
      if Get_Logger /= Test_Logger'Access then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Logger should still be set"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Logger_State_Management")
         ));
      end if;

      return Void_Result.Ok (True);
   end Test_Logger_State_Management;

   function Test_Min_Level_Configuration return Void_Result.Result is
   begin
      Reset_Test_State;

--  Test that we can set and use a logger with default min level
      Set_Logger (Test_Logger'Access);
      Set_Min_Level (Debug);  -- Ensure debug level is set
      Log (Debug, "test message");

      if Test_Call_Count = 0 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Debug message should be logged with Debug min level"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Min_Level_Configuration")
         ));
      end if;

--  Test setting higher min level filters lower messages
      Reset_Test_State;
      Set_Min_Level (Error);
      Log (Warning, "warning message");

      if Test_Call_Count > 0 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Warning message should be filtered with Error min level"),
            Details     => To_Unbounded_String ("Call count: " & Test_Call_Count'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Min_Level_Configuration")
         ));
      end if;

--  Test that higher or equal level messages pass through
      Log (Error, "error message");

      if Test_Call_Count = 0 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Error message should pass through Error min level"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Min_Level_Configuration")
         ));
      end if;

      return Void_Result.Ok (True);
   end Test_Min_Level_Configuration;

--   ==========================================================================
--   Log Level Tests
--   ==========================================================================

   function Test_Log_Level_Comparison return Void_Result.Result is
   begin
--  Test ordering relationships
      if not (Debug < Info) then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Debug should be less than Info"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Log_Level_Comparison")
         ));
      end if;

      if not (Info < Warning) then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Info should be less than Warning"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Log_Level_Comparison")
         ));
      end if;

      if not (Warning < Error) then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Warning should be less than Error"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Log_Level_Comparison")
         ));
      end if;

      if not (Error < Critical) then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Error should be less than Critical"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Log_Level_Comparison")
         ));
      end if;

--  Test <= relationships
      if not (Debug <= Debug) then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Debug should be <= Debug"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Log_Level_Comparison")
         ));
      end if;

      if not (Debug <= Info) then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Debug should be <= Info"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Log_Level_Comparison")
         ));
      end if;

      return Void_Result.Ok (True);
   end Test_Log_Level_Comparison;

   function Test_Log_Level_Filtering return Void_Result.Result is
   begin
      Reset_Test_State;
      Set_Logger (Test_Logger'Access);

--  Test each level with different min levels
      for Min_Lvl in Log_Level loop
         Set_Min_Level (Min_Lvl);

         for Test_Lvl in Log_Level loop
            Reset_Test_State;
            Log (Test_Lvl, "test");

            if Test_Lvl >= Min_Lvl then
               if Test_Call_Count = 0 then
                  return Void_Result.Err (Test_Error'(
                     Kind        => Assertion_Failed,
                     Message     => To_Unbounded_String ("Message should pass filter"),
                     Details     => To_Unbounded_String ("Test level: " & Test_Lvl'Image &
                                                       ", Min level: " & Min_Lvl'Image),
                     Line_Number => 0,
                     Test_Name   => To_Unbounded_String ("Test_Log_Level_Filtering")
                  ));
               end if;
            else
               if Test_Call_Count > 0 then
                  return Void_Result.Err (Test_Error'(
                     Kind        => Assertion_Failed,
                     Message     => To_Unbounded_String ("Message should be filtered"),
                     Details     => To_Unbounded_String ("Test level: " & Test_Lvl'Image &
                                                       ", Min level: " & Min_Lvl'Image),
                     Line_Number => 0,
                     Test_Name   => To_Unbounded_String ("Test_Log_Level_Filtering")
                  ));
               end if;
            end if;
         end loop;
      end loop;

      return Void_Result.Ok (True);
   end Test_Log_Level_Filtering;

   function Test_Level_String_Conversion return Void_Result.Result is
   begin
--  Test each level converts to appropriate string
      for Level in Log_Level loop
         declare
            Level_Str : constant String := Level_To_String (Level);
         begin
            if Level_Str'Length = 0 then
               return Void_Result.Err (Test_Error'(
                  Kind        => Assertion_Failed,
                  Message     => To_Unbounded_String ("Level string should not be empty"),
                  Details     => To_Unbounded_String ("Level: " & Level'Image),
                  Line_Number => 0,
                  Test_Name   => To_Unbounded_String ("Test_Level_String_Conversion")
               ));
            end if;

            if Level_Str'Length > 8 then
               return Void_Result.Err (Test_Error'(
                  Kind        => Assertion_Failed,
                  Message     => To_Unbounded_String ("Level string should not exceed 8 characters"),
                  Details     => To_Unbounded_String ("Level: " & Level'Image &
                                                    ", String: " & Level_Str),
                  Line_Number => 0,
                  Test_Name   => To_Unbounded_String ("Test_Level_String_Conversion")
               ));
            end if;
         end;
      end loop;

      return Void_Result.Ok (True);
   end Test_Level_String_Conversion;

--   ==========================================================================
--   Basic Logging Function Tests
--   ==========================================================================

   function Test_Basic_Logging_Functions return Void_Result.Result is
   begin
      Reset_Test_State;
      Set_Logger (Test_Logger'Access);
      Set_Min_Level (Debug);

--  Test basic Log procedure
      Log (Info, "test message");

      if Test_Last_Level /= Info then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Logger should receive correct level"),
            Details     => To_Unbounded_String ("Expected Info, got: " & Test_Last_Level'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Basic_Logging_Functions")
         ));
      end if;

      if To_String (Test_Last_Message) /= "test message" then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Logger should receive correct message"),
            Details     => To_Unbounded_String ("Expected 'test message', got: '" &
                                              To_String (Test_Last_Message) & "'"),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Basic_Logging_Functions")
         ));
      end if;

      if Test_Call_Count /= 1 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Logger should be called exactly once"),
            Details     => To_Unbounded_String ("Call count: " & Test_Call_Count'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Basic_Logging_Functions")
         ));
      end if;

      return Void_Result.Ok (True);
   end Test_Basic_Logging_Functions;

   function Test_Convenience_Logging_Functions return Void_Result.Result is
   begin
      Reset_Test_State;
      Set_Logger (Test_Logger'Access);
      Set_Min_Level (Debug);

--  Test Log_Debug
      Log_Debug ("debug msg");
      if Test_Last_Level /= Debug or else To_String (Test_Last_Message) /= "debug msg" then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Log_Debug should work correctly"),
            Details     => To_Unbounded_String ("Level: " & Test_Last_Level'Image &
                                              ", Message: " & To_String (Test_Last_Message)),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Convenience_Logging_Functions")
         ));
      end if;

--  Test Log_Info
      Reset_Test_State;
      Log_Info ("info msg");
      if Test_Last_Level /= Info or else To_String (Test_Last_Message) /= "info msg" then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Log_Info should work correctly"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Convenience_Logging_Functions")
         ));
      end if;

--  Test Log_Warning
      Reset_Test_State;
      Log_Warning ("warning msg");
      if Test_Last_Level /= Warning or else To_String (Test_Last_Message) /= "warning msg" then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Log_Warning should work correctly"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Convenience_Logging_Functions")
         ));
      end if;

--  Test Log_Error
      Reset_Test_State;
      Log_Error ("error msg");
      if Test_Last_Level /= Error or else To_String (Test_Last_Message) /= "error msg" then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Log_Error should work correctly"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Convenience_Logging_Functions")
         ));
      end if;

--  Test Log_Critical
      Reset_Test_State;
      Log_Critical ("critical msg");
      if Test_Last_Level /= Critical or else To_String (Test_Last_Message) /= "critical msg" then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Log_Critical should work correctly"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Convenience_Logging_Functions")
         ));
      end if;

      return Void_Result.Ok (True);
   end Test_Convenience_Logging_Functions;

   function Test_Logging_With_Null_Logger return Void_Result.Result is
   begin
      Reset_Test_State;
      Set_Logger (null);

--  Test that logging with null logger doesn't crash
      Log (Info, "test message");
      Log_Debug ("debug");
      Log_Info ("info");
      Log_Warning ("warning");
      Log_Error ("error");
      Log_Critical ("critical");

--  Test that no calls were made to our test logger
      if Test_Call_Count > 0 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Null logger should not call any handlers"),
            Details     => To_Unbounded_String ("Call count: " & Test_Call_Count'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Logging_With_Null_Logger")
         ));
      end if;

      return Void_Result.Ok (True);
   end Test_Logging_With_Null_Logger;

--   ==========================================================================
--   Message Formatting Tests
--   ==========================================================================

   function Test_Message_Formatting return Void_Result.Result is
   begin
--  Test basic message formatting
      declare
         Formatted : constant String := Format_Message (Info, "test message");
      begin
         if Formatted'Length = 0 then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Formatted message should not be empty"),
               Details     => Null_Unbounded_String,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Message_Formatting")
            ));
         end if;

--  Should contain both level and message
         if Formatted'Length < 12 then  -- Length of "test message"
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Formatted message should be at least as long as input"),
               Details     => To_Unbounded_String ("Formatted: " & Formatted),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Message_Formatting")
            ));
         end if;
      end;

      return Void_Result.Ok (True);
   end Test_Message_Formatting;

   function Test_Timestamped_Message_Formatting return Void_Result.Result is
   begin
--  Test timestamped message formatting
      declare
         Test_Time : constant Ada.Calendar.Time := Ada.Calendar.Clock;
         Formatted : constant String := Format_Timestamped_Message (Error, "test", Test_Time);
      begin
         if Formatted'Length = 0 then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Timestamped message should not be empty"),
               Details     => Null_Unbounded_String,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Timestamped_Message_Formatting")
            ));
         end if;

--  Should be longer than basic formatted message
         declare
            Basic : constant String := Format_Message (Error, "test");
         begin
            if Formatted'Length <= Basic'Length then
               return Void_Result.Err (Test_Error'(
                  Kind        => Assertion_Failed,
                  Message     => To_Unbounded_String ("Timestamped message should be longer than basic"),
                  Details     => To_Unbounded_String ("Timestamped length: " & Formatted'Length'Image &
                                                    ", Basic length: " & Basic'Length'Image),
                  Line_Number => 0,
                  Test_Name   => To_Unbounded_String ("Test_Timestamped_Message_Formatting")
               ));
            end if;
         end;
      end;

      return Void_Result.Ok (True);
   end Test_Timestamped_Message_Formatting;

   function Test_Message_Format_Boundaries return Void_Result.Result is
   begin
--  Test with empty message
      declare
         Empty_Formatted : constant String := Format_Message (Debug, "");
      begin
         if Empty_Formatted'Length = 0 then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Empty message formatting should still produce output"),
               Details     => Null_Unbounded_String,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Message_Format_Boundaries")
            ));
         end if;
      end;

--  Test with long message
      declare
         Long_Msg : constant String (1 .. 1000) := (others => 'X');
         Long_Formatted : constant String := Format_Message (Critical, Long_Msg);
      begin
         if Long_Formatted'Length < Long_Msg'Length then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Long message should be preserved in formatting"),
               Details     => Null_Unbounded_String,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Message_Format_Boundaries")
            ));
         end if;
      end;

      return Void_Result.Ok (True);
   end Test_Message_Format_Boundaries;

--   ==========================================================================
--   Structured Logging Tests
--   ==========================================================================

   function Test_Context_Creation return Void_Result.Result is
   begin
--  Test new context creation
      declare
         Ctx : constant Log_Context := New_Context;
      begin
         if Ctx.Count /= 0 then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("New context should have zero count"),
               Details     => To_Unbounded_String ("Count: " & Ctx.Count'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Context_Creation")
            ));
         end if;
      end;

      return Void_Result.Ok (True);
   end Test_Context_Creation;

   function Test_Context_Key_Value_Addition return Void_Result.Result is
   begin
--  Test adding key-value pairs
      declare
         Ctx : Log_Context := New_Context;
      begin
         Add_Context (Ctx, "key1", "value1");

         if Ctx.Count /= 1 then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Context count should be 1 after adding one pair"),
               Details     => To_Unbounded_String ("Count: " & Ctx.Count'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Context_Key_Value_Addition")
            ));
         end if;

--  Add another pair
         Add_Context (Ctx, "key2", "value2");

         if Ctx.Count /= 2 then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Context count should be 2 after adding two pairs"),
               Details     => To_Unbounded_String ("Count: " & Ctx.Count'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Context_Key_Value_Addition")
            ));
         end if;
      end;

      return Void_Result.Ok (True);
   end Test_Context_Key_Value_Addition;

   function Test_Context_Boundary_Conditions return Void_Result.Result is
   begin
--  Test maximum length keys and values
      declare
         Ctx : Log_Context := New_Context;
         Max_Key : String (1 .. Max_Key_Length) := (others => 'K');
         Max_Value : String (1 .. Max_Value_Length) := (others => 'V');
      begin
         Add_Context (Ctx, Max_Key, Max_Value);

         if Ctx.Count /= 1 then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Should handle maximum length key/value"),
               Details     => Null_Unbounded_String,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Context_Boundary_Conditions")
            ));
         end if;
      end;

--  Test filling context to maximum capacity
      declare
         Ctx : Log_Context := New_Context;
      begin
         for I in 1 .. Max_Context_Pairs loop
            Add_Context (Ctx, "key" & I'Image, "val" & I'Image);
         end loop;

         if Ctx.Count /= Max_Context_Pairs then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Should handle maximum context pairs"),
               Details     => To_Unbounded_String ("Expected: " & Max_Context_Pairs'Image &
                                                 ", Got: " & Ctx.Count'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Context_Boundary_Conditions")
            ));
         end if;
      end;

      return Void_Result.Ok (True);
   end Test_Context_Boundary_Conditions;

   function Test_Context_Formatting return Void_Result.Result is
   begin
--  Test context formatting
      declare
         Ctx : Log_Context := New_Context;
      begin
         Add_Context (Ctx, "user", "alice");
         Add_Context (Ctx, "action", "login");

         declare
            Formatted : constant String := Format_Context (Ctx);
         begin
            if Formatted'Length = 0 then
               return Void_Result.Err (Test_Error'(
                  Kind        => Assertion_Failed,
                  Message     => To_Unbounded_String ("Formatted context should not be empty"),
                  Details     => Null_Unbounded_String,
                  Line_Number => 0,
                  Test_Name   => To_Unbounded_String ("Test_Context_Formatting")
               ));
            end if;
         end;
      end;

--  Test empty context formatting
      declare
         Empty_Ctx : constant Log_Context := New_Context;
         Empty_Formatted : constant String := Format_Context (Empty_Ctx);
      begin
--  Empty context should produce some output (even if minimal)
         null; -- Implementation detail - may be empty or minimal string
      end;

      return Void_Result.Ok (True);
   end Test_Context_Formatting;

   function Test_Structured_Logging return Void_Result.Result is
   begin
      Reset_Test_State;
      Set_Logger (Test_Logger'Access);
      Set_Min_Level (Debug);

--  Test logging with context
      declare
         Ctx : Log_Context := New_Context;
      begin
         Add_Context (Ctx, "component", "auth");
         Add_Context (Ctx, "user_id", "12345");

         Log_With_Context (Info, "User logged in", Ctx);

         if Test_Call_Count = 0 then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Structured logging should call logger"),
               Details     => Null_Unbounded_String,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Structured_Logging")
            ));
         end if;

         if Test_Last_Level /= Info then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Structured logging should preserve level"),
               Details     => To_Unbounded_String ("Level: " & Test_Last_Level'Image),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Structured_Logging")
            ));
         end if;
      end;

      return Void_Result.Ok (True);
   end Test_Structured_Logging;

--   ==========================================================================
--   Built-in Logger Tests
--   ==========================================================================

   function Test_Console_Logger return Void_Result.Result is
   begin
--  Test that console logger doesn't crash
      Console_Logger (Info, "test console message");

--  This is primarily a smoke test - we can't easily capture console output
      return Void_Result.Ok (True);
   end Test_Console_Logger;

   function Test_Null_Logger return Void_Result.Result is
   begin
--  Test that null logger doesn't crash and does nothing
      Null_Logger (Critical, "this should be ignored");

--  This is a smoke test - null logger should silently discard messages
      return Void_Result.Ok (True);
   end Test_Null_Logger;

   function Test_Timestamped_Console_Logger return Void_Result.Result is
   begin
--  Test that timestamped console logger doesn't crash
      Timestamped_Console_Logger (Warning, "test timestamped message");

--  This is primarily a smoke test
      return Void_Result.Ok (True);
   end Test_Timestamped_Console_Logger;

--   ==========================================================================
--   Logger Factory Tests
--   ==========================================================================

   function Test_Logger_Factory_Creation return Void_Result.Result is
   begin
--  Test null logger creation
      declare
         Null_Logger_Ptr : constant Logger_Procedure := Create_Logger (Null_Logger_Type);
      begin
         if Null_Logger_Ptr /= null then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Null logger factory should return null"),
               Details     => Null_Unbounded_String,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Logger_Factory_Creation")
            ));
         end if;
      end;

--  Test console logger creation
      declare
         Console_Logger_Ptr : constant Logger_Procedure := Create_Logger (Console);
      begin
         if Console_Logger_Ptr = null then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Console logger factory should return non-null"),
               Details     => Null_Unbounded_String,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Logger_Factory_Creation")
            ));
         end if;
      end;

      return Void_Result.Ok (True);
   end Test_Logger_Factory_Creation;

   function Test_Logger_Factory_Types return Void_Result.Result is
   begin
--  Test all logger types can be created
      for Logger_Kind in Logger_Type loop
         declare
            Logger_Ptr : constant Logger_Procedure := Create_Logger (Logger_Kind);
         begin
--  Both Null_Logger_Type and Custom return null (user provides Custom)
            if Logger_Kind = Null_Logger_Type or Logger_Kind = Custom then
               if Logger_Ptr /= null then
                  return Void_Result.Err (Test_Error'(
                     Kind        => Assertion_Failed,
                     Message     => To_Unbounded_String ("Null/Custom logger type should return null"),
                     Details     => To_Unbounded_String ("Type: " & Logger_Kind'Image),
                     Line_Number => 0,
                     Test_Name   => To_Unbounded_String ("Test_Logger_Factory_Types")
                  ));
               end if;
            else
               if Logger_Ptr = null then
                  return Void_Result.Err (Test_Error'(
                     Kind        => Assertion_Failed,
                     Message     => To_Unbounded_String ("Non-null logger type should return logger"),
                     Details     => To_Unbounded_String ("Type: " & Logger_Kind'Image),
                     Line_Number => 0,
                     Test_Name   => To_Unbounded_String ("Test_Logger_Factory_Types")
                  ));
               end if;
            end if;
         end;
      end loop;

      return Void_Result.Ok (True);
   end Test_Logger_Factory_Types;

--   ==========================================================================
--   Integration and State Tests
--   ==========================================================================

   function Test_Logger_Integration return Void_Result.Result is
   begin
      Reset_Test_State;

--  Test complete integration workflow
      Set_Logger (Create_Logger (Console));
      Set_Min_Level (Warning);

--  Create context for structured logging
      declare
         Ctx : Log_Context := New_Context;
      begin
         Add_Context (Ctx, "test", "integration");
         Log_With_Context (Error, "Integration test message", Ctx);
      end;

--  Switch to test logger to verify state
      Set_Logger (Test_Logger'Access);
      Log_Error ("verification message");

      if Test_Last_Message /= "verification message" then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Logger switch should work correctly"),
            Details     => To_Unbounded_String ("Message: " & To_String (Test_Last_Message)),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Logger_Integration")
         ));
      end if;

      if Get_Min_Level /= Warning then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Min level should persist across logger changes"),
            Details     => To_Unbounded_String ("Level: " & Get_Min_Level'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Logger_Integration")
         ));
      end if;

      return Void_Result.Ok (True);
   end Test_Logger_Integration;

   function Test_Thread_Safety_Simulation return Void_Result.Result is
   begin
      Reset_Test_State;

--  Simulate rapid configuration changes (simulates thread safety requirements)
      for I in 1 .. 100 loop
         if I mod 2 = 0 then
            Set_Logger (Test_Logger'Access);
            Set_Min_Level (Debug);
         else
            Set_Logger (Create_Logger (Null_Logger_Type));
            Set_Min_Level (Critical);
         end if;

         Log (Info, "test message " & I'Image);
      end loop;

--  Verify final state is consistent
      Set_Logger (Test_Logger'Access);
      Set_Min_Level (Debug);
      Reset_Test_State;
      Log (Info, "final test");

      if Test_Call_Count /= 1 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Thread safety simulation should maintain consistency"),
            Details     => To_Unbounded_String ("Call count: " & Test_Call_Count'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Thread_Safety_Simulation")
         ));
      end if;

      return Void_Result.Ok (True);
   end Test_Thread_Safety_Simulation;

--   ==========================================================================
--   Main Test Runner
--   ==========================================================================

   function Run_All_Tests
     (Output : access Test_Output_Port'Class) return Test_Stats_Result.Result
   is
      Tests : Test_Results_Array (1 .. 24);
      Index : Positive := 1;

      procedure Add_Test_Result (Name : String; Test : access function return Void_Result.Result) is
         Result : constant Void_Result.Result := Test.all;
      begin
         if Result.Is_Ok then
            Tests (Index) := (
               Name           => To_Unbounded_String (Name),
               Status         => Passed,
               Message        => Null_Unbounded_String,
               Elapsed_Time   => 0.0,
               Line_Number    => 0,
               Correlation_ID => To_Unbounded_String ("LOGGING-" & Name)
            );
            Print_Test_Result (Tests (Index), Output);
            Index := Index + 1;
         else
            declare
               Error : constant Test_Error := Result.Get_Err;
            begin
               Tests (Index) := (
                  Name           => To_Unbounded_String (Name),
                  Status         => Failed,
                  Message        => Error.Message,
                  Elapsed_Time   => 0.0,
                  Line_Number    => Error.Line_Number,
                  Correlation_ID => To_Unbounded_String ("LOGGING-" & Name)
               );
               Print_Test_Result (Tests (Index), Output);
               Index := Index + 1;
            end;
         end if;
      end Add_Test_Result;

   begin
      Output.Write_Line ("=== Running Logging Infrastructure Unit Tests ===");
      Output.Write_Line ("");

--  Logger State Management Tests
      Add_Test_Result ("Test_Logger_Configuration", Test_Logger_Configuration'Access);
      Add_Test_Result ("Test_Logger_State_Management", Test_Logger_State_Management'Access);
      Add_Test_Result ("Test_Min_Level_Configuration", Test_Min_Level_Configuration'Access);

--  Log Level Tests
      Add_Test_Result ("Test_Log_Level_Comparison", Test_Log_Level_Comparison'Access);
      Add_Test_Result ("Test_Log_Level_Filtering", Test_Log_Level_Filtering'Access);
      Add_Test_Result ("Test_Level_String_Conversion", Test_Level_String_Conversion'Access);

--  Basic Logging Function Tests
      Add_Test_Result ("Test_Basic_Logging_Functions", Test_Basic_Logging_Functions'Access);
      Add_Test_Result ("Test_Convenience_Logging_Functions", Test_Convenience_Logging_Functions'Access);
      Add_Test_Result ("Test_Logging_With_Null_Logger", Test_Logging_With_Null_Logger'Access);

--  Message Formatting Tests
      Add_Test_Result ("Test_Message_Formatting", Test_Message_Formatting'Access);
      Add_Test_Result ("Test_Timestamped_Message_Formatting", Test_Timestamped_Message_Formatting'Access);
      Add_Test_Result ("Test_Message_Format_Boundaries", Test_Message_Format_Boundaries'Access);

--  Structured Logging Tests
      Add_Test_Result ("Test_Context_Creation", Test_Context_Creation'Access);
      Add_Test_Result ("Test_Context_Key_Value_Addition", Test_Context_Key_Value_Addition'Access);
      Add_Test_Result ("Test_Context_Boundary_Conditions", Test_Context_Boundary_Conditions'Access);
      Add_Test_Result ("Test_Context_Formatting", Test_Context_Formatting'Access);
      Add_Test_Result ("Test_Structured_Logging", Test_Structured_Logging'Access);

--  Built-in Logger Tests
      Add_Test_Result ("Test_Console_Logger", Test_Console_Logger'Access);
      Add_Test_Result ("Test_Null_Logger", Test_Null_Logger'Access);
      Add_Test_Result ("Test_Timestamped_Console_Logger", Test_Timestamped_Console_Logger'Access);

--  Logger Factory Tests
      Add_Test_Result ("Test_Logger_Factory_Creation", Test_Logger_Factory_Creation'Access);
      Add_Test_Result ("Test_Logger_Factory_Types", Test_Logger_Factory_Types'Access);

--  Integration and State Tests
      Add_Test_Result ("Test_Logger_Integration", Test_Logger_Integration'Access);
      Add_Test_Result ("Test_Thread_Safety_Simulation", Test_Thread_Safety_Simulation'Access);

--  Generate summary
      declare
         Stats_Result : constant Test_Stats_Result.Result :=
            Run_Test_Suite ("Logging_Infrastructure_Tests", Tests (1 .. Index - 1), Output);
      begin
         if Stats_Result.Is_Ok then
            declare
               Stats : constant Test_Statistics := Stats_Result.Get_Ok;
            begin
               Output.Write_Line ("");
               Print_Test_Summary ("Logging Infrastructure Unit Tests", Stats, Output);
               return Test_Stats_Result.Ok (Stats);
            end;
         else
            return Stats_Result;
         end if;
      end;
   end Run_All_Tests;

end Test_Logging_Unit;

pragma Warnings (On, "subprogram body has no previous spec");
