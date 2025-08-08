--  =============================================================================
--  Abohlib.Infrastructure.Resilience.Generic_Retry_Handler - Implementation
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Implementation Details:
--    This generic retry handler implements sophisticated retry logic with:
--    - Multiple backoff strategies (exponential, linear, constant)
--    - Jitter to prevent thundering herd
--    - Circuit breaker integration
--    - Comprehensive statistics and event tracking
--
--  Thread Safety:
--    The implementation is thread-safe when using the global circuit breaker.
--    Each retry execution maintains its own state.
--  =============================================================================

pragma Ada_2022;

with Ada.Exceptions;
with Abohlib.Core.Domain.Constants.Time_Units;

package body Abohlib.Infrastructure.Resilience.Generic_Retry_Handler is

   use Abohlib.Core.Domain.Constants.Time_Units;

   --  Constants  
   --  Equal jitter factor (0.5) provides balanced randomization
   --  This helps prevent synchronized retries across multiple clients
   Jitter_Half_Factor : constant := 
     Abohlib.Infrastructure.Constants.Resilience.EQUAL_JITTER_FACTOR;

   ------------------------
   -- Execute_With_Retry --
   ------------------------
   --  Main retry execution logic
   --  Algorithm:
   --    1. Loop up to Max_Attempts times
   --    2. Check circuit breaker (if enabled) 
   --    3. Execute the operation via Execute_Operation
   --    4. If successful or non-retryable, return
   --    5. Calculate backoff delay
   --    6. Apply jitter to prevent thundering herd
   --    7. Wait and retry

   function Execute_With_Retry
     (Config : Retry_Config := Default_Config) return Retry_Result
   is
      Result         : Retry_Result;
      Current_Delay  : Duration := 0.0;
      Previous_Delay : Duration := 0.0;
   begin
      Result.Stats.Total_Attempts := 0;

      --  Main retry loop
      for Attempt in 1 .. Natural (Config.Max_Attempts) loop
         Result.Stats.Total_Attempts := Attempt;
         Result.Stats.Last_Attempt_Time := Ada.Calendar.Clock;

         --  Fire attempt started event for monitoring
         Fire_Event
           ((Event_Type => Attempt_Started,
             Attempt    => Attempt,
             Delay_Ms   => Natural (Current_Delay * Ms_Per_Second),
             Error_Msg  => Null_Unbounded_String,
             Timestamp  => Ada.Calendar.Clock));

         --  Check circuit breaker state before attempting
         if Config.Circuit_Breaker_Enabled
           and then not Global_Circuit.Can_Execute (Config)
         then
            --  Circuit is open, fail fast
            Result.Success := False;
            Result.Stats.Failed_Attempts := Result.Stats.Failed_Attempts + 1;
            Result.Stats.Last_Error :=
              To_Unbounded_String ("Circuit breaker is open");
            Fire_Event
              ((Event_Type => Circuit_Opened,
                Attempt    => Attempt,
                Delay_Ms   => 0,
                Error_Msg  => Result.Stats.Last_Error,
                Timestamp  => Ada.Calendar.Clock));
            exit;
         end if;

         --  Execute the operation
         begin
            Result.Result := Execute;
            Result.Success := Is_Success (Result.Result);

            if Result.Success then
               Result.Stats.Successful_Attempts :=
                 Result.Stats.Successful_Attempts + 1;
               Fire_Event
                 ((Event_Type => Attempt_Succeeded,
                   Attempt    => Attempt,
                   Delay_Ms   => 0,
                   Error_Msg  => Null_Unbounded_String,
                   Timestamp  => Ada.Calendar.Clock));

               --  Update circuit breaker on success
               if Config.Circuit_Breaker_Enabled then
                  declare
                     CB_State : Circuit_State;
                     CB_Event : Retry_Event;
                  begin
                     Global_Circuit.Record_Success (Config, CB_State, CB_Event);
                     if CB_Event.Event_Type = Circuit_Closed then
                        CB_Event.Attempt := Attempt;
                        Fire_Event (CB_Event);
                     end if;
                  end;
               end if;

               exit;  --  Success, no need to retry

            else
               Result.Stats.Failed_Attempts :=
                 Result.Stats.Failed_Attempts + 1;
               Result.Stats.Last_Error :=
                 To_Unbounded_String (Get_Error_Message (Result.Result));
               Fire_Event
                 ((Event_Type => Attempt_Failed,
                   Attempt    => Attempt,
                   Delay_Ms   => 0,
                   Error_Msg  => Result.Stats.Last_Error,
                   Timestamp  => Ada.Calendar.Clock));

               --  Update circuit breaker on failure
               if Config.Circuit_Breaker_Enabled then
                  declare
                     CB_State : Circuit_State;
                     CB_Event : Retry_Event;
                  begin
                     Global_Circuit.Record_Failure (Config, CB_State, CB_Event);
                     if CB_Event.Event_Type = Circuit_Opened then
                        CB_Event.Attempt := Attempt;
                        CB_Event.Error_Msg := 
                          To_Unbounded_String ("Circuit breaker opened");
                        Fire_Event (CB_Event);
                     end if;
                  end;
               end if;
            end if;

         exception
            when E : others =>
               Result.Success := False;
               Result.Stats.Failed_Attempts :=
                 Result.Stats.Failed_Attempts + 1;
               Result.Stats.Last_Error :=
                 To_Unbounded_String
                   ("Exception: " & Ada.Exceptions.Exception_Message (E));
               Fire_Event
                 ((Event_Type => Attempt_Failed,
                   Attempt    => Attempt,
                   Delay_Ms   => 0,
                   Error_Msg  => Result.Stats.Last_Error,
                   Timestamp  => Ada.Calendar.Clock));
         end;

         --  Don't delay after the last attempt
         if Attempt < Natural (Config.Max_Attempts) and then not Result.Success then
            Current_Delay := Calculate_Delay (Config, Positive (Attempt));
            Current_Delay :=
              Apply_Jitter (Current_Delay, Config.Jitter_Mode, Previous_Delay);
            Previous_Delay := Current_Delay;

            Result.Stats.Total_Delay_Ms :=
              Result.Stats.Total_Delay_Ms + Natural (Current_Delay * Ms_Per_Second);

            delay Current_Delay;
         end if;
      end loop;

      if not Result.Success then
         Fire_Event
           ((Event_Type => Retry_Exhausted,
             Attempt    => Result.Stats.Total_Attempts,
             Delay_Ms   => 0,
             Error_Msg  => Result.Stats.Last_Error,
             Timestamp  => Ada.Calendar.Clock));
      end if;

      return Result;
   end Execute_With_Retry;

   -------------------------------
   -- Execute_With_Custom_Retry --
   -------------------------------

   function Execute_With_Custom_Retry
     (Config : Retry_Config := Default_Config) return Retry_Result
   is
      Result        : Retry_Result;
      Current_Delay : Duration := 0.0;
   begin
      Result.Stats.Total_Attempts := 0;

      for Attempt in 1 .. Natural (Config.Max_Attempts) loop
         Result.Stats.Total_Attempts := Attempt;
         Result.Stats.Last_Attempt_Time := Ada.Calendar.Clock;

         Result.Result := Execute;
         Result.Success := Is_Success (Result.Result);

         if Result.Success then
            Result.Stats.Successful_Attempts :=
              Result.Stats.Successful_Attempts + 1;
            exit;
         else
            Result.Stats.Failed_Attempts := Result.Stats.Failed_Attempts + 1;
            if not Should_Retry (Result.Result, Positive (Attempt)) then
               exit;  --  Custom predicate says don't retry

            end if;
         end if;

         if Attempt < Natural (Config.Max_Attempts) and then not Result.Success then
            Current_Delay := Calculate_Delay (Config, Positive (Attempt));
            Result.Stats.Total_Delay_Ms :=
              Result.Stats.Total_Delay_Ms + Natural (Current_Delay * Ms_Per_Second);
            delay Current_Delay;
         end if;
      end loop;

      return Result;
   end Execute_With_Custom_Retry;

   ---------------------------
   -- Execute_With_Fallback --
   ---------------------------

   function Execute_With_Fallback
     (Config : Retry_Config := Default_Config) return Retry_Result
   is
      Result : constant Retry_Result := Execute_With_Retry (Config);
   begin
      if Result.Success then
         return Result;
      else
         return (Success => True, Result => Fallback, Stats => Result.Stats);
      end if;
   end Execute_With_Fallback;

   -------------------
   -- Execute_Async --
   -------------------

   function Execute_Async
     (Config : Retry_Config := Default_Config) return Async_Retry_Handle
   is
      Handle : constant Async_Retry_Handle := new Async_Retry_Task;
   begin
      Handle.Start (Config);
      return Handle;
   end Execute_Async;

   -------------------
   -- Create_Policy --
   -------------------

   function Create_Policy return Retry_Policy is
   begin
      return (Config => Default_Config);
   end Create_Policy;

   ------------------------
   -- With_Max_Attempts --
   ------------------------

   function With_Max_Attempts
     (Policy : Retry_Policy; Attempts : Positive) return Retry_Policy
   is
      New_Policy : Retry_Policy := Policy;
   begin
      New_Policy.Config.Max_Attempts := Retry_Count_Type (Attempts);
      return New_Policy;
   end With_Max_Attempts;

   ----------------
   -- With_Delay --
   ----------------

   function With_Delay
     (Policy : Retry_Policy; Delay_Ms : Natural) return Retry_Policy
   is
      New_Policy : Retry_Policy := Policy;
   begin
      New_Policy.Config.Initial_Delay_Ms := Retry_Delay_Ms_Type (Delay_Ms);
      return New_Policy;
   end With_Delay;

   ------------------------------
   -- With_Exponential_Backoff --
   ------------------------------

   function With_Exponential_Backoff
     (Policy     : Retry_Policy; 
      Multiplier : Float := Abohlib.Infrastructure.Constants.Resilience.DEFAULT_BACKOFF_MULTIPLIER) 
      return Retry_Policy
   is
      New_Policy : Retry_Policy := Policy;
   begin
      New_Policy.Config.Backoff_Strategy := Exponential_Backoff;
      New_Policy.Config.Backoff_Multiplier := Multiplier;
      return New_Policy;
   end With_Exponential_Backoff;

   -----------------
   -- With_Jitter --
   -----------------

   function With_Jitter
     (Policy : Retry_Policy; Strategy : Jitter_Strategy) return Retry_Policy
   is
      New_Policy : Retry_Policy := Policy;
   begin
      New_Policy.Config.Jitter_Mode := Strategy;
      return New_Policy;
   end With_Jitter;

   --------------------------
   -- With_Circuit_Breaker --
   --------------------------

   function With_Circuit_Breaker
     (Policy            : Retry_Policy; 
      Failure_Threshold : Natural := 
        Abohlib.Infrastructure.Constants.Resilience.DEFAULT_CIRCUIT_BREAKER_THRESHOLD)
      return Retry_Policy
   is
      New_Policy : Retry_Policy := Policy;
   begin
      New_Policy.Config.Circuit_Breaker_Enabled := True;
      New_Policy.Config.Failure_Threshold := Circuit_Failure_Threshold_Type (Failure_Threshold);
      return New_Policy;
   end With_Circuit_Breaker;

   -----------
   -- Build --
   -----------

   function Build (Policy : Retry_Policy) return Retry_Config is
   begin
      return Policy.Config;
   end Build;

   --------------------------
   -- Execute_With_Policy --
   --------------------------

   function Execute_With_Policy (Policy : Retry_Policy) return Retry_Result is
   begin
      return Execute_With_Retry (Policy.Config);
   end Execute_With_Policy;

   -----------------------
   -- Set_Event_Handler --
   -----------------------

   procedure Set_Event_Handler (Handler : Retry_Event_Handler) is
   begin
      Global_Circuit.Set_Event_Handler (Handler);
   end Set_Event_Handler;

   ---------------------
   -- Calculate_Delay --
   ---------------------
   --  Calculate the base delay for a retry attempt based on strategy
   --  Strategies:
   --    Fixed_Delay: Same delay for all attempts
   --    Linear_Backoff: Delay increases linearly (delay * attempt)
   --    Exponential_Backoff: Delay grows exponentially (delay * multiplier^(attempt-1))
   --    Fibonacci_Backoff: Delay follows Fibonacci sequence
   --  Note: Result is capped by Max_Delay_Ms to prevent excessive waits

   function Calculate_Delay
     (Config : Retry_Config; Attempt : Positive) return Duration
   is
      Base_Delay : constant Float := Float (Config.Initial_Delay_Ms) / Ms_Per_Second;
      Calculated : Float;
   begin
      case Config.Backoff_Strategy is
         when Fixed_Delay =>
            --  Constant delay between attempts
            Calculated := Base_Delay;

         when Linear_Backoff =>
            --  Delay increases linearly: initial * attempt
            --  Example: 100ms, 200ms, 300ms, 400ms...
            Calculated := Base_Delay * Float (Attempt);

         when Exponential_Backoff =>
            --  Delay grows exponentially: initial * multiplier^(attempt-1)
            --  Example with multiplier=2: 100ms, 200ms, 400ms, 800ms...
            Calculated :=
              Base_Delay * (Config.Backoff_Multiplier ** (Attempt - 1));

         when Fibonacci_Backoff =>
            --  Delay follows Fibonacci sequence scaled by initial delay
            --  Example: 100ms, 100ms, 200ms, 300ms, 500ms, 800ms...
            declare
               Fib_A : Natural := 0;
               Fib_B : Natural := 1;
               Fib_C : Natural;
            begin
               --  Calculate Nth Fibonacci number
               for I in 1 .. Attempt loop
                  Fib_C := Fib_A + Fib_B;
                  Fib_A := Fib_B;
                  Fib_B := Fib_C;
               end loop;
               Calculated := Base_Delay * Float (Fib_A);
            end;
      end case;

      --  Cap at max delay to prevent excessive waits
      if Calculated * Ms_Per_Second > Float (Config.Max_Delay_Ms) then
         Calculated := Float (Config.Max_Delay_Ms) / Ms_Per_Second;
      end if;

      return Duration (Calculated);
   end Calculate_Delay;

   ------------------
   -- Apply_Jitter --
   ------------------
   --  Add randomization to delays to prevent thundering herd
   --  Jitter modes:
   --    None: No randomization
   --    Full: Random between 0 and calculated delay
   --    Equal: Random between half and full delay (recommended)
   --    Decorrelated: Uses previous delay for better distribution

   function Apply_Jitter
     (Base_Delay : Duration;
      Strategy   : Jitter_Strategy;
      Previous   : Duration := 0.0) return Duration
   is
      Random : Float;
   begin
      case Strategy is
         when No_Jitter =>
            return Base_Delay;

         when Full_Jitter =>
            Random := Ada.Numerics.Float_Random.Random (Gen);
            return Duration (Float (Base_Delay) * Random);

         when Equal_Jitter =>
            Random := Ada.Numerics.Float_Random.Random (Gen);
            return
              Duration
                (Float (Base_Delay) * Jitter_Half_Factor + Float (Base_Delay) * Jitter_Half_Factor * Random);

         when Decorrelated_Jitter =>
            Random := Ada.Numerics.Float_Random.Random (Gen);
            declare
               Min_Delay : constant Float := Float (Base_Delay);
               Max_Delay : constant Float := 
                 Float (Previous) * Abohlib.Infrastructure.Constants.Resilience.DECORRELATED_JITTER_MULTIPLIER;
            begin
               return Duration (Min_Delay + (Max_Delay - Min_Delay) * Random);
            end;
      end case;
   end Apply_Jitter;

   ----------------
   -- Fire_Event --
   ----------------

   procedure Fire_Event (Event : Retry_Event) is
      Handler : constant Retry_Event_Handler := Global_Circuit.Get_Event_Handler;
   begin
      if Handler /= null then
         Handler (Event);
      end if;
   end Fire_Event;

   ----------------------
   -- Async_Retry_Task --
   ----------------------

   task body Async_Retry_Task is
      Config  : Retry_Config;
      Result  : Retry_Result;
      Started : Boolean := False;
   begin
      select
         accept Start (Config : Retry_Config) do
            Async_Retry_Task.Config := Config;
            Started := True;
         end Start;
      or
         terminate;
      end select;

      if Started then
         Result := Execute_With_Retry (Config);

         select
            accept Get_Result (Result : out Retry_Result) do
               Get_Result.Result := Async_Retry_Task.Result;
            end Get_Result;
         or
            accept Cancel;
         end select;
      end if;
   end Async_Retry_Task;

   --  ===========================================================================
   --  Protected_Circuit_Breaker Implementation
   --  ===========================================================================
   
   protected body Protected_Circuit_Breaker is
      
      ---------------
      -- Get_State --
      ---------------
      
      function Get_State return Circuit_State is
      begin
         return Circuit.State;
      end Get_State;
      
      ----------------------
      -- Get_Failure_Count --
      ----------------------
      
      function Get_Failure_Count return Natural is
      begin
         return Circuit.Failure_Count;
      end Get_Failure_Count;
      
      ----------------------
      -- Get_Success_Count --
      ----------------------
      
      function Get_Success_Count return Natural is
      begin
         return Circuit.Success_Count;
      end Get_Success_Count;
      
      -----------------
      -- Can_Execute --
      -----------------
      
      function Can_Execute (Config : Retry_Config) return Boolean is
         use Ada.Calendar;
      begin
         case Circuit.State is
            when Closed =>
               return True;
               
            when Open =>
               if Clock - Circuit.Last_State_Change
                 > Duration (Natural (Config.Half_Open_Delay_Ms)) / Ms_Per_Second
               then
                  -- Note: State transition happens in Record_Success/Record_Failure
                  -- This just reports if execution is allowed
                  return True;
               else
                  return False;
               end if;
               
            when Half_Open =>
               return True;
         end case;
      end Can_Execute;
      
      --------------------
      -- Record_Success --
      --------------------
      
      procedure Record_Success 
        (Config : Retry_Config;
         Out_State : out Circuit_State;
         Out_Event : out Retry_Event) is
         use Ada.Calendar;
      begin
         Out_Event := (Event_Type => None, others => <>);
         
         -- Handle state transition from Open to Half_Open if timeout expired
         if Circuit.State = Open and then
            Clock - Circuit.Last_State_Change > 
            Duration (Natural (Config.Half_Open_Delay_Ms)) / Ms_Per_Second
         then
            Circuit.State := Half_Open;
            Circuit.Success_Count := 0;
         end if;
         
         -- Increment success count
         Circuit.Success_Count := Circuit.Success_Count + 1;
         
         -- Check for state transition from Half_Open to Closed
         if Circuit.State = Half_Open and then
            Circuit.Success_Count >= Natural (Config.Success_Threshold)
         then
            Circuit.State := Closed;
            Circuit.Failure_Count := 0;
            Out_Event := (Event_Type => Circuit_Closed,
                         Attempt => 0,
                         Delay_Ms => 0,
                         Error_Msg => Null_Unbounded_String,
                         Timestamp => Clock);
         end if;
         
         Out_State := Circuit.State;
      end Record_Success;
      
      --------------------
      -- Record_Failure --
      --------------------
      
      procedure Record_Failure 
        (Config : Retry_Config;
         Out_State : out Circuit_State;
         Out_Event : out Retry_Event) is
         use Ada.Calendar;
      begin
         Out_Event := (Event_Type => None, others => <>);
         
         -- Increment failure count
         Circuit.Failure_Count := Circuit.Failure_Count + 1;
         
         -- Check for state transition from Closed to Open
         if Circuit.State = Closed and then
            Circuit.Failure_Count >= Natural (Config.Failure_Threshold)
         then
            Circuit.State := Open;
            Circuit.Last_State_Change := Clock;
            Out_Event := (Event_Type => Circuit_Opened,
                         Attempt => 0,
                         Delay_Ms => 0,
                         Error_Msg => Null_Unbounded_String,
                         Timestamp => Clock);
         end if;
         
         Out_State := Circuit.State;
      end Record_Failure;
      
      -----------
      -- Reset --
      -----------
      
      procedure Reset is
      begin
         Circuit.State := Closed;
         Circuit.Failure_Count := 0;
         Circuit.Success_Count := 0;
         Circuit.Last_State_Change := Ada.Calendar.Clock;
      end Reset;
      
      -----------------------
      -- Set_Event_Handler --
      -----------------------
      
      procedure Set_Event_Handler (New_Handler : Retry_Event_Handler) is
      begin
         Handler := New_Handler;
      end Set_Event_Handler;
      
      -----------------------
      -- Get_Event_Handler --
      -----------------------
      
      function Get_Event_Handler return Retry_Event_Handler is
      begin
         return Handler;
      end Get_Event_Handler;
      
      ----------------
      -- Initialize --
      ----------------
      
      procedure Initialize is
      begin
         Circuit.Last_State_Change := Ada.Calendar.Clock;
      end Initialize;
      
   end Protected_Circuit_Breaker;

begin
   --  Initialize random generator
   Ada.Numerics.Float_Random.Reset (Gen);
   --  Initialize protected circuit breaker
   Global_Circuit.Initialize;
end Abohlib.Infrastructure.Resilience.Generic_Retry_Handler;
