--  =============================================================================
--  Abohlib.Infrastructure.Resilience.Generic_Retry_Handler - Retry Pattern
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Generic retry handler with configurable strategies including exponential
--  backoff, jitter, and circuit breaker integration. Supports both synchronous
--  and asynchronous operations with Ada 2022 features.
--
--  Usage Example:
--  ```ada
--  function Call_API return Response_Type;
--  function Is_Success (R : Response_Type) return Boolean;
--
--  package API_Retry is new Generic_Retry_Handler
--    (Result_Type => Response_Type,
--     Execute     => Call_API,
--     Is_Success  => Is_Success);
--
--  Result : constant Retry_Result := API_Retry.Execute_With_Retry
--    (Config => (Max_Attempts => 3,
--                Initial_Delay_Ms => 100,
--                Backoff_Strategy => Exponential_Backoff,
--                others => <>));
--  ```
--
--  Features:
--  - Multiple retry strategies (fixed, linear, exponential, fibonacci)
--  - Jitter strategies to avoid thundering herd
--  - Circuit breaker integration
--  - Async retry support
--  - Builder pattern for policies
--  - Event monitoring
--  =============================================================================

pragma Ada_2022;

with Ada.Calendar;
with Ada.Numerics.Float_Random;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Abohlib.Core.Domain.Types.Counts; use Abohlib.Core.Domain.Types.Counts;
with Abohlib.Core.Domain.Types.Time; use Abohlib.Core.Domain.Types.Time;
with Abohlib.Core.Domain.Types.Resilience; use Abohlib.Core.Domain.Types.Resilience;
with Abohlib.Infrastructure.Constants.Resilience;

generic
   --  The result type of the operation
   type Result_Type is private;

   --  The operation to retry
   with function Execute return Result_Type;

   --  Function to determine if result is successful
   with function Is_Success (Result : Result_Type) return Boolean;

   --  Optional: Extract error message from failed result
   with function Get_Error_Message (Result : Result_Type) return String is <>;

package Abohlib.Infrastructure.Resilience.Generic_Retry_Handler
is
   pragma Elaborate_Body;

   --  Retry strategies
   type Retry_Strategy is
     (Fixed_Delay,           --  Fixed delay between retries
      Linear_Backoff,        --  Linear increase in delay
      Exponential_Backoff,   --  Exponential increase in delay
      Fibonacci_Backoff);    --  Fibonacci sequence delays

   --  Jitter strategies to avoid thundering herd
   type Jitter_Strategy is
     (No_Jitter,             --  No randomization
      Full_Jitter,           --  Randomize full delay
      Equal_Jitter,          --  Add random value to half delay
      Decorrelated_Jitter);  --  Based on previous delay

   --  Retry configuration
   type Retry_Config is record
      Max_Attempts       : Retry_Count_Type := 3;
      Initial_Delay_Ms   : Retry_Delay_Ms_Type := 100;
      Max_Delay_Ms       : Retry_Delay_Ms_Type := 60_000;  --  1 minute max
      Backoff_Strategy   : Retry_Strategy := Exponential_Backoff;
      Jitter_Mode        : Jitter_Strategy := Equal_Jitter;
      Backoff_Multiplier : Multiplier_Type := Abohlib.Infrastructure.Constants.Resilience.DEFAULT_BACKOFF_MULTIPLIER;
      Timeout_Ms         : Timeout_Ms_Type := 0;  --  0 = no timeout

      --  Circuit breaker integration
      Circuit_Breaker_Enabled : Boolean := False;
      Failure_Threshold       : Circuit_Failure_Threshold_Type :=
        Abohlib.Infrastructure.Constants.Resilience.DEFAULT_CIRCUIT_BREAKER_THRESHOLD;
        --  Failures before opening circuit
      Success_Threshold       : Circuit_Success_Threshold_Type := 2;  --  Successes before closing circuit
      Half_Open_Delay_Ms      : Circuit_Delay_Ms_Type := 30_000;  --  30 seconds
   end record;

   --  Default configurations
   Default_Config : constant Retry_Config := (others => <>);

   Fast_Retry : constant Retry_Config :=
     (Max_Attempts     => 3,
      Initial_Delay_Ms => 50,
      Max_Delay_Ms     => 1_000,
      others           => <>);

   Slow_Retry : constant Retry_Config :=
     (Max_Attempts     => 5,
      Initial_Delay_Ms => 1_000,
      Max_Delay_Ms     => 300_000,
      --  5 minutes
      others           => <>);

   --  Retry statistics
   type Retry_Stats is record
      Total_Attempts      : Natural := 0;
      Successful_Attempts : Natural := 0;
      Failed_Attempts     : Natural := 0;
      Total_Delay_Ms      : Natural := 0;
      Last_Attempt_Time   : Ada.Calendar.Time;
      Last_Error          : Unbounded_String;
   end record;

   --  Result wrapper with statistics
   type Retry_Result is record
      Success : Boolean := False;
      Result  : Result_Type;
      Stats   : Retry_Stats;
   end record;

   --  Main retry function
   function Execute_With_Retry
     (Config : Retry_Config := Default_Config) return Retry_Result
   with
     Post =>
       Execute_With_Retry'Result.Stats.Total_Attempts >= 1
       and then Execute_With_Retry'Result.Stats.Total_Attempts
                <= Natural(Config.Max_Attempts);

   --  Retry with custom predicate
   generic
      with
        function Should_Retry
          (Result : Result_Type; Attempt : Positive) return Boolean;
   function Execute_With_Custom_Retry
     (Config : Retry_Config := Default_Config) return Retry_Result;

   --  Retry with fallback
   generic
      with function Fallback return Result_Type;
   function Execute_With_Fallback
     (Config : Retry_Config := Default_Config) return Retry_Result with
     Post =>
       Execute_With_Fallback'Result.Success
       or else Execute_With_Fallback'Result.Result = Fallback;

   --  Async retry support (returns immediately, retries in background)
   task type Async_Retry_Task is
      entry Start (Config : Retry_Config);
      entry Get_Result (Result : out Retry_Result);
      entry Cancel;
   end Async_Retry_Task;

   type Async_Retry_Handle is access Async_Retry_Task;

   --  Start async retry
   function Execute_Async
     (Config : Retry_Config := Default_Config) return Async_Retry_Handle;

   --  Policy combination
   type Retry_Policy is tagged private;

   --  Builder pattern for retry policies
   function Create_Policy return Retry_Policy;

   function With_Max_Attempts
     (Policy : Retry_Policy; Attempts : Positive) return Retry_Policy;

   function With_Delay
     (Policy : Retry_Policy; Delay_Ms : Natural) return Retry_Policy;

   function With_Exponential_Backoff
     (Policy     : Retry_Policy; 
      Multiplier : Float := Abohlib.Infrastructure.Constants.Resilience.DEFAULT_BACKOFF_MULTIPLIER) 
      return Retry_Policy;

   function With_Jitter
     (Policy : Retry_Policy; Strategy : Jitter_Strategy) return Retry_Policy;

   function With_Circuit_Breaker
     (Policy            : Retry_Policy; 
      Failure_Threshold : Natural := 
        Abohlib.Infrastructure.Constants.Resilience.DEFAULT_CIRCUIT_BREAKER_THRESHOLD)
      return Retry_Policy;

   function Build (Policy : Retry_Policy) return Retry_Config;

   --  Execute with policy
   function Execute_With_Policy (Policy : Retry_Policy) return Retry_Result;

   --  Retry event for monitoring
   type Retry_Event_Type is
     (None,              -- No event (for initialization)
      Attempt_Started,
      Attempt_Succeeded,
      Attempt_Failed,
      Retry_Exhausted,
      Circuit_Opened,
      Circuit_Closed);

   type Retry_Event is record
      Event_Type : Retry_Event_Type;
      Attempt    : Natural;
      Delay_Ms   : Natural;
      Error_Msg  : Unbounded_String;
      Timestamp  : Ada.Calendar.Time;
   end record;

   --  Event handler for monitoring
   type Retry_Event_Handler is access procedure (Event : Retry_Event);

   --  Set global event handler
   procedure Set_Event_Handler (Handler : Retry_Event_Handler);

private

   --  Random generator for jitter
   Gen : Ada.Numerics.Float_Random.Generator;

   --  Circuit breaker state
   type Circuit_State is (Closed, Open, Half_Open);

   type Circuit_Breaker is record
      State             : Circuit_State := Closed;
      Failure_Count     : Natural := 0;
      Success_Count     : Natural := 0;
      Last_State_Change : Ada.Calendar.Time;
   end record;

   --  Protected wrapper for thread-safe circuit breaker and event handler
   protected type Protected_Circuit_Breaker is
      
      --  Query operations
      function Get_State return Circuit_State;
      function Get_Failure_Count return Natural;
      function Get_Success_Count return Natural;
      function Can_Execute (Config : Retry_Config) return Boolean;
      
      --  State modification operations  
      procedure Record_Success 
        (Config : Retry_Config;
         Out_State : out Circuit_State;
         Out_Event : out Retry_Event)
      with Pre => Config.Circuit_Breaker_Enabled,
           Post => Get_Success_Count >= Get_Success_Count'Old;
      
      procedure Record_Failure 
        (Config : Retry_Config;
         Out_State : out Circuit_State;
         Out_Event : out Retry_Event)
      with Pre => Config.Circuit_Breaker_Enabled,
           Post => Get_Failure_Count >= Get_Failure_Count'Old;
      
      procedure Reset
      with Post => Get_State = Closed and then
                   Get_Failure_Count = 0 and then
                   Get_Success_Count = 0;
      
      --  Event handler management
      procedure Set_Event_Handler (New_Handler : Retry_Event_Handler);
      function Get_Event_Handler return Retry_Event_Handler;
      
      --  Initialize with current time
      procedure Initialize;
      
   private
      Circuit : Circuit_Breaker := 
        (State => Closed, 
         Failure_Count => 0, 
         Success_Count => 0, 
         Last_State_Change => Ada.Calendar.Clock);
      Handler : Retry_Event_Handler := null;
   end Protected_Circuit_Breaker;

   --  Global protected circuit breaker instance
   Global_Circuit : Protected_Circuit_Breaker;

   --  Policy implementation
   type Retry_Policy is tagged record
      Config : Retry_Config := Default_Config;
   end record;

   --  Helper functions
   function Calculate_Delay
     (Config : Retry_Config; Attempt : Positive) return Duration;

   function Apply_Jitter
     (Base_Delay : Duration;
      Strategy   : Jitter_Strategy;
      Previous   : Duration := 0.0) return Duration;

   procedure Fire_Event (Event : Retry_Event);

end Abohlib.Infrastructure.Resilience.Generic_Retry_Handler;
