# Resilience Patterns Guide

**Version 1.0.0**

## Overview

Building reliable systems requires handling failures gracefully. Abohlib provides battle-tested resilience patterns including retry handlers, circuit breakers, and timeout management. This guide explains how to use these patterns effectively.

## Table of Contents

1. [Introduction](#introduction)
2. [Retry Handlers](#retry-handlers)
3. [Circuit Breakers](#circuit-breakers)
4. [Timeout Management](#timeout-management)
5. [Combining Patterns](#combining-patterns)
6. [Best Practices](#best-practices)
7. [Common Scenarios](#common-scenarios)

## Introduction

### Why Resilience Matters

In distributed systems, failures are inevitable:
- Networks experience temporary issues
- Services become temporarily overloaded
- Resources may be briefly unavailable

Rather than failing immediately, resilient systems:
- **Retry** transient failures with appropriate delays
- **Circuit break** to prevent cascading failures
- **Timeout** to avoid hanging indefinitely
- **Degrade gracefully** when services are unavailable

### Abohlib's Approach

Abohlib provides generic, composable resilience components:
- Type-safe configuration with strong domain types
- Result-based error handling (no exceptions)
- Configurable strategies (exponential backoff, jitter, etc.)
- Integration with monitoring and logging

## Retry Handlers

### Basic Retry Usage

```ada
with Abohlib.Infrastructure.Resilience.Generic_Retry_Handler;
with Abohlib.Core.Domain.Types.Resilience; use Abohlib.Core.Domain.Types.Resilience;

-- Define your operation that might fail
function Fetch_Data (URL : String) return Data_Result.Result;

-- Instantiate retry handler for your operation
package Data_Retry is new Generic_Retry_Handler
  (Input_Type => String,
   Output_Type => Data_Type,
   Operation => Fetch_Data);

-- Configure retry behavior
Config : constant Retry_Config := 
  (Max_Attempts => Retry_Count_Type (3),
   Base_Delay => Retry_Delay_Ms_Type (100),    -- Start with 100ms
   Max_Delay => Retry_Delay_Ms_Type (5000),     -- Cap at 5 seconds
   Backoff_Factor => 2.0,                       -- Double each time
   Jitter_Enabled => True);                     -- Add randomness

-- Use the retry handler
Result : constant Data_Result.Result := 
  Data_Retry.Execute_With_Retry (Config, "https://api.example.com/data");

if Result.Is_Ok then
   Process_Data (Result.Get_Ok);
else
   Log_Error ("Failed after retries: " & Result.Get_Err.Message);
end if;
```

### Retry Strategies

#### Exponential Backoff
Delays increase exponentially to reduce load on failing services:
```ada
-- Delays: 100ms, 200ms, 400ms, 800ms, 1600ms...
Config : constant Retry_Config := 
  (Base_Delay => Retry_Delay_Ms_Type (100),
   Backoff_Factor => 2.0,
   others => <>);
```

#### Exponential Backoff with Jitter
Adds randomness to prevent thundering herd:
```ada
-- Delays: 50-150ms, 100-300ms, 200-600ms... (randomized)
Config : constant Retry_Config := 
  (Base_Delay => Retry_Delay_Ms_Type (100),
   Backoff_Factor => 2.0,
   Jitter_Enabled => True,
   others => <>);
```

#### Fixed Delay
Same delay between all attempts:
```ada
-- Delays: 500ms, 500ms, 500ms...
Config : constant Retry_Config := 
  (Base_Delay => Retry_Delay_Ms_Type (500),
   Backoff_Factor => 1.0,  -- No increase
   others => <>);
```

### Retryable vs Non-Retryable Errors

Not all errors should trigger retries:

```ada
function Smart_Fetch (URL : String) return Data_Result.Result is
   Response : constant HTTP_Result.Result := HTTP_Get (URL);
begin
   if Response.Is_Err then
      case Response.Get_Err.Status_Code is
         when 429 | 500..599 =>
            -- Retryable: Rate limit or server errors
            return Data_Result.Err (Retryable_Error);
            
         when 400..499 =>
            -- Non-retryable: Client errors
            return Data_Result.Err (Permanent_Error);
            
         when others =>
            -- Unknown: treat as retryable
            return Data_Result.Err (Retryable_Error);
      end case;
   end if;
   
   return Parse_Response (Response.Get_Ok);
end Smart_Fetch;
```

## Circuit Breakers

Circuit breakers prevent cascading failures by stopping requests to failing services:

### Circuit Breaker States

1. **Closed**: Normal operation, requests pass through
2. **Open**: Failures exceeded threshold, requests fail immediately  
3. **Half-Open**: Testing if service recovered, limited requests allowed

### Basic Circuit Breaker Usage

```ada
with Abohlib.Infrastructure.Resilience.Circuit_Breaker;

-- Create circuit breaker
Breaker : Circuit_Breaker.Circuit_Breaker_Type;

-- Configure thresholds
Breaker.Configure
  (Failure_Threshold => Failure_Count_Type (5),      -- Open after 5 failures
   Success_Threshold => Success_Count_Type (2),      -- Close after 2 successes
   Timeout => Timeout_Ms_Type (60_000),              -- Reset after 1 minute
   Half_Open_Requests => Request_Count_Type (3));    -- Test with 3 requests

-- Use circuit breaker
function Protected_Call (Data : String) return Result_Type.Result is
begin
   if not Breaker.Can_Execute then
      return Result_Type.Err ("Circuit breaker is open");
   end if;
   
   declare
      Result : constant Result_Type.Result := Actual_Call (Data);
   begin
      if Result.Is_Ok then
         Breaker.Record_Success;
      else
         Breaker.Record_Failure;
      end if;
      
      return Result;
   end;
end Protected_Call;
```

### Circuit Breaker with Retry

Combine circuit breakers with retry for maximum resilience:

```ada
-- Retry with circuit breaker protection
function Resilient_Call (Data : String) return Result_Type.Result is
begin
   -- Check circuit breaker first
   if not Breaker.Can_Execute then
      return Result_Type.Err ("Service unavailable (circuit open)");
   end if;
   
   -- Try with retries
   declare
      Result : constant Result_Type.Result := 
         Retry_Handler.Execute_With_Retry (Retry_Config, Data);
   begin
      -- Update circuit breaker based on final result
      if Result.Is_Ok then
         Breaker.Record_Success;
      else
         Breaker.Record_Failure;
      end if;
      
      return Result;
   end;
end Resilient_Call;
```

## Timeout Management

Timeouts prevent operations from hanging indefinitely:

### Basic Timeout Usage

```ada
with Abohlib.Core.Domain.Types.Time; use Abohlib.Core.Domain.Types.Time;

-- Configure timeout
Timeout : constant Timeout_Ms_Type := Timeout_Ms_Type (5000);  -- 5 seconds

-- Use with operations
select
   delay To_Duration (Timeout);
   return Result_Type.Err ("Operation timed out");
then abort
   return Slow_Operation (Data);
end select;
```

### Timeout Strategies

#### Aggressive Timeouts
For user-facing operations where responsiveness matters:
```ada
User_Timeout : constant Timeout_Ms_Type := Timeout_Ms_Type (3000);  -- 3 seconds max
```

#### Conservative Timeouts  
For batch operations where completion matters more than speed:
```ada
Batch_Timeout : constant Timeout_Ms_Type := Timeout_Ms_Type (300_000);  -- 5 minutes
```

#### Adaptive Timeouts
Adjust based on historical performance:
```ada
type Adaptive_Timeout is record
   Base : Timeout_Ms_Type;
   Multiplier : Float;
   History : Performance_History;
end record;

function Calculate_Timeout (AT : Adaptive_Timeout) return Timeout_Ms_Type is
   Avg_Duration : constant Duration := AT.History.Average_Duration;
   Calculated : constant Natural := Natural (Float (Avg_Duration) * AT.Multiplier);
begin
   return Timeout_Ms_Type (Natural'Max (To_Natural (AT.Base), Calculated));
end Calculate_Timeout;
```

## Combining Patterns

### The Resilience Stack

Layer patterns for comprehensive protection:

```ada
function Fully_Resilient_Operation (Request : Request_Type) return Response_Result.Result is
   
   -- Layer 1: Input validation
   Validated : constant Request_Result.Result := Validate_Request (Request);
   if Validated.Is_Err then
      return Response_Result.Err ("Invalid request: " & Validated.Get_Err.Message);
   end if;
   
   -- Layer 2: Circuit breaker check
   if not Service_Breaker.Can_Execute then
      -- Return cached/default response if available
      if Has_Cached_Response (Request) then
         return Response_Result.Ok (Get_Cached_Response (Request));
      else
         return Response_Result.Err ("Service temporarily unavailable");
      end if;
   end if;
   
   -- Layer 3: Timeout protection
   declare
      Start_Time : constant Time := Clock;
   begin
      select
         delay To_Duration (Operation_Timeout);
         return Response_Result.Err ("Operation timed out");
      then abort
         -- Layer 4: Retry logic
         declare
            Result : constant Response_Result.Result := 
               Retry_Handler.Execute_With_Retry (Retry_Config, Validated.Get_Ok);
         begin
            -- Update circuit breaker
            if Result.Is_Ok then
               Service_Breaker.Record_Success;
               Cache_Response (Request, Result.Get_Ok);  -- Cache for future
            else
               Service_Breaker.Record_Failure;
            end if;
            
            -- Record metrics
            Record_Operation_Duration (Clock - Start_Time);
            
            return Result;
         end;
      end select;
   end;
end Fully_Resilient_Operation;
```

## Best Practices

### 1. Start Simple
Begin with basic retry logic, add circuit breakers when you see cascading failures:
```ada
-- Start with this
Result := Retry_Handler.Execute_With_Retry (Simple_Config, Input);

-- Evolve to circuit breaker when needed
if Breaker.Can_Execute then
   Result := Retry_Handler.Execute_With_Retry (Config, Input);
   Update_Breaker (Result);
end if;
```

### 2. Monitor and Measure
Track resilience metrics to tune configurations:
```ada
type Resilience_Metrics is record
   Total_Attempts : Natural := 0;
   Successful_Attempts : Natural := 0;
   Failed_Attempts : Natural := 0;
   Retried_Attempts : Natural := 0;
   Circuit_Opens : Natural := 0;
   Timeouts : Natural := 0;
   Average_Duration : Duration := 0.0;
end record;
```

### 3. Fail Fast When Appropriate
Not everything should be retried:
```ada
case Error_Type is
   when Network_Timeout | Service_Unavailable =>
      -- Retry these
      return Execute_With_Retry (Config, Request);
      
   when Invalid_Credentials | Not_Found | Invalid_Request =>
      -- Fail immediately
      return Result.Err (Error);
end case;
```

### 4. Provide Fallbacks
Always have a plan B:
```ada
-- Primary service with fallback
Result := Call_Primary_Service (Request);
if Result.Is_Err then
   -- Try fallback service
   Result := Call_Fallback_Service (Request);
   if Result.Is_Err then
      -- Use cached or default data
      return Get_Default_Response (Request);
   end if;
end if;
```

## Common Scenarios

### Database Connection Resilience
```ada
-- Retry connection with exponential backoff
DB_Retry_Config : constant Retry_Config :=
  (Max_Attempts => Retry_Count_Type (5),
   Base_Delay => Retry_Delay_Ms_Type (100),
   Max_Delay => Retry_Delay_Ms_Type (10_000),
   Backoff_Factor => 2.0,
   Jitter_Enabled => True);

function Get_DB_Connection return Connection_Result.Result is
begin
   return DB_Retry.Execute_With_Retry 
     (DB_Retry_Config, Connection_Params);
end Get_DB_Connection;
```

### API Rate Limiting
```ada
-- Handle rate limits with smart backoff
function Call_Rate_Limited_API (Endpoint : String) return API_Result.Result is
   Result : constant API_Result.Result := API_Call (Endpoint);
begin
   if Result.Is_Err and then Result.Get_Err.Status = 429 then
      -- Rate limited - use retry-after header if available
      declare
         Retry_After : constant Natural := 
           Get_Retry_After_Header (Result.Get_Err);
         Delay_Ms : constant Retry_Delay_Ms_Type := 
           Retry_Delay_Ms_Type (Retry_After * 1000);
      begin
         delay To_Duration (Delay_Ms);
         return API_Call (Endpoint);  -- One retry after suggested delay
      end;
   end if;
   
   return Result;
end Call_Rate_Limited_API;
```

### Distributed Transaction Resilience
```ada
-- Saga pattern with compensations
procedure Process_Order_Saga (Order : Order_Type) is
   -- Step 1: Reserve inventory
   Inventory_Result : constant Result := Reserve_Inventory (Order.Items);
   if Inventory_Result.Is_Err then
      return;  -- Nothing to compensate
   end if;
   
   -- Step 2: Charge payment
   Payment_Result : constant Result := Charge_Payment (Order.Payment);
   if Payment_Result.Is_Err then
      -- Compensate: Release inventory
      Release_Inventory (Order.Items);
      return;
   end if;
   
   -- Step 3: Schedule shipping  
   Shipping_Result : constant Result := Schedule_Shipping (Order.Shipping);
   if Shipping_Result.Is_Err then
      -- Compensate: Refund and release
      Refund_Payment (Order.Payment);
      Release_Inventory (Order.Items);
      return;
   end if;
   
   -- All steps succeeded
   Confirm_Order (Order);
end Process_Order_Saga;
```

## Summary

Resilience patterns help build systems that handle failures gracefully:

1. **Use retry handlers** for transient failures
2. **Add circuit breakers** to prevent cascading failures
3. **Set appropriate timeouts** to avoid hanging
4. **Combine patterns** for comprehensive protection
5. **Monitor metrics** to tune configurations
6. **Provide fallbacks** for critical operations

Remember: The goal isn't to prevent all failures, but to handle them gracefully when they occur.

## See Also

- [Result Pattern Guide](RESULT_PATTERN_GUIDE.md) - Error handling foundation
- [Code Examples](../EXAMPLES.md#error-recovery) - Practical resilience examples
- [Testing Guide](../TESTING_GUIDE.md#testing-resilience) - Testing failure scenarios