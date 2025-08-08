--  =============================================================================
--  Abohlib.Core.Domain.Types.Resilience - Resilience Pattern Strong Types
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Purpose:
--    Provides strong types for resilience patterns including circuit breakers,
--    retry handlers, and fault tolerance mechanisms.
--
--  Design:
--    - Domain-specific types for circuit breaker thresholds
--    - Distinct types for different failure/success contexts
--    - Type constraints enforce valid ranges
--    - Clear API contracts for resilience patterns
--  =============================================================================

pragma Ada_2022;

with Abohlib.Core.Domain.Types.Time;

package Abohlib.Core.Domain.Types.Resilience is

   --  ==========================================================================
   --  Circuit Breaker Types
   --  ==========================================================================
   
   --  Circuit breaker thresholds (distinct from retry counts)
   type Circuit_Failure_Threshold_Type is new Positive
     with Static_Predicate => Circuit_Failure_Threshold_Type in 1 .. 1000;
     
   type Circuit_Success_Threshold_Type is new Positive  
     with Static_Predicate => Circuit_Success_Threshold_Type in 1 .. 100;
     
   type Circuit_Delay_Ms_Type is new Abohlib.Core.Domain.Types.Time.Milliseconds_Type
     with Static_Predicate => Circuit_Delay_Ms_Type in 1_000 .. 300_000;  -- 1 sec to 5 min

end Abohlib.Core.Domain.Types.Resilience;