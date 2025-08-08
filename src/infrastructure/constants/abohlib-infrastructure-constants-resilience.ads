--  =============================================================================
--  Abohlib.Infrastructure.Constants.Resilience - Resilience Algorithm Constants
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Purpose:
--    Defines constants for resilience patterns including retry handlers,
--    circuit breakers, and backoff strategies. These constants eliminate
--    magic numbers and provide well-tuned default values based on industry
--    best practices and research.
--
--  Design Rationale:
--    These values are based on:
--    - AWS Architecture Center recommendations for retry strategies
--    - Netflix Hystrix circuit breaker patterns  
--    - Google SRE practices for fault tolerance
--    - Academic research on exponential backoff algorithms
--
--  Usage:
--    Use these constants as default values in retry policies and circuit
--    breaker configurations. They can be overridden for specific use cases
--    but provide sensible defaults for most scenarios.
--
--  Examples:
--    Multiplier : Float := DEFAULT_BACKOFF_MULTIPLIER;  -- Instead of: 2.0
--    Threshold : Natural := DEFAULT_CIRCUIT_BREAKER_THRESHOLD;  -- Instead of: 5
--
--  References:
--    - AWS Architecture Center: Exponential Backoff and Jitter
--    - "The Tail at Scale" (Dean & Barroso, 2013)
--    - Netflix Hystrix Circuit Breaker pattern documentation
--  =============================================================================

pragma Ada_2022;

package Abohlib.Infrastructure.Constants.Resilience is
   pragma Pure;

   --  ==========================================================================
   --  Exponential Backoff Constants
   --  ==========================================================================
   
   --  Default multiplier for exponential backoff (base^attempt * multiplier)
   --  Value: 2.0 provides standard doubling behavior (1s, 2s, 4s, 8s, ...)
   --  Research shows 2.0 is optimal for most distributed systems
   DEFAULT_BACKOFF_MULTIPLIER : constant := 2.0;
   
   --  Minimum backoff multiplier to prevent too aggressive retry storms  
   MIN_BACKOFF_MULTIPLIER : constant := 1.1;
   
   --  Maximum backoff multiplier to prevent runaway exponential growth
   MAX_BACKOFF_MULTIPLIER : constant := 10.0;
   
   --  ==========================================================================
   --  Jitter Algorithm Constants  
   --  ==========================================================================
   
   --  Decorrelated jitter multiplier for randomizing retry delays
   --  Value: 3.0 provides good distribution spread while maintaining backoff
   --  Based on AWS recommendations for decorrelated jitter algorithm
   DECORRELATED_JITTER_MULTIPLIER : constant := 3.0;
   
   --  Full jitter multiplier (not currently used but available)
   FULL_JITTER_MULTIPLIER : constant := 1.0;
   
   --  Equal jitter balance factor (50% deterministic, 50% random)
   EQUAL_JITTER_FACTOR : constant := 0.5;
   
   --  ==========================================================================
   --  Circuit Breaker Constants
   --  ==========================================================================
   
   --  Default failure threshold before opening circuit breaker
   --  Value: 5 consecutive failures is industry standard for most services
   --  Balances fault detection speed with false positive avoidance
   DEFAULT_CIRCUIT_BREAKER_THRESHOLD : constant := 5;
   
   --  Conservative threshold for critical services
   CONSERVATIVE_CIRCUIT_BREAKER_THRESHOLD : constant := 3;
   
   --  Aggressive threshold for non-critical services  
   AGGRESSIVE_CIRCUIT_BREAKER_THRESHOLD : constant := 10;
   
   --  Maximum reasonable threshold to prevent runaway failures
   MAX_CIRCUIT_BREAKER_THRESHOLD : constant := 50;
   
   --  ==========================================================================
   --  Timeout Constants
   --  ==========================================================================
   
   --  Default timeout multiplier for calculating request timeouts
   DEFAULT_TIMEOUT_MULTIPLIER : constant := 1.5;
   
   --  Circuit breaker half-open test request timeout multiplier
   HALF_OPEN_TIMEOUT_MULTIPLIER : constant := 2.0;
   
   --  ==========================================================================
   --  Rate Limiting Constants
   --  ==========================================================================
   
   --  Default rate limit burst capacity
   DEFAULT_RATE_LIMIT_BURST : constant := 100;
   
   --  Default rate limit refill rate (requests per second)
   DEFAULT_RATE_LIMIT_REFILL_RATE : constant := 10.0;
   
   --  ==========================================================================
   --  Retry Strategy Constants
   --  ==========================================================================
   
   --  Default maximum retry attempts for most operations
   DEFAULT_MAX_RETRY_ATTEMPTS : constant := 3;
   
   --  Conservative max retries for write operations
   CONSERVATIVE_MAX_RETRY_ATTEMPTS : constant := 2;
   
   --  Aggressive max retries for idempotent read operations  
   AGGRESSIVE_MAX_RETRY_ATTEMPTS : constant := 5;
   
   --  Maximum reasonable retry attempts to prevent infinite loops
   MAX_RETRY_ATTEMPTS_LIMIT : constant := 20;

end Abohlib.Infrastructure.Constants.Resilience;