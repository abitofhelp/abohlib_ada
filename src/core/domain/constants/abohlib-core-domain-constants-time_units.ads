--  =============================================================================
--  Abohlib.Core.Domain.Constants.Time_Units - Time Unit Constants
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Purpose:
--    Provides time-related constants for consistent time calculations and
--    conversions throughout the library.
--
--  Constants:
--    Time conversion factors between different units (seconds, milliseconds,
--    microseconds, nanoseconds) and common time-related values.
--  =============================================================================

pragma Ada_2022;

with Abohlib.Core.Domain.Types.Time; use Abohlib.Core.Domain.Types.Time;

package Abohlib.Core.Domain.Constants.Time_Units is

   --  ==========================================================================
   --  Time Unit Conversion Factors
   --  ==========================================================================
   
   --  Milliseconds conversions
   Ms_Per_Second     : constant := 1_000.0;
   Ms_Per_Minute     : constant := 60_000.0;
   Ms_Per_Hour       : constant := 3_600_000.0;
   Ms_Per_Day        : constant := 86_400_000.0;
   
   --  Microseconds conversions
   Us_Per_Second     : constant := 1_000_000.0;
   Us_Per_Ms         : constant := 1_000.0;
   
   --  Nanoseconds conversions
   Ns_Per_Second     : constant := 1_000_000_000.0;
   Ns_Per_Ms         : constant := 1_000_000.0;
   Ns_Per_Us         : constant := 1_000.0;
   
   --  Seconds conversions
   Seconds_Per_Minute : constant := 60.0;
   Seconds_Per_Hour   : constant := 3_600.0;
   Seconds_Per_Day    : constant := 86_400.0;
   Seconds_Per_Week   : constant := 604_800.0;
   
   --  Minutes conversions
   Minutes_Per_Hour   : constant := 60.0;
   Minutes_Per_Day    : constant := 1_440.0;
   Minutes_Per_Week   : constant := 10_080.0;
   
   --  Hours conversions
   Hours_Per_Day      : constant := 24.0;
   Hours_Per_Week     : constant := 168.0;
   
   --  Days conversions
   Days_Per_Week      : constant := 7.0;
   Days_Per_Year      : constant := 365.0;
   Days_Per_Leap_Year : constant := 366.0;
   
   --  ==========================================================================
   --  Common Time Values
   --  ==========================================================================
   
   --  Zero durations
   Zero_Duration     : constant Duration := 0.0;
   
   --  Common durations in seconds
   One_Millisecond   : constant Duration := 0.001;
   One_Second        : constant Duration := 1.0;
   One_Minute        : constant Duration := 60.0;
   One_Hour          : constant Duration := 3_600.0;
   
   --  Timeout defaults (using strong types)
   Default_Timeout     : constant Timeout_Ms_Type := 30_000;   -- 30 seconds
   Short_Timeout       : constant Timeout_Ms_Type := 5_000;    -- 5 seconds
   Long_Timeout        : constant Timeout_Ms_Type := 300_000;  -- 5 minutes
   
   --  Retry delays (using strong types)
   Min_Retry_Delay     : constant Retry_Delay_Ms_Type := 100;      -- 100ms
   Default_Retry_Delay : constant Retry_Delay_Ms_Type := 1_000;    -- 1 second
   Max_Retry_Delay     : constant Retry_Delay_Ms_Type := 60_000;   -- 1 minute

end Abohlib.Core.Domain.Constants.Time_Units;