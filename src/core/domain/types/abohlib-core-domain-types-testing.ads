--  =============================================================================
--  Abohlib.Core.Domain.Types.Testing - Testing Framework Strong Types
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Purpose:
--    Provides strong types for testing frameworks including property-based
--    testing, mutation testing, and test execution metrics.
--
--  Design:
--    - Types for test counts and limits
--    - Property testing shrink counts
--    - Mutation testing constraints
--    - Random seed types for reproducibility
--  =============================================================================

pragma Ada_2022;

package Abohlib.Core.Domain.Types.Testing is

   --  ==========================================================================
   --  Test Execution Types
   --  ==========================================================================
   
   --  Test execution and property testing types
   type Test_Count_Type is new Natural
     with Predicate => Test_Count_Type > 0;
     
   type Shrink_Count_Type is new Natural;
     
   type Random_Seed_Type is new Natural;
   
   --  Test statistics types  
   type Test_Success_Count_Type is new Natural;
   type Test_Failure_Count_Type is new Natural;
   type Test_Skip_Count_Type is new Natural;
   
   --  ==========================================================================
   --  Mutation Testing Types
   --  ==========================================================================
   
   type Max_Mutations_Type is new Positive
     with Predicate => Max_Mutations_Type <= 1_000;  -- Reasonable limit

end Abohlib.Core.Domain.Types.Testing;