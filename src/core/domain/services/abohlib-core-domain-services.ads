--   =============================================================================
--   Abohlib.Core.Domain.Services - Domain Services Package
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--
--   Purpose:
--     Parent package for domain services that encapsulate business logic
--     spanning multiple entities or requiring complex coordination.
--
--   What Are Domain Services?
--     Domain services contain business logic that doesn't naturally fit within
--     a single entity or value object. They represent operations or workflows
--     that involve multiple domain objects working together.
--
--   When to Use Domain Services:
--     - Operations that involve multiple aggregates
--     - Complex business rules that span entities
--     - Calculations requiring data from multiple sources
--     - Orchestration of domain objects to fulfill use cases
--
--   Child Packages:
--     - File_Path_Service: Operations on file paths (validation, manipulation)
--     - Pipeline.*: Data processing pipeline with stages and transformations
--
--   Design Guidelines:
--     1. Domain services should be stateless
--     2. Operations should be expressed in ubiquitous language
--     3. Services should not depend on infrastructure
--     4. Use dependency injection for required repositories
--
--   Example Pattern:
--     -- Domain service for complex order pricing
--     type Order_Pricing_Service is tagged private;
--     
--     function Calculate_Total_Price
--       (Service : Order_Pricing_Service;
--        Order   : Order_Type;
--        Customer: Customer_Type) return Money_Type;
--
--   Key Principles:
--     - Stateless operations (no instance variables for business data)
--     - Pure domain logic (no infrastructure concerns)
--     - Clear, intention-revealing interfaces
--     - Testable without infrastructure dependencies
--   =============================================================================

pragma Ada_2022;

package Abohlib.Core.Domain.Services is
   pragma Pure;
end Abohlib.Core.Domain.Services;
