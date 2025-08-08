--  =============================================================================
--  Abohlib.Core.Domain.Aggregates - DDD Aggregates Root Package
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Purpose:
--    Root package for Domain-Driven Design aggregates that define consistency
--    boundaries and encapsulate business invariants.
--
--  What Are Aggregates?
--    An aggregate is a cluster of domain objects that can be treated as a
--    single unit. An aggregate has:
--    - One root entity (the aggregate root)
--    - Clear boundaries defining what's inside
--    - Business invariants that must always be satisfied
--    - Controlled access through the aggregate root only
--
--  Key Concepts:
--    1. Aggregate Root: The only entry point to the aggregate
--    2. Consistency Boundary: All invariants enforced within the aggregate
--    3. Transactional Boundary: Save the entire aggregate as one unit
--    4. Identity: Each aggregate has a unique identifier
--
--  Design Guidelines:
--    - Keep aggregates small (prefer smaller aggregates over large ones)
--    - Reference other aggregates by ID only, not direct references
--    - Update only one aggregate per transaction
--    - Use domain events to maintain consistency across aggregates
--
--  Example Structure:
--    -- Order aggregate with Order as root and OrderItem as internal entity
--    type Order_Type is new Aggregate_Root with private;
--    type Order_Item_Type is tagged private;  -- Not accessible outside
--    
--    -- All operations go through the aggregate root
--    procedure Add_Item 
--      (Order    : in out Order_Type;
--       Product  : Product_Id_Type;
--       Quantity : Positive);
--
--  Child Packages:
--    - Generic_Aggregate_Root: Base implementation for aggregate roots
--
--  Common Patterns:
--    - Factory methods for complex creation logic
--    - Domain events for state changes
--    - Invariant checking in all mutating operations
--    - Encapsulation of all internal entities
--  =============================================================================

pragma Ada_2022;

package Abohlib.Core.Domain.Aggregates is
   pragma Pure;
end Abohlib.Core.Domain.Aggregates;
