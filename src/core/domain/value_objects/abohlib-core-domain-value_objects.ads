--  =============================================================================
--  Abohlib.Core.Domain.Value_Objects - Value Objects Parent Package
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Purpose:
--    Parent package for all domain value objects. Value objects are immutable
--    objects that are defined by their attributes rather than identity.
--
--  Design Principles:
--    - Immutability: Once created, value objects cannot be changed
--    - Equality by value: Two value objects with same attributes are equal
--    - No identity: Value objects don't have unique identifiers
--    - Self-validation: Value objects validate their invariants
--  =============================================================================

pragma Ada_2022;

package Abohlib.Core.Domain.Value_Objects is
   pragma Pure;

end Abohlib.Core.Domain.Value_Objects;
