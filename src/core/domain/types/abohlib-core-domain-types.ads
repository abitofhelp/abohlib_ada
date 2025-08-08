--  =============================================================================
--  Abohlib.Core.Domain.Types - Parent Package for Type Hierarchy
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Purpose:
--    Parent package for the domain types hierarchy. This package exists only
--    to satisfy Ada's package hierarchy requirements. All actual type
--    definitions are in the child packages.
--
--  Child Packages:
--    - Abohlib.Core.Domain.Types.Time       : Time-related types
--    - Abohlib.Core.Domain.Types.Bytes      : Byte and memory types
--    - Abohlib.Core.Domain.Types.Strings    : String length types
--    - Abohlib.Core.Domain.Types.Counts     : Count and index types
--    - Abohlib.Core.Domain.Types.Performance: Performance measurement types
--    - Abohlib.Core.Domain.Types.Resilience : Circuit breaker types
--    - Abohlib.Core.Domain.Types.Testing    : Testing framework types
--    - Abohlib.Core.Domain.Types.Formatting : Display and formatting types
--    - Abohlib.Core.Domain.Types.Utilities  : Type conversion utilities
--  =============================================================================

pragma Ada_2022;

package Abohlib.Core.Domain.Types is
   pragma Pure;
end Abohlib.Core.Domain.Types;