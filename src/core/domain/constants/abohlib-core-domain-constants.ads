--  =============================================================================
--  Abohlib.Constants - Parent Package for Library Constants
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Purpose:
--    This package serves as the parent for all constant definitions used
--    throughout the Abohlib library. Child packages organize constants by
--    category for better maintainability and clarity.
--
--  Usage:
--    This is a pure package that contains no declarations itself. Child
--    packages provide specific constant definitions organized by domain.
--
--  Child Packages:
--    - Abohlib.Core.Domain.Constants.Bytes: Byte size constants (SI and IEC standards)
--    - Abohlib.Core.Domain.Constants.System: Hardware and system constants (cache lines, pages, etc.)
--    - Abohlib.Core.Domain.Constants.Limits: Domain-specific business rule limits
--
--  Note:
--    These constants are kept in the domain layer since they are needed
--    by domain value objects (like file sizes, chunk sizes). Outer layers
--    can access these constants following the dependency rule.
--  =============================================================================

pragma Ada_2022;

package Abohlib.Core.Domain.Constants is
   pragma Pure;

end Abohlib.Core.Domain.Constants;
