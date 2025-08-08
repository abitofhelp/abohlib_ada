--   =============================================================================
--   Abohlib.Core.Domain.Utilities - Domain Utilities Package
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--
--   Purpose:
--     Parent package for domain-level utility components that provide
--     foundational capabilities used throughout the domain layer.
--
--   Architecture:
--     This package serves as the organizational root for utility components
--     that implement cross-cutting domain concerns. All child packages follow
--     Domain-Driven Design principles and maintain domain purity.
--
--   Child Packages:
--     - Byte_Formatter: Format byte values for human-readable display
--     - File_Size_Formatter: Specialized formatting for file sizes
--     - Generic_Object_Pool: Thread-safe object pooling for resource management
--     - Safe_Call: Exception-safe function call wrappers
--     - State_Machine: Generic finite state machine implementation
--     - Concurrent.*: Thread-safe data structures and synchronization utilities
--     - IO.*: Input/output utilities for buffered and zero-copy operations
--     - System.*: System-level utilities like signal handling
--
--   Usage Guidelines:
--     1. Import specific child packages as needed:
--        with Abohlib.Core.Domain.Utilities.Byte_Formatter;
--     2. These utilities are designed to be used by domain entities and services
--     3. All utilities maintain domain purity (no infrastructure dependencies)
--     4. Thread safety is documented for each component
--
--   Design Principles:
--     - Zero dependencies on infrastructure layer
--     - Strong typing with Ada 2022 contracts
--     - Thread safety where applicable
--     - Performance-conscious implementations
--     - Comprehensive error handling via Result pattern
--   =============================================================================

pragma Ada_2022;

package Abohlib.Core.Domain.Utilities is
   pragma Pure;
end Abohlib.Core.Domain.Utilities;
