--  =============================================================================
--  Abohlib.Core.Domain.Services.Pipeline - Pipeline Processing Framework
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Purpose:
--    Parent package for the unified pipeline processing framework. This package
--    consolidates pipeline stage implementations from abohlib, pipelib, and
--    pipeline projects into a single, reusable component.
--
--  Design:
--    - Follows hybrid DDD/Clean/Hexagonal architecture
--    - Uses Result pattern for all error handling
--    - Supports both interface and implementation approaches
--    - Leverages Ada 2022 features with comprehensive contracts
--
--  Child Packages:
--    - Generic_Stage     : Core generic pipeline stage implementation
--    - Interfaces        : Interface types for maximum flexibility
--    - Builder           : Pipeline composition using builder pattern
--    - Specialized       : Domain-specific pipeline stages
--  =============================================================================

pragma Ada_2022;

package Abohlib.Core.Domain.Services.Pipeline is
   pragma Preelaborate;
end Abohlib.Core.Domain.Services.Pipeline;