# Abohlib Source Code Component Index

This directory contains the complete source code for Abohlib, organized following Domain-Driven Design (DDD), Clean Architecture, and Hexagonal Architecture principles. The codebase implements a hybrid architecture with clear separation of concerns across architectural layers.

## 📋 Table of Contents

- [Architecture Overview](#🏗️-architecture-overview)
- [Root Package](#📦-root-package)
- [Core Layer](#🎯-core-layer)
  - [Domain Layer](#domain-layer)
  - [Application Layer](#application-layer)
- [Infrastructure Layer](#🔧-infrastructure-layer)
- [Presentation Layer](#🌐-presentation-layer)
- [Navigation Guide](#🧭-navigation-guide)

## 🏗️ Architecture Overview

The source code is organized following our hybrid architecture with strict layer boundaries:

- **Domain Layer**: Pure business logic with no external dependencies
- **Application Layer**: Use case orchestration and application services
- **Infrastructure Layer**: Technical implementations and external system adapters
- **Presentation Layer**: External interfaces (CLI, API endpoints)

**Dependency Rule:** Dependencies point inward. Outer layers depend on inner layers, never the reverse.

For detailed architecture diagrams and design rationale, see the [Software Design Document](../docs/SOFTWARE_DESIGN_DOCUMENT.md#2-1-hybrid-architecture-overview).

## 📦 Root Package

| Component | Description |
|-----------|-------------|
| [`abohlib.ads`](./abohlib.ads) | Main library package specification - entry point for all library functionality |

## 🎯 Core Layer

The innermost layer containing pure business logic with no external dependencies.

### [`core/`](./core/) - Core Business Logic

| Component | Description |
|-----------|-------------|
| [`abohlib-core.ads`](./core/abohlib-core.ads) | Core layer package specification |

### Domain Layer

Pure business logic and domain concepts with zero external dependencies.

#### [`core/domain/`](./core/domain/) - Domain Layer Root

| Component | Description |
|-----------|-------------|
| [`abohlib-core-domain.ads`](./core/domain/abohlib-core-domain.ads) | Domain layer package specification |

#### [`core/domain/aggregates/`](./core/domain/aggregates/) - Domain Aggregates

| Component | Description |
|-----------|-------------|
| [`abohlib-core-domain-aggregates.ads`](./core/domain/aggregates/abohlib-core-domain-aggregates.ads) | Aggregates package specification |
| [`abohlib-core-domain-aggregates-aggregate_root.ads`](./core/domain/aggregates/abohlib-core-domain-aggregates-aggregate_root.ads) | Generic aggregate root for DDD aggregates with event sourcing |
| [`abohlib-core-domain-aggregates-aggregate_root.adb`](./core/domain/aggregates/abohlib-core-domain-aggregates-aggregate_root.adb) | Aggregate root implementation with version control and event tracking |

#### [`core/domain/constants/`](./core/domain/constants/) - Domain Constants

| Component | Description |
|-----------|-------------|
| [`abohlib-core-domain-constants.ads`](./core/domain/constants/abohlib-core-domain-constants.ads) | Domain constants package specification |
| [`abohlib-core-domain-constants-bytes.ads`](./core/domain/constants/abohlib-core-domain-constants-bytes.ads) | Byte-related constants (KB, MB, GB, etc.) |
| [`abohlib-core-domain-constants-limits.ads`](./core/domain/constants/abohlib-core-domain-constants-limits.ads) | System and domain limits (max lengths, counts, etc.) |
| [`abohlib-core-domain-constants-time_units.ads`](./core/domain/constants/abohlib-core-domain-constants-time_units.ads) | Time unit conversions and common durations (Ms_Per_Second, timeout defaults, etc.) |

#### [`core/domain/errors/`](./core/domain/errors/) - Error Handling

| Component | Description |
|-----------|-------------|
| [`abohlib-core-domain-errors.ads`](./core/domain/errors/abohlib-core-domain-errors.ads) | Domain error types and Result pattern foundation |
| [`abohlib-core-domain-errors.adb`](./core/domain/errors/abohlib-core-domain-errors.adb) | Error constructors and string formatting implementation |
| [`abohlib-core-domain-result.ads`](./core/domain/errors/abohlib-core-domain-result.ads) | Generic Result pattern for functional error handling |
| [`abohlib-core-domain-result.adb`](./core/domain/errors/abohlib-core-domain-result.adb) | Result pattern implementation with monadic operations |

#### [`core/domain/events/`](./core/domain/events/) - Domain Events

| Component | Description |
|-----------|-------------|
| [`abohlib-core-domain-events.ads`](./core/domain/events/abohlib-core-domain-events.ads) | Domain event types and dispatcher interface |
| [`abohlib-core-domain-events.adb`](./core/domain/events/abohlib-core-domain-events.adb) | Event creation and basic event handling implementation |

#### [`core/domain/ports/`](./core/domain/ports/) - Domain Ports (Hexagonal Architecture)

| Component | Description |
|-----------|-------------|
| [`abohlib-core-domain-ports.ads`](./core/domain/ports/abohlib-core-domain-ports.ads) | Ports package specification |
| [`abohlib-core-domain-ports-file_system.ads`](./core/domain/ports/abohlib-core-domain-ports-file_system.ads) | File system abstraction interface |
| [`abohlib-core-domain-ports-file_system.adb`](./core/domain/ports/abohlib-core-domain-ports-file_system.adb) | File system port implementation |
| [`abohlib-core-domain-ports-system_info.ads`](./core/domain/ports/abohlib-core-domain-ports-system_info.ads) | System information abstraction interface |
| [`abohlib-core-domain-ports-system_info.adb`](./core/domain/ports/abohlib-core-domain-ports-system_info.adb) | System info port implementation |

#### [`core/domain/repositories/`](./core/domain/repositories/) - Repository Pattern

| Component | Description |
|-----------|-------------|
| [`abohlib-core-domain-repositories.ads`](./core/domain/repositories/abohlib-core-domain-repositories.ads) | Repositories package specification |
| [`abohlib-core-domain-repositories-generic_repository.ads`](./core/domain/repositories/abohlib-core-domain-repositories-generic_repository.ads) | Generic repository interface with CRUD operations |
| [`abohlib-core-domain-repositories-generic_repository.adb`](./core/domain/repositories/abohlib-core-domain-repositories-generic_repository.adb) | Base repository implementation |
| [`abohlib-core-domain-repositories-acid_repository.ads`](./core/domain/repositories/abohlib-core-domain-repositories-acid_repository.ads) | ACID-compliant repository with transaction support |
| [`abohlib-core-domain-repositories-acid_repository.adb`](./core/domain/repositories/abohlib-core-domain-repositories-acid_repository.adb) | ACID repository implementation with optimistic concurrency |
| [`abohlib-core-domain-repositories-unit_of_work.ads`](./core/domain/repositories/abohlib-core-domain-repositories-unit_of_work.ads) | Unit of Work pattern for transaction management |
| [`abohlib-core-domain-repositories-unit_of_work.adb`](./core/domain/repositories/abohlib-core-domain-repositories-unit_of_work.adb) | Unit of Work implementation |

#### [`core/domain/sagas/`](./core/domain/sagas/) - Saga Pattern

| Component | Description |
|-----------|-------------|
| [`abohlib-core-domain-sagas.ads`](./core/domain/sagas/abohlib-core-domain-sagas.ads) | Sagas package specification |
| [`abohlib-core-domain-sagas-saga_coordinator.ads`](./core/domain/sagas/abohlib-core-domain-sagas-saga_coordinator.ads) | Saga coordinator for distributed transactions |
| [`abohlib-core-domain-sagas-saga_coordinator.adb`](./core/domain/sagas/abohlib-core-domain-sagas-saga_coordinator.adb) | Saga execution and compensation logic |

#### [`core/domain/services/`](./core/domain/services/) - Domain Services

| Component | Description |
|-----------|-------------|
| [`abohlib-core-domain-services.ads`](./core/domain/services/abohlib-core-domain-services.ads) | Domain services package specification |
| [`abohlib-core-domain-services-file_path_service.ads`](./core/domain/services/abohlib-core-domain-services-file_path_service.ads) | File path validation and manipulation service |
| [`abohlib-core-domain-services-file_path_service.adb`](./core/domain/services/abohlib-core-domain-services-file_path_service.adb) | File path service implementation |
| [`abohlib-core-domain-services-generic_pipeline_stage.ads`](./core/domain/services/abohlib-core-domain-services-generic_pipeline_stage.ads) | Generic pipeline processing stage |
| [`abohlib-core-domain-services-generic_pipeline_stage.adb`](./core/domain/services/abohlib-core-domain-services-generic_pipeline_stage.adb) | Pipeline stage with batch processing and statistics |
| [`abohlib-core-domain-services-sha256_hasher.ads`](./core/domain/services/abohlib-core-domain-services-sha256_hasher.ads) | SHA256 hashing service interface (provides public SHA256_Hex_Length constant) |
| [`abohlib-core-domain-services-sha256_hasher.adb`](./core/domain/services/abohlib-core-domain-services-sha256_hasher.adb) | SHA256 hasher implementation |
| [`abohlib-core-domain-services-generic_sha256_hasher.ads`](./core/domain/services/abohlib-core-domain-services-generic_sha256_hasher.ads) | Generic SHA256 hasher for any data type (provides public SHA256_Hex_Length constant) |
| [`abohlib-core-domain-services-generic_sha256_hasher.adb`](./core/domain/services/abohlib-core-domain-services-generic_sha256_hasher.adb) | Generic hasher implementation |

#### [`core/domain/utilities/`](./core/domain/utilities/) - Domain Utilities

| Component | Description |
|-----------|-------------|
| [`abohlib-core-domain-utilities.ads`](./core/domain/utilities/abohlib-core-domain-utilities.ads) | Utilities package specification |
| [`abohlib-core-domain-utilities-byte_formatter.ads`](./core/domain/utilities/abohlib-core-domain-utilities-byte_formatter.ads) | Byte size formatting (B, KB, MB, GB) |
| [`abohlib-core-domain-utilities-byte_formatter.adb`](./core/domain/utilities/abohlib-core-domain-utilities-byte_formatter.adb) | Human-readable byte formatting implementation |
| [`abohlib-core-domain-utilities-system_info.ads`](./core/domain/utilities/abohlib-core-domain-utilities-system_info.ads) | System information utilities |
| [`abohlib-core-domain-utilities-system_info.adb`](./core/domain/utilities/abohlib-core-domain-utilities-system_info.adb) | System info utility implementation |
| [`abohlib-core-domain-utilities-ulid_helpers.ads`](./core/domain/utilities/abohlib-core-domain-utilities-ulid_helpers.ads) | ULID generation and validation helpers |
| [`abohlib-core-domain-utilities-ulid_helpers.adb`](./core/domain/utilities/abohlib-core-domain-utilities-ulid_helpers.adb) | Safe ULID operations with error handling |

#### [`core/domain/value_objects/`](./core/domain/value_objects/) - Value Objects

| Component | Description |
|-----------|-------------|
| [`abohlib-core-domain-value_objects.ads`](./core/domain/value_objects/abohlib-core-domain-value_objects.ads) | Value objects package specification |
| [`abohlib-core-domain-value_objects-common_ids.ads`](./core/domain/value_objects/abohlib-core-domain-value_objects-common_ids.ads) | Common ID types for entities |
| [`abohlib-core-domain-value_objects-constrained_strings.ads`](./core/domain/value_objects/abohlib-core-domain-value_objects-constrained_strings.ads) | String types with validation constraints |
| [`abohlib-core-domain-value_objects-constrained_strings.adb`](./core/domain/value_objects/abohlib-core-domain-value_objects-constrained_strings.adb) | Constrained string implementation |
| [`abohlib-core-domain-value_objects-file_path.ads`](./core/domain/value_objects/abohlib-core-domain-value_objects-file_path.ads) | Type-safe file path value object |
| [`abohlib-core-domain-value_objects-file_path.adb`](./core/domain/value_objects/abohlib-core-domain-value_objects-file_path.adb) | File path validation and manipulation |
| [`abohlib-core-domain-value_objects-file_path-results.ads`](./core/domain/value_objects/abohlib-core-domain-value_objects-file_path-results.ads) | Result types for file path operations |
| [`abohlib-core-domain-value_objects-file_path-results.adb`](./core/domain/value_objects/abohlib-core-domain-value_objects-file_path-results.adb) | File path result implementations |
| [`abohlib-core-domain-value_objects-generic_wrapper.ads`](./core/domain/value_objects/abohlib-core-domain-value_objects-generic_wrapper.ads) | Generic wrapper for creating value objects |
| [`abohlib-core-domain-value_objects-generic_wrapper.adb`](./core/domain/value_objects/abohlib-core-domain-value_objects-generic_wrapper.adb) | Value object wrapper implementation |
| [`abohlib-core-domain-value_objects-type_safe_generic_id.ads`](./core/domain/value_objects/abohlib-core-domain-value_objects-type_safe_generic_id.ads) | Type-safe generic ID using phantom types |
| [`abohlib-core-domain-value_objects-type_safe_generic_id.adb`](./core/domain/value_objects/abohlib-core-domain-value_objects-type_safe_generic_id.adb) | Type-safe ID implementation with ULID |

### Application Layer

Use case orchestration and application-specific logic.

#### [`core/application/`](./core/application/) - Application Layer Root

| Component | Description |
|-----------|-------------|
| [`abohlib-core-application.ads`](./core/application/abohlib-core-application.ads) | Application layer package specification |

#### [`core/application/constants/`](./core/application/constants/) - Application Constants

| Component | Description |
|-----------|-------------|
| [`abohlib-core-application-constants.ads`](./core/application/constants/abohlib-core-application-constants.ads) | Application-level constants and configuration |

#### [`core/application/errors/`](./core/application/errors/) - Application Errors

| Component | Description |
|-----------|-------------|
| [`abohlib-core-application-errors.ads`](./core/application/errors/abohlib-core-application-errors.ads) | Application layer error types |
| [`abohlib-core-application-errors.adb`](./core/application/errors/abohlib-core-application-errors.adb) | Use case and workflow error implementations |

#### [`core/application/dtos/`](./core/application/dtos/) - Data Transfer Objects

*Directory structure for DTOs (currently empty)*

#### [`core/application/services/`](./core/application/services/) - Application Services

*Directory structure for application services (currently empty)*

#### [`core/application/use_cases/`](./core/application/use_cases/) - Use Cases

*Directory structure for use case implementations (currently empty)*

## 🔧 Infrastructure Layer

Technical implementations and external integrations.

### [`infrastructure/`](./infrastructure/) - Infrastructure Layer Root

| Component | Description |
|-----------|-------------|
| [`abohlib-infrastructure.ads`](./infrastructure/abohlib-infrastructure.ads) | Infrastructure layer package specification |

#### [`infrastructure/adapters/`](./infrastructure/adapters/) - External Adapters

| Component | Description |
|-----------|-------------|
| [`abohlib-infrastructure-adapters.ads`](./infrastructure/adapters/abohlib-infrastructure-adapters.ads) | Adapters package specification |
| [`abohlib-infrastructure-adapters-posix_file_system.ads`](./infrastructure/adapters/abohlib-infrastructure-adapters-posix_file_system.ads) | POSIX file system adapter interface |
| [`abohlib-infrastructure-adapters-posix_file_system.adb`](./infrastructure/adapters/abohlib-infrastructure-adapters-posix_file_system.adb) | POSIX file system implementation |
| [`abohlib-infrastructure-adapters-posix_system_info.ads`](./infrastructure/adapters/abohlib-infrastructure-adapters-posix_system_info.ads) | POSIX system information adapter |
| [`abohlib-infrastructure-adapters-posix_system_info.adb`](./infrastructure/adapters/abohlib-infrastructure-adapters-posix_system_info.adb) | System info implementation |

#### [`infrastructure/constants/`](./infrastructure/constants/) - Infrastructure Constants

| Component | Description |
|-----------|-------------|
| [`abohlib-infrastructure-constants.ads`](./infrastructure/constants/abohlib-infrastructure-constants.ads) | Infrastructure layer constants |

#### [`infrastructure/errors/`](./infrastructure/errors/) - Infrastructure Errors

| Component | Description |
|-----------|-------------|
| [`abohlib-infrastructure-errors.ads`](./infrastructure/errors/abohlib-infrastructure-errors.ads) | Infrastructure-specific error types |
| [`abohlib-infrastructure-errors.adb`](./infrastructure/errors/abohlib-infrastructure-errors.adb) | Database, network, and file system error implementations |

#### [`infrastructure/logging/`](./infrastructure/logging/) - Logging

| Component | Description |
|-----------|-------------|
| [`abohlib-logging.ads`](./infrastructure/logging/abohlib-logging.ads) | Logging interface and types |
| [`abohlib-logging.adb`](./infrastructure/logging/abohlib-logging.adb) | Basic logging implementation |

#### [`infrastructure/resilience/`](./infrastructure/resilience/) - Resilience Patterns

| Component | Description |
|-----------|-------------|
| [`abohlib-infrastructure-resilience.ads`](./infrastructure/resilience/abohlib-infrastructure-resilience.ads) | Resilience package specification |
| [`abohlib-infrastructure-resilience-generic_retry_handler.ads`](./infrastructure/resilience/abohlib-infrastructure-resilience-generic_retry_handler.ads) | Generic retry handler with multiple strategies |
| [`abohlib-infrastructure-resilience-generic_retry_handler.adb`](./infrastructure/resilience/abohlib-infrastructure-resilience-generic_retry_handler.adb) | Retry implementation with exponential backoff |

#### [`infrastructure/testing/`](./infrastructure/testing/) - Testing Framework

| Component | Description |
|-----------|-------------|
| [`abohlib-infrastructure-testing.ads`](./infrastructure/testing/abohlib-infrastructure-testing.ads) | Testing package specification |
| [`abohlib-infrastructure-testing-test_framework.ads`](./infrastructure/testing/abohlib-infrastructure-testing-test_framework.ads) | Result-based testing framework interface |
| [`abohlib-infrastructure-testing-test_framework.adb`](./infrastructure/testing/abohlib-infrastructure-testing-test_framework.adb) | Test framework with assertions and reporting |
| [`abohlib-infrastructure-testing-console_output.ads`](./infrastructure/testing/abohlib-infrastructure-testing-console_output.ads) | Console output adapter for tests |
| [`abohlib-infrastructure-testing-console_output.adb`](./infrastructure/testing/abohlib-infrastructure-testing-console_output.adb) | Console output implementation |

#### Infrastructure Directories

| Directory | Description |
|-----------|-------------|
| [`infrastructure/config/`](./infrastructure/config/) | Configuration management (planned) |
| [`infrastructure/persistence/`](./infrastructure/persistence/) | Data persistence adapters (planned) |
| [`infrastructure/utilities/`](./infrastructure/utilities/) | Infrastructure utilities (planned) |

## 🌐 Presentation Layer

External interfaces and API adapters.

### [`presentation/`](./presentation/) - Presentation Layer Root

| Component | Description |
|-----------|-------------|
| [`abohlib-presentation.ads`](./presentation/abohlib-presentation.ads) | Presentation layer package specification |

#### [`presentation/constants/`](./presentation/constants/) - Presentation Constants

| Component | Description |
|-----------|-------------|
| [`abohlib-presentation-constants.ads`](./presentation/constants/abohlib-presentation-constants.ads) | Presentation layer constants |

#### [`presentation/errors/`](./presentation/errors/) - Presentation Errors

| Component | Description |
|-----------|-------------|
| [`abohlib-presentation-errors.ads`](./presentation/errors/abohlib-presentation-errors.ads) | Presentation layer error types |
| [`abohlib-presentation-errors.adb`](./presentation/errors/abohlib-presentation-errors.adb) | API and interface error implementations |

#### Presentation Directories

| Directory | Description |
|-----------|-------------|
| [`presentation/adapters/cli/`](./presentation/adapters/cli/) | Command-line interface adapters (planned) |
| [`presentation/adapters/graphql/`](./presentation/adapters/graphql/) | GraphQL API adapters (planned) |
| [`presentation/adapters/rest/`](./presentation/adapters/rest/) | REST API adapters (planned) |

## 🧭 Navigation Guide

### Finding Components by Purpose

**🔍 Looking for error handling?**
- Start with [`core/domain/errors/`](./core/domain/errors/) for the Result pattern
- Check [`core/application/errors/`](./core/application/errors/) for use case errors
- See [`infrastructure/errors/`](./infrastructure/errors/) for technical errors

**🔍 Looking for data validation?**
- Check [`core/domain/value_objects/`](./core/domain/value_objects/) for validated types
- See [`core/domain/services/file_path_service.ads`](./core/domain/services/abohlib-core-domain-services-file_path_service.ads) for path validation

**🔍 Looking for data persistence?**
- Start with [`core/domain/repositories/`](./core/domain/repositories/) for interfaces
- See [`core/domain/ports/`](./core/domain/ports/) for storage abstractions
- Check [`infrastructure/adapters/`](./infrastructure/adapters/) for implementations

**🔍 Looking for processing pipelines?**
- See [`core/domain/services/generic_pipeline_stage.ads`](./core/domain/services/abohlib-core-domain-services-generic_pipeline_stage.ads)

**🔍 Looking for testing utilities?**
- Check [`infrastructure/testing/`](./infrastructure/testing/) for the test framework

### Architecture Navigation Rules

1. **Dependency Direction**: Always follow dependencies inward (presentation → infrastructure → application → domain)
2. **Interface First**: Look for `.ads` files to understand public contracts
3. **Layer Boundaries**: Never import across layers except inward
4. **Ports and Adapters**: Domain defines ports (interfaces), infrastructure provides adapters (implementations)

---

**📝 Note**: This index reflects the current state of the codebase. Some directories marked as "planned" contain structure but no implementation files yet.

**🔄 Last Updated**: August 2, 2025
**📊 Total Components**: 80+ files across 4 architectural layers
**✅ Test Coverage**: 58 comprehensive test cases
