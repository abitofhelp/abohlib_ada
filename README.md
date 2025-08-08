# ABOHLIB - A Bit of Help Library

**Version 1.0.0**

A comprehensive Ada 2022 library providing type-safe, reusable components for building robust applications with modern architectural patterns.

ABOHLIB implements industry best practices through a hybrid architecture combining Domain-Driven Design (DDD), Clean Architecture, and Hexagonal Architecture principles. The library emphasizes compile-time safety, explicit error handling, and clear separation of concerns.

## 🚀 Quick Links

- **[Getting Started](docs/GETTING_STARTED.md)** - Installation and first steps
- **[Code Examples](docs/EXAMPLES.md)** - Complete, runnable examples
- **[API Documentation](docs/api/)** - Full API reference
- **[Architecture Overview](docs/SOFTWARE_DESIGN_DOCUMENT.md#2-architecture-overview)** - Design principles

## 📋 Key Features

### Type Safety and Error Handling
- **[Type-Safe IDs](docs/EXAMPLES.md#type-safe-ids)** - Prevent mixing entity identifiers at compile time. Each entity type has its own ID type that cannot be accidentally mixed with others.
- **[Result Pattern](docs/guides/RESULT_PATTERN_GUIDE.md)** - Functional error handling without exceptions. All errors are explicit in function signatures, making them impossible to ignore.
- **[Strong Domain Types](docs/guides/TYPE_CONVERSIONS_GUIDE.md)** - Prevent semantic errors with type safety. Use `Milliseconds_Type` instead of `Natural` to avoid unit confusion.

### Data Processing and Persistence
- **[Pipeline Processing](docs/EXAMPLES.md#5-pipeline-processing)** - Generic, composable data processing stages that automatically optimize for single or batch operations.
- **[Repository Pattern](docs/EXAMPLES.md#4-repository-pattern)** - Clean data access abstractions with ACID compliance and unit of work support.
- **[Event Sourcing](docs/EXAMPLES.md#event-sourcing)** - Capture all changes as domain events for complete audit trails and system reconstruction.

### Resilience and Concurrency
- **[Retry Handlers](docs/EXAMPLES.md#error-recovery)** - Configurable retry logic with exponential backoff and circuit breaker integration.
- **[Circuit Breakers](docs/guides/RESILIENCE_GUIDE.md)** - Prevent cascading failures in distributed systems.
- **Lock-Free Data Structures** - High-performance concurrent components including ring buffers and queues.

### Development Support
- **[Testing Framework](docs/TESTING_GUIDE.md)** - Result-based testing with Ada 2022 contracts, property-based testing, and comprehensive mocking.
- **[Value Objects](docs/EXAMPLES.md#value-objects)** - Constrained strings, file paths, and optional types with built-in validation.
- **[ULID Support](docs/guides/ULID_GUIDE.md)** - Universally Unique Lexicographically Sortable Identifiers for distributed systems.

## 🏗️ Architecture

ABOHLIB implements a hybrid architecture that separates concerns into distinct layers:

### Layer Structure
1. **Domain Layer** - Core business logic, entities, value objects, and domain services
2. **Application Layer** - Use cases, application services, and orchestration logic
3. **Infrastructure Layer** - External adapters, persistence, and technical implementations
4. **Presentation Layer** - API interfaces, CLI tools, and user-facing components

### Key Principles
- **Dependency Inversion** - High-level modules don't depend on low-level modules; both depend on abstractions
- **Explicit Contracts** - All module interfaces use Ada 2022 contracts for compile-time verification
- **No Hidden State** - All dependencies are explicit through constructor parameters
- **Testability First** - Every component is designed with testing in mind

See the [Architecture Diagrams](docs/diagrams/) for visual representations and the [Software Design Document](docs/SOFTWARE_DESIGN_DOCUMENT.md) for detailed explanations.

## 📚 Documentation

### Getting Started
- **[Quick Start Guide](docs/GETTING_STARTED.md)** - Installation, setup, and your first ABOHLIB program
- **[Code Examples](docs/EXAMPLES.md)** - Complete, runnable examples demonstrating all major features
- **[Architecture Overview](docs/diagrams/README.md)** - Visual diagrams explaining the library structure

### Core Documentation
- **[Software Requirements Specification](docs/SOFTWARE_REQUIREMENTS_SPECIFICATION.md)** - Detailed explanation of what the library provides and why
- **[Software Design Document](docs/SOFTWARE_DESIGN_DOCUMENT.md)** - Technical implementation details and design decisions
- **[Testing Guide](docs/TESTING_GUIDE.md)** - Comprehensive guide to testing with examples, patterns, and best practices

### Topic Guides
- **[Result Pattern Guide](docs/guides/RESULT_PATTERN_GUIDE.md)** - Master functional error handling without exceptions
- **[Type Conversions Guide](docs/guides/TYPE_CONVERSIONS_GUIDE.md)** - Safe conversions between strong domain types
- **[Math Module Guide](docs/guides/MATH_MODULE_GUIDE.md)** - Mathematical operations with overflow protection
- **[Performance Guide](docs/guides/PERFORMANCE_CALCULATIONS_GUIDE.md)** - Calculate throughput and optimize processing
- **[Resilience Guide](docs/guides/RESILIENCE_GUIDE.md)** - Build fault-tolerant systems with retry and circuit breaker patterns
- **[Porting Guide](docs/guides/PORTING.md)** - Implement patterns from Rust, Go, and other languages in Ada

## 💻 Installation

### Prerequisites
- Ada 2022 compiler (GNAT 2024 or later)
- Alire package manager ([alire.ada-lang.io](https://alire.ada-lang.io))
- Make (for build automation)

### Quick Start
```bash
# Create a new Alire project
alr init --bin my_project
cd my_project

# Add ABOHLIB as a dependency
alr with abohlib~1.0.0

# Build your project
alr build
```

### Using ABOHLIB in Your Code
```ada
-- Import Result pattern for error handling
with Abohlib.Core.Domain.Errors;
with Abohlib.Core.Domain.Result;

-- Import strong types for type safety
with Abohlib.Core.Domain.Types;

-- Import value objects
with Abohlib.Core.Domain.Value_Objects.Common_Ids;
```

See [Getting Started](docs/GETTING_STARTED.md) for comprehensive setup instructions and first examples.

## 🧪 Testing

ABOHLIB includes a comprehensive testing framework that follows the Result pattern:

```bash
# Run all tests with detailed output
make test

# Run specific test categories
make test-unit          # Fast, isolated unit tests
make test-integration   # System integration tests
make test-contract      # Ada 2022 contract verification
make test-property      # Property-based testing

# Run with coverage analysis
make test-coverage      # Generate coverage report

# Debug specific tests
make test-debug         # Run with debugging symbols
make test-verbose       # Detailed test output
```

### Writing Tests
Tests in ABOHLIB use the Result pattern for assertions:
```ada
procedure Test_Example (T : in out Test_Context) is
   Result : constant My_Result.Result := Function_Under_Test;
begin
   Assert_Ok (T, Result, "Expected successful result");
   Assert_Equals (T, Result.Get_Ok, Expected_Value);
end Test_Example;
```

See [Testing Guide](docs/TESTING_GUIDE.md) for comprehensive testing documentation.

## 🎯 Common Use Cases

### Building a Web Service
```ada
-- Use Result pattern for request handling
-- Strong types for validation
-- Repository pattern for data access
-- Pipeline processing for transformations
```

### Data Processing Pipeline
```ada
-- Generic pipeline stages for ETL
-- Batch processing with parallelization
-- Error aggregation and reporting
-- Progress tracking and cancellation
```

### Domain-Driven Design Application
```ada
-- Aggregate roots with domain events
-- Value objects with validation
-- Repository pattern with unit of work
-- Saga pattern for distributed transactions
```

## 🤝 Contributing

Contributions are welcome! Please see [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

### Development Setup
```bash
# Clone the repository
git clone https://github.com/abitofhelp/abohlib.git
cd abohlib

# Install dependencies
alr update

# Run tests to verify setup
make test

# Run style checks
make style-check
```

## 📄 License

MIT License - see [LICENSE.md](LICENSE.md) for details.

## 🔗 Related Projects

This library is part of a cross-language architecture research project exploring how modern architectural patterns can be implemented consistently across different programming paradigms:

- **Ada** - This repository (ABOHLIB) - Demonstrates strong typing and contract-based design
- **Rust** - [rustlib](https://github.com/abitofhelp/rustlib) (planned) - Will showcase ownership and lifetime patterns
- **Go** - [golib](https://github.com/abitofhelp/golib) (planned) - Will demonstrate simplicity and concurrency patterns

Each implementation maintains the same architectural principles while leveraging language-specific strengths.

---

Copyright (c) 2025 A Bit of Help, Inc. All rights reserved.