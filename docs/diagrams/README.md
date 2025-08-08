# Abohlib Architecture Diagrams

This directory contains UML diagrams that visualize the architecture and design patterns used in the abohlib library. These diagrams help developers understand the structure, relationships, and workflows within the codebase.

## Diagram Index

### Architecture Overview Diagrams

#### 1. **architecture_overview.svg**
- **Purpose**: High-level view of the hybrid architecture (DDD + Clean + Hexagonal)
- **Key Concepts**: Layer dependencies, ports and adapters, dependency rule
- **When to Reference**: Understanding overall system architecture and layer boundaries

#### 2. **package_hierarchy_overview.svg**
- **Purpose**: Complete package structure and organization
- **Key Concepts**: Package relationships, module organization, component locations
- **When to Reference**: Finding where specific functionality lives in the codebase

### Design Pattern Diagrams

#### 3. **result_pattern_class.svg**
- **Purpose**: Result<T,E> pattern for error handling without exceptions
- **Key Concepts**: Type-safe error handling, Result variants, common instantiations
- **When to Reference**: Implementing error handling or understanding return types

#### 4. **result_pattern_sequence.svg**
- **Purpose**: How Result pattern is used in practice
- **Key Concepts**: Error propagation, pattern matching, composition
- **When to Reference**: Understanding error flow through the system

#### 5. **repository_pattern_class.svg**
- **Purpose**: Repository pattern for data access abstraction
- **Key Concepts**: Port/adapter separation, ACID properties, unit of work
- **When to Reference**: Implementing data persistence or understanding data access

### Component Design Diagrams

#### 6. **pipeline_stage_class.svg**
- **Purpose**: Generic pipeline stage with adaptive parallel processing
- **Key Concepts**: Stage interface, batch processing, automatic optimization
- **When to Reference**: Building data processing pipelines or understanding stage behavior

#### 7. **state_machine_sequence.svg**
- **Purpose**: State machine transition flow with validation
- **Key Concepts**: State transitions, validation hooks, history tracking
- **When to Reference**: Implementing state machines or understanding state management

#### 8. **strong_types_class.svg**
- **Purpose**: Domain-specific type system for compile-time safety
- **Key Concepts**: Type safety, unit prevention, explicit conversions
- **When to Reference**: Understanding type system or adding new domain types

### Infrastructure Diagrams

#### 9. **retry_handler_activity.svg**
- **Purpose**: Retry logic flow with circuit breaker and backoff strategies
- **Key Concepts**: Retry strategies, jitter, circuit breaker integration
- **When to Reference**: Implementing resilient operations or configuring retries

#### 10. **test_framework_sequence.svg**
- **Purpose**: Test execution flow using Result-based assertions
- **Key Concepts**: Result-based testing, assertion flow, suite execution
- **When to Reference**: Writing tests or understanding test framework

#### 11. **concurrent_components_class.svg**
- **Purpose**: Thread-safe components and data structures
- **Key Concepts**: Lock-free structures, protected objects, task management
- **When to Reference**: Building concurrent systems or using thread-safe components

### Workflow Diagrams

#### 12. **pipeline_processing_activity.svg**
- **Purpose**: Data processing workflow through pipeline stages
- **Key Concepts**: Stage orchestration, error handling, parallel execution
- **When to Reference**: Understanding data flow through pipelines

#### 13. **event_sourcing_sequence.svg**
- **Purpose**: Event sourcing pattern implementation
- **Key Concepts**: Event storage, replay, projection building
- **When to Reference**: Implementing event-driven features

#### 14. **saga_pattern_state.svg**
- **Purpose**: Distributed transaction coordination using sagas
- **Key Concepts**: Compensation logic, state management, failure handling
- **When to Reference**: Implementing distributed transactions

#### 15. **domain_model.svg**
- **Purpose**: Core domain model relationships
- **Key Concepts**: Entities, value objects, aggregates, domain services
- **When to Reference**: Understanding business logic organization

## Viewing Diagrams

The diagrams are in SVG format and can be viewed:
1. **In Browser**: Click on any .svg file in GitHub
2. **In IDE**: Most modern IDEs support SVG preview
3. **In Documentation**: Reference them in markdown: `![Diagram](./diagram_name.svg)`

## Modifying Diagrams

Diagrams are created using PlantUML:
1. Edit the corresponding `.puml` file
2. Run `./regenerate_diagrams.sh` to regenerate all SVGs
3. Or regenerate individually: `plantuml -tsvg diagram_name.puml`

## Diagram Conventions

- **Colors**:
  - Blue shades: Domain/Core components
  - Orange shades: Application layer
  - Green shades: Infrastructure layer
  - Purple shades: Presentation layer
  
- **Shapes**:
  - Rectangles: Classes/Types
  - Rounded rectangles: Interfaces/Ports
  - Diamonds: Decision points
  - Ovals: Start/End states

- **Arrows**:
  - Solid arrows: Direct dependencies
  - Dashed arrows: Implements/Realizes
  - Dotted arrows: Uses/Depends on

## Adding New Diagrams

When adding new diagrams:
1. Use lowercase with underscores for filenames
2. Follow existing PlantUML style and theme
3. Include clear titles and notes
4. Update this README with the new diagram
5. Ensure both .puml and .svg files are committed

## Benefits for Developers

These diagrams help developers:
- **Understand** the system architecture quickly
- **Navigate** the codebase more effectively  
- **Design** new features following established patterns
- **Debug** by understanding component interactions
- **Onboard** new team members faster
- **Document** architectural decisions visually