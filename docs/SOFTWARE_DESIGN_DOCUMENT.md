# Software Design Document (SDD)
## Abohlib - Ada 2022 Package of Reusable Components

**Version:** 1.0.0
**Date:** January 2025
**Classification:** Open Source Library (MIT License)
**Status:** Production Release

---

## Table of Contents

1. [Introduction](#1-introduction)
2. [Architectural Design](#2-architectural-design)
3. [System Components](#3-system-components)
4. [Data Design](#4-data-design)
5. [Interface Design](#5-interface-design)
6. [Component Design](#6-component-design)
   - 6.1 [Generic Pipeline Stage](#61-generic-pipeline-stage)
   - 6.2 [Contract Specifications](#62-contract-specifications)
   - 6.3 [Core Domain Modules](#63-core-domain-modules)
7. [Deployment Design](#7-deployment-design)
8. [Quality Attributes](#8-quality-attributes)

---

## 1. Introduction

### 1.1 Purpose

This Software Design Document (SDD) provides the detailed technical design of Abohlib, explaining how the library achieves its goals of type safety, explicit error handling, and architectural clarity.

The document serves multiple purposes:
- **Implementation Guide**: Details how each component is structured and why
- **Architecture Reference**: Explains the layered design and dependency rules
- **Pattern Catalog**: Documents reusable patterns with concrete examples
- **Decision Record**: Captures design trade-offs and rationale

Abohlib combines Domain-Driven Design (DDD), Clean Architecture, and Hexagonal Architecture principles, adapted specifically for Ada 2022's strengths in type safety and contract-based programming.

### 1.2 Scope

This document covers:
- Detailed architectural design decisions
- Component interfaces and relationships
- Implementation strategies for core patterns
- Design rationale and trade-offs
- Quality attribute implementations

### 1.3 Design Principles

**Core Design Principles:**

1. **Dependency Inversion** 
   - All dependencies point inward toward the domain layer
   - High-level modules never depend on low-level modules
   - Both depend on abstractions (Ada interfaces or generic formal parameters)

2. **Type Safety First**
   - Strong types prevent semantic errors at compile time
   - Each domain concept gets its own type (no primitive obsession)
   - Type conversions are explicit and validated

3. **Explicit Error Handling**
   - Result pattern makes all errors visible in function signatures
   - No exceptions cross architectural boundaries
   - Errors are values that can be transformed and combined

4. **Generic Programming**
   - Components are parameterized for maximum reuse
   - Generic contracts ensure type safety
   - Zero runtime overhead through instantiation

5. **Minimal Dependencies**
   - Core domain has zero external dependencies
   - Infrastructure adapters isolate external libraries
   - Each layer depends only on what it needs

**Supporting Principles:**

1. **Ada 2022 Contracts**
   - Preconditions, postconditions, and invariants document behavior
   - Contracts are executable specifications
   - Runtime checking during development, removed in production

2. **Immutability by Default**
   - Value objects are immutable after creation
   - State changes create new values
   - Reduces complexity and enables parallelism

3. **Composition Over Inheritance**
   - Favor generic instantiation and aggregation
   - Interfaces define contracts, not implementation
   - Enables flexible, testable designs

---

## 2. Architectural Design

### 2.1 Hybrid Architecture Overview

Abohlib implements a hybrid architecture combining three proven patterns:

```
┌─────────────────────────────────────────────────────────────┐
│                 Presentation Layer                           │
│              (External Interfaces)                          │
├─────────────────────────────────────────────────────────────┤
│                Infrastructure Layer                          │
│            (Technical Implementations)                      │
├─────────────────────────────────────────────────────────────┤
│                Application Layer                             │
│              (Use Case Orchestration)                       │
├─────────────────────────────────────────────────────────────┤
│                  Domain Layer                                │
│               (Pure Business Logic)                         │
└─────────────────────────────────────────────────────────────┘
```

### 2.2 Architectural Patterns in Practice

**Domain-Driven Design (DDD) Implementation:**

1. **Rich Domain Models**
   ```ada
   -- Entities have identity and behavior
   type Order is new Aggregate_Root with record
      ID : Order_ID;
      Items : Order_Item_List;
      Status : Order_Status;
   end record;
   
   -- Business logic lives in the domain
   procedure Add_Item (O : in out Order; Item : Order_Item)
     with Pre => O.Status = Draft,
          Post => O.Total >= O.Total'Old;
   ```

2. **Ubiquitous Language**
   - Code uses business terminology consistently
   - Types and operations reflect domain concepts
   - No technical jargon in domain layer

3. **Aggregates and Consistency**
   - Aggregates maintain invariants
   - All changes go through aggregate root
   - Transactions scope to single aggregate

**Clean Architecture Implementation:**

1. **Dependency Rule**
   ```
   Domain ← Application ← Infrastructure ← Presentation
   ```
   - Inner layers know nothing of outer layers
   - Abstractions defined in domain, implemented outside

2. **Use Case Focus**
   ```ada
   -- Application layer orchestrates domain objects
   function Process_Order (Repo : Repository'Class; 
                          Order_ID : Order_ID_Type) 
                          return Process_Result.Result;
   ```

3. **Framework Independence**
   - No web framework code in domain or application
   - Database specifics isolated in infrastructure
   - UI concerns stay in presentation layer

**Hexagonal Architecture Implementation:**

1. **Ports (Interfaces)**
   ```ada
   -- Domain defines what it needs
   type File_System_Port is interface;
   function Read_File (FS : File_System_Port; 
                      Path : String) 
                      return File_Result.Result is abstract;
   ```

2. **Adapters (Implementations)**
   ```ada
   -- Infrastructure provides implementations
   type POSIX_File_System is new File_System_Port with null record;
   overriding function Read_File (FS : POSIX_File_System;
                                 Path : String)
                                 return File_Result.Result;
   ```

3. **Testability**
   - Mock adapters for testing
   - In-memory repositories for fast tests
   - No need for real infrastructure in unit tests

### 2.3 Package Structure Design

```ada
-- Root package
package Abohlib is
   pragma Pure;
end Abohlib;

-- Core business logic (innermost layer)
package Abohlib.Core is
   pragma Preelaborate;
end Abohlib.Core;

-- Domain layer - pure business logic
package Abohlib.Core.Domain is
   pragma Preelaborate;
end Abohlib.Core.Domain;

-- Application layer - use case orchestration
package Abohlib.Core.Application is
   pragma Preelaborate;
end Abohlib.Core.Application;

-- Infrastructure layer - technical implementations
package Abohlib.Infrastructure is
   pragma Preelaborate;
end Abohlib.Infrastructure;

-- Presentation layer - external interfaces
package Abohlib.Presentation is
   pragma Preelaborate;
end Abohlib.Presentation;
```

---

## 3. System Components

### 3.1 Core Domain Components

#### 3.1.1 Result Pattern Implementation

**Design Decision:** Functional error handling without exceptions

```ada
generic
   type Ok_Type is private;
   type Err_Type is private;
package Result_Package is

   type Result is tagged private;

   function Ok (Value : Ok_Type) return Result;
   function Err (Error : Err_Type) return Result;

   function Is_Ok (Self : Result) return Boolean;
   function Is_Err (Self : Result) return Boolean;

   function Get_Ok (Self : Result) return Ok_Type
     with Pre => Self.Is_Ok;
   function Get_Err (Self : Result) return Err_Type
     with Pre => Self.Is_Err;

private
   type Result is tagged record
      Is_Success : Boolean := False;
      Ok_Value   : Ok_Type;
      Err_Value  : Err_Type;
   end record;
end Result_Package;
```

**Rationale:**
- Explicit error handling prevents hidden failures
- Composable through map/bind operations
- Type-safe error propagation
- No performance overhead from exception unwinding

**Functional Operations (New in v1.4):**

The Result pattern now includes functional combinators for composing operations:

```ada
-- Result.Functional package provides these generic operations
generic
   with package Source_Result is new Result_Package (<>);
   with package Target_Result is new Result_Package 
      (Ok_Type => <>, Err_Type => Source_Result.Err_Type);
   with function Transform (Value : Source_Result.Ok_Type) 
      return Target_Result.Ok_Type;
function Map (R : Source_Result.Result) return Target_Result.Result;

-- Transform error values
generic
   with package Source_Result is new Result_Package (<>);
   with package Target_Result is new Result_Package 
      (Ok_Type => Source_Result.Ok_Type, Err_Type => <>);
   with function Transform (Error : Source_Result.Err_Type)
      return Target_Result.Err_Type;
function Map_Err (R : Source_Result.Result) return Target_Result.Result;

-- Chain operations that return Results (flatMap/bind)
generic
   with package Source_Result is new Result_Package (<>);
   with package Target_Result is new Result_Package 
      (Ok_Type => <>, Err_Type => Source_Result.Err_Type);
   with function Operation (Value : Source_Result.Ok_Type)
      return Target_Result.Result;
function And_Then (R : Source_Result.Result) return Target_Result.Result;

-- Provide alternative on error
generic
   with package Result_Pkg is new Result_Package (<>);
   with function Alternative (Error : Result_Pkg.Err_Type)
      return Result_Pkg.Result;
function Or_Else (R : Result_Pkg.Result) return Result_Pkg.Result;
```

These operations enable functional composition of error-handling code without nested if-statements.

#### 3.1.2 Type-Safe Generic IDs

**Design Decision:** Phantom types for compile-time ID safety

```ada
generic
   type Category_Type is limited private;
   Category_Name : String;
   Prefix : String := "";
   with function "=" (Left, Right : Category_Type) return Boolean is <>;
package Generic_ID_Type is

   type ID is new String (1 .. 26);  -- ULID format

   function New_ID return ID;
   function To_String (Self : ID) return String;
   function From_String (S : String) return ID
     with Pre => S'Length = 26;

   function Is_Valid (Self : ID) return Boolean;
   
   -- Hash support for Ada.Containers (New in v1.4)
   function Hash (Self : ID) return Ada.Containers.Hash_Type;

private
   -- Implementation uses ULID library
end Generic_ID_Type;
```

**Rationale:**
- Phantom types prevent mixing different entity IDs
- Zero runtime overhead
- ULID provides lexicographic sorting and embedded timestamps
- Human-readable with optional prefixes

**Container Support (New in v1.4):**

The Generic_ID now includes a Hash function implementing the DJB2 algorithm, enabling IDs to be used as keys in Ada.Containers.Hashed_Maps and Hashed_Sets:

```ada
-- Example usage with containers
package User_Maps is new Ada.Containers.Hashed_Maps
  (Key_Type        => User_ID.ID,
   Element_Type    => User_Record,
   Hash            => User_ID.Hash,
   Equivalent_Keys => User_ID."=");
```

#### 3.1.3 Generic Repository Pattern

**Design Decision:** Collection-like interface with Result types

```ada
generic
   type Entity_Type is tagged private;
   type Id_Type is private;
   with function Get_Id (Entity : Entity_Type) return Id_Type;
   with function "=" (Left, Right : Id_Type) return Boolean is <>;
package Generic_Repository is

   package Save_Result is new Result_Package (Id_Type, Unbounded_String);
   package Find_Result is new Result_Package (Entity_Type, Unbounded_String);
   package List_Result is new Result_Package (Entity_Array, Unbounded_String);

   type Repository_Interface is limited interface;

   function Save (Self : Repository_Interface; Entity : Entity_Type)
     return Save_Result.Result is abstract;

   function Find_By_Id (Self : Repository_Interface; ID : Id_Type)
     return Find_Result.Result is abstract;

   function Find_All (Self : Repository_Interface)
     return List_Result.Result is abstract;

   function Delete (Self : Repository_Interface; ID : Id_Type)
     return Boolean_Result.Result is abstract;
end Generic_Repository;
```

**Rationale:**
- Generic over entity and ID types for reusability
- Interface-based for dependency inversion
- Result types for explicit error handling
- Collection-like semantics familiar to developers

### 3.2 Infrastructure Components

#### 3.2.1 Testing Framework Design

**Design Decision:** Result-based assertions without exceptions

```ada
package Test_Framework is

   type Test_Status is (Passed, Failed, Skipped);

   type Test_Result is record
      Name           : Unbounded_String;
      Status         : Test_Status;
      Message        : Unbounded_String;
      Elapsed_Time   : Duration;
      Line_Number    : Natural;
      Correlation_ID : Unbounded_String;
   end record;

   type Test_Function_Access is access function return Void_Result.Result;

   function Run_Test (Name : String; Test_Func : Test_Function_Access;
                     Output : access Test_Output_Port'Class)
     return Test_Result_Pkg.Result;

   function Assert_Equal (Expected, Actual : String; Message : String; Line : Natural)
     return Void_Result.Result;

   -- Additional assertion functions...
end Test_Framework;
```

**Rationale:**
- Result-based assertions prevent test framework exceptions
- Comprehensive test metadata collection
- Pluggable output through port pattern
- Performance timing built-in

#### 3.2.2 POSIX File System Adapter

**Design Decision:** Port/adapter pattern for file system operations

```ada
package POSIX_File_System is

   type POSIX_File_System_Provider is limited new File_System_Port with null record;

   overriding
   function Exists (Self : POSIX_File_System_Provider; Path : String)
     return Boolean_Result.Result;

   overriding
   function Read_File (Self : POSIX_File_System_Provider; Path : String)
     return String_Result.Result;

   overriding
   function Write_File (Self : POSIX_File_System_Provider; Path : String; Content : String)
     return Void_Result.Result;

   -- Additional file operations...
end POSIX_File_System;
```

**Rationale:**
- Implements domain port for dependency inversion
- POSIX compliance for portability
- Result types for explicit error handling
- Testable through mock implementations

---

## 4. Data Design

### 4.1 Domain Data Structures

#### 4.1.1 Strong Types Design

**Purpose:** Prevent mixing of semantically different values that share the same underlying representation (e.g., mixing timeouts with delays, or SI bytes with IEC bytes).

**Core Strong Types:**
```ada
-- Time-related strong types prevent mixing different time concepts
type Milliseconds_Type is new Natural;
type Timeout_Ms_Type is new Milliseconds_Type;
type Retry_Delay_Ms_Type is new Delay_Ms_Type;

-- Byte unit strong types prevent mixing decimal vs binary units
type Bytes_Type is new Long_Long_Integer with Dynamic_Predicate => Bytes_Type >= 0;
type SI_Bytes_Type is new Bytes_Type;   -- Decimal (1000-based)
type IEC_Bytes_Type is new Bytes_Type;  -- Binary (1024-based)

-- Performance measurement types
type Throughput_Type is new Bytes_Per_Second_Type;
type Processing_Speed_Type is new Float range 0.0 .. Float'Last;

-- Count types distinguish between counting items vs indexing
type Element_Count_Type is new Natural;
type Array_Index_Type is new Positive;
type Retry_Count_Type is new Positive;
```

**Design Benefits:**
- **Compile-time Safety**: Cannot accidentally mix timeout with delay values
- **Self-Documenting**: Function signatures clearly indicate expected value types
- **Zero Runtime Cost**: Strong types compile away to underlying base types
- **Conversion Control**: Explicit conversion functions prevent accidental mixing

**Usage Example:**
```ada
-- This is compile-time safe - cannot mix timeout and delay types
Timeout : constant Timeout_Ms_Type := 30_000;  -- 30 seconds
Delay   : constant Retry_Delay_Ms_Type := 1_000; -- 1 second
-- Timeout := Delay;  -- Compilation error!

-- Performance calculations with type safety
function Calculate_Throughput 
  (Bytes : SI_Bytes_Type; Time_Sec : Seconds_Type) return Throughput_Type;
```

#### 4.1.2 Value Objects Design

**File_Path Value Object:**
```ada
type File_Path is tagged record
   Path : Bounded_String (1 .. 4096);
   File_Type : File_Type_Enum;
end record
  with Type_Invariant => Is_Valid_Path (File_Path);

function Create (Path : String; FType : File_Type_Enum) return File_Path
  with Pre => Path'Length > 0 and Path'Length <= 4096;
```

**Design Principles:**
- Immutable after creation
- Self-validating with type invariants
- Bounded types for predictable memory usage
- Factory functions enforce validity

#### 4.1.2 Aggregate Data Design

**Generic Aggregate Root:**
```ada
type Aggregate_Root_Type is tagged record
   Id : Id_Type;
   Version : Natural := 0;
   Uncommitted_Events : Event_List;
   Created_At : Ada.Calendar.Time;
   Updated_At : Ada.Calendar.Time;
end record;
```

**Design Principles:**
- Version field for optimistic concurrency control
- Event list for event sourcing capability
- Timestamps for audit trails
- Generic over ID type for reusability

### 4.2 Error Data Design

#### 4.2.1 Domain Error Hierarchy

```ada
type Domain_Error is abstract tagged record
   Error_ID : ULID.ULID_Number;
   Message : Unbounded_String;
   Timestamp : Ada.Calendar.Time;
   Context : Unbounded_String;
end record;

type Validation_Error is new Domain_Error with record
   Kind : Validation_Error_Kind;
   Field_Name : Unbounded_String;
   Invalid_Value : Unbounded_String;
end record;

type Business_Rule_Error is new Domain_Error with record
   Kind : Business_Rule_Kind;
   Rule_Name : Unbounded_String;
   Rule_Context : Unbounded_String;
end record;
```

**Design Principles:**
- Hierarchical error types for categorization
- Rich context information for debugging
- Unique error IDs for tracking
- Timestamp for audit trails

---

## 5. Interface Design

### 5.1 Port Interfaces (Domain)

#### 5.1.1 File System Port

```ada
package File_System is

   type File_System_Port is limited interface;

   function Exists (Self : File_System_Port; Path : String)
     return Boolean_Result.Result is abstract;

   function Read_File (Self : File_System_Port; Path : String)
     return String_Result.Result is abstract;

   function Write_File (Self : File_System_Port; Path : String; Content : String)
     return Void_Result.Result is abstract;

   function Get_File_Size (Self : File_System_Port; Path : String)
     return Natural_Result.Result is abstract;
end File_System;
```

#### 5.1.2 System Info Port

```ada
package System_Info is

   type System_Info_Port is limited interface;

   function Get_Environment_Variable (Self : System_Info_Port; Name : String)
     return String_Result.Result is abstract;

   function Get_Current_Directory (Self : System_Info_Port)
     return String_Result.Result is abstract;

   function Get_System_Time (Self : System_Info_Port)
     return Time_Result.Result is abstract;
end System_Info;
```

### 5.2 API Design Principles

**Consistency:**
- All fallible operations return Result types
- Naming follows Ada conventions (Get_, Set_, Is_, Has_)
- Parameters follow in/in out/out conventions
- Generic parameters consistently named

**Usability:**
- Factory functions for complex object creation
- Sensible defaults where appropriate
- Clear parameter names and documentation
- Pre/post conditions for contracts

---

## 6. Component Design

### 6.1 Generic Pipeline Stage

#### 6.1.1 Design Overview

```ada
generic
   type Input_Type is private;
   type Output_Type is private;
   type State_Type is private;
   type Config_Type is private;
   Stage_Name : String;
   with function Process_Element (State : in out State_Type;
                                 Config : Config_Type;
                                 Input : Input_Type) return Output_Type;
   with function Initialize_State (Config : Config_Type) return State_Type;
package Generic_Pipeline_Stage is

   type Pipeline_Stage is tagged private;

   function Create (Config : Config_Type) return Pipeline_Stage;

   function Process (Self : in out Pipeline_Stage; Input : Input_Type)
     return Output_Result.Result;

   procedure Process_Batch (Self : in out Pipeline_Stage;
                           Inputs : Input_Array;
                           Outputs : out Output_Array;
                           Success_Flags : out Success_Array;
                           Errors : out Error_Array);
end Generic_Pipeline_Stage;
```

#### 6.1.2 Design Rationale

**Generic Parameters:**
- Input_Type/Output_Type: Type safety for data flow
- State_Type: Stateful processing with shared state
- Config_Type: Parameterized behavior
- Stage_Name: Runtime identification and logging
- Process_Element: User-defined processing logic
- Initialize_State: State setup from configuration

**Features:**
- Single item and batch processing
- Individual error tracking in batches
- Performance statistics collection
- Parallel processing capability
- State management across items

### 6.2 Saga Coordinator

#### 6.2.1 Design Overview

```ada
package Saga_Coordinator is

   type Saga_Step_Interface is limited interface;

   function Execute (Step : Saga_Step_Interface; Context : Unbounded_String)
     return Step_Result_Package.Result is abstract;

   function Compensate (Step : Saga_Step_Interface; Context : Unbounded_String)
     return Step_Result_Package.Result is abstract;

   function Name (Step : Saga_Step_Interface) return String is abstract;

   type Saga_Definition is tagged record
      Name : Unbounded_String;
      Steps : Step_Lists.List;
      Timeout : Duration := 300.0;
      Max_Retries : Natural := 3;
   end record;

   type Saga_Coordinator is tagged private;

   function Execute_Saga (Self : in out Saga_Coordinator;
                         Saga_Name : String;
                         Context : Unbounded_String)
     return Saga_Result_Package.Result;
end Saga_Coordinator;
```

#### 6.2.2 Design Rationale

**Saga Pattern Implementation:**
- Step interface for individual transaction operations
- Compensation logic for rollback capability
- Timeout handling for hanging operations
- Retry mechanism for transient failures
- Context passing for step coordination

**Benefits:**
- Distributed transaction management
- Automatic compensation on failures
- Configurable timeout and retry policies
- Event-driven progress tracking

---

## 6.2 Contract Specifications

This section documents the Ada 2022 contracts (Pre/Post conditions, Type Invariants) that ensure runtime safety and document critical assumptions for key components.

### 6.2.1 ACID Repository Contracts

**Purpose**: Ensure transaction state consistency and prevent invalid state transitions.

```ada
-- Transaction state management
function Begin_Transaction
  (Self      : ACID_Repository_Interface;
   Isolation : Isolation_Level := Read_Committed) return Transaction_Result.Result
with
  Pre'Class => not Self.In_Transaction and then
               Self.Get_Transaction_State = None,
  Post'Class =>
    (if Transaction_Result.Is_Ok (Begin_Transaction'Result) then
       Self.In_Transaction and then
       Self.Get_Transaction_State = Active);

-- Commit safety
function Commit_Transaction
  (Self : ACID_Repository_Interface) return Transaction_Result.Result
with
  Pre'Class => Self.In_Transaction and then
               Self.Get_Transaction_State = Active,
  Post'Class =>
    (if Transaction_Result.Is_Ok (Commit_Transaction'Result) then
       not Self.In_Transaction and then
       Self.Get_Transaction_State = None);

-- Version control for optimistic concurrency
function Update
  (Self     : ACID_Repository_Interface;
   Entity   : Entity_Type;
   Expected_Version : Natural) return Update_Result.Result
with
  Pre'Class => Entity.Is_Valid and then
               Expected_Version = Entity.Get_Version and then
               Self.In_Transaction,
  Post'Class =>
    (if Update_Result.Is_Ok (Update'Result) then
       Entity.Get_Version = Expected_Version + 1);
```

### 6.2.2 Aggregate Root State Management

**Purpose**: Maintain aggregate consistency and event sourcing integrity.

```ada
type Aggregate_Root_Type is tagged record
   Id : Id_Type;
   Version : Natural := 0;
   Is_New : Boolean := True;
   Uncommitted_Events : Event_List;
end record
with Type_Invariant =>
  Aggregate_Root_Type.Version >= 0 and then
  (Aggregate_Root_Type.Is_New = (Aggregate_Root_Type.Version = 0));

function Apply_Event
  (Self : in out Aggregate_Root_Type;
   Event : Domain_Event'Class) return Event_Result.Result
with
  Pre'Class => Self.Is_Valid and then Event.Is_Valid,
  Post'Class => 
    (if Event_Result.Is_Ok (Apply_Event'Result) then
       Self.Version = Self'Old.Version + 1 and then
       Self.Uncommitted_Events.Length = Self'Old.Uncommitted_Events.Length + 1);
```

### 6.2.3 File Path Operations

**Purpose**: Validate path operations and prevent path traversal attacks.

```ada
function Create
  (Path : String; 
   File_Type : File_Type_Enum) return File_Path_Result.Result
with
  Pre => Path'Length > 0 and then
         Path'Length <= Max_Path_Length and then
         not Contains_Null_Character(Path),
  Post =>
    (if File_Path_Result.Is_Ok (Create'Result) then
       File_Path_Result.Unwrap(Create'Result).Get_Path = Path and then
       File_Path_Result.Unwrap(Create'Result).Get_File_Type = File_Type);

function Normalize (Self : File_Path) return File_Path_Result.Result
with
  Pre'Class => Self.Is_Valid,
  Post'Class =>
    (if File_Path_Result.Is_Ok (Normalize'Result) then
       not Contains_Path_Traversal(File_Path_Result.Unwrap(Normalize'Result).Get_Path));
```

### 6.2.4 Strong Types in Contracts

**Purpose**: Demonstrate how strong types enhance contract expressiveness and prevent type mixing errors.

```ada
function Process_File_With_Timeout
  (File_Size : SI_Bytes_Type;
   Timeout   : Timeout_Ms_Type;
   Max_Speed : Throughput_Type) return Processing_Result
with
  Pre => File_Size > 0 and then
         Timeout >= 1_000 and then        -- At least 1 second
         Max_Speed > 0.0,
  Post => 
    (if Processing_Result.Is_Ok then
       Calculate_Throughput(File_Size, To_Seconds(Timeout)) <= Max_Speed);

function Calculate_Performance_Metrics
  (Bytes_Processed : SI_Bytes_Type;    -- Decimal bytes (MB = 1,000,000)
   Time_Elapsed    : Seconds_Type;     -- Duration in seconds
   Retry_Count     : Retry_Count_Type) -- Number of retry attempts
   return Performance_Report
with
  Pre => Bytes_Processed > 0 and then
         Time_Elapsed > 0.0 and then
         Retry_Count >= 0,
  Post =>
    Performance_Report.Throughput = 
      Calculate_Throughput(Bytes_Processed, Time_Elapsed) and then
    Performance_Report.Retry_Count = Retry_Count;
```

### 6.2.5 Contract Best Practices and Patterns

**Contract Writing Guidelines:**
- **Be Specific**: Use complete conditions rather than vague checks
- **Document Assumptions**: Contracts serve as executable documentation
- **Use Strong Types**: Leverage domain types in contracts to prevent type mixing

#### Common Contract Patterns

**Null Safety Pattern:**
```ada
Pre => Ptr /= null and then Ptr.all.Is_Valid
```

**Range Safety Pattern:**
```ada
Pre => Value in Min_Value .. Max_Value,
Post => Result in Expected_Min .. Expected_Max
```

**State Transition Pattern:**
```ada
Pre => State = Ready,
Post => State = Processing or else State = Error
```

**Collection Safety Pattern:**
```ada
Pre => not Collection.Is_Empty and then
       Collection.Length <= Max_Size,
Post => Collection.Length = Collection'Old.Length + 1
```

**Performance Metrics Example:**
```ada
function Calculate_Performance_Metrics
  (Bytes_Processed : SI_Bytes_Type;    -- Decimal bytes (MB = 1,000,000)
   Time_Elapsed    : Seconds_Type;     -- Duration in seconds
   Retry_Count     : Retry_Count_Type) -- Number of retry attempts
   return Performance_Report
with
  Pre => Bytes_Processed > 0 and then
         Time_Elapsed > 0.0 and then
         Retry_Count >= 0,
  Post =>
    Performance_Report.Throughput = 
      Calculate_Throughput(Bytes_Processed, Time_Elapsed) and then
    Performance_Report.Retry_Count = Retry_Count;
```

---

## 6.3 Core Domain Modules

### 6.3.1 Math Module

#### Purpose
The `Abohlib.Core.Domain.Math` module provides common mathematical calculations and types used throughout the library. It centralizes generic math operations that don't belong to specific domain types, ensuring consistency and preventing code duplication.

#### Design Overview

```ada
package Abohlib.Core.Domain.Math is
   pragma Pure;
   
   -- Strong type for percentages with fixed-point precision
   type Percentage_Type is delta 0.01 range 0.0 .. 100.0;
   
   -- Strong type for ratios where 1.0 = 100%
   type Ratio_Type is new Float range 0.0 .. Float'Last
      with Default_Value => 1.0;
```

#### Key Features

**Percentage Calculations:**
- Overloaded `Calculate_Percentage` functions for Natural, Long_Long_Integer, and Float
- Automatic clamping to valid range (0.0 to 100.0)
- Preconditions ensure denominator is never zero

**Ratio Operations:**
- Bidirectional conversion between percentages and ratios
- Type safety prevents mixing percentages with raw floats
- Clear semantic meaning (0.5 = 50%, 1.0 = 100%)

**Utility Functions:**
- Generic `Clamp` function for constraining values to ranges
- `Approximately_Equal` for floating-point comparisons with epsilon

#### Integration Points
- Performance module uses `Percentage_Type` as a subtype
- All percentage calculations throughout the library delegate to Math module
- Ensures consistent precision (0.01) and range validation

### 6.3.2 Type System and Conversions

#### Purpose
Abohlib employs strong typing to prevent semantic errors at compile time. The type system includes comprehensive conversion functions that maintain type safety while providing practical usability.

#### Design Principles

**Type Creation Strategy:**
1. **New Types** (`type X is new Y`) - For semantically distinct concepts
2. **Subtypes** (`subtype X is Y range A..B`) - For computational constraints
3. **Conversion Functions** - Explicit, named functions for clarity

#### Core Conversions

**Bytes Module (`Abohlib.Core.Domain.Types.Bytes`):**
```ada
-- Bidirectional conversions
function To_Long_Long_Integer (Bytes : SI_Bytes_Type) return Long_Long_Integer;
function From_Long_Long_Integer (Value : Long_Long_Integer) return SI_Bytes_Type;

-- Natural conversions with safety checks
function To_Natural (Bytes : SI_Bytes_Type) return Natural
   with Pre => Bytes <= SI_Bytes_Type (Natural'Last);
function From_Natural (Value : Natural) return SI_Bytes_Type;

-- Unit-based creation functions (support very large values)
-- From_KB: up to 9,223,372,036,854,775 KB (≈ 9.2 EB)
-- From_MB: up to 9,223,372,036,854 MB (≈ 9.2 PB)
-- From_GB: up to 9,223,372,036 GB (≈ 9,223 TB or 9.2 PB)
function From_KB (KB : Long_Long_Integer) return SI_Bytes_Type;
function From_MB (MB : Long_Long_Integer) return SI_Bytes_Type;
function From_GB (GB : Long_Long_Integer) return SI_Bytes_Type;

-- Cross-unit conversions
function To_IEC_Bytes (SI : SI_Bytes_Type) return IEC_Bytes_Type;
function To_SI_Bytes (IEC : IEC_Bytes_Type) return SI_Bytes_Type;
```

**Time Module (`Abohlib.Core.Domain.Types.Time`):**
```ada
-- Duration conversions
function To_Milliseconds (D : Duration) return Milliseconds_Type
   with Pre => D >= 0.0 and then D <= Duration (Natural'Last / 1_000);
function From_Milliseconds (Ms : Milliseconds_Type) return Duration;

-- Specific type conversions
function To_Duration (Timeout : Timeout_Ms_Type) return Duration;
function To_Duration (Delay_Ms : Delay_Ms_Type) return Duration;

-- Unit conversions
function To_Seconds (Ms : Milliseconds_Type) return Seconds_Type;
function From_Seconds (S : Seconds_Type) return Milliseconds_Type;
```

#### Conversion Safety
- All conversions use expression functions for efficiency
- Preconditions catch overflow/underflow at compile time when possible
- Runtime checks ensure data integrity
- No implicit conversions - all type changes are explicit

### 6.3.3 Performance Module

#### Purpose
The Performance module provides specialized types and calculations for measuring application performance, including throughput, compression ratios, and data transfer rates.

#### Design Overview

```ada
package Abohlib.Core.Domain.Types.Performance is
   -- Re-export Math module's percentage type
   subtype Percentage_Type is Abohlib.Core.Domain.Math.Percentage_Type;
   
   -- Performance measurement types
   type Bytes_Per_Second_Type is new Float range 0.0 .. Float'Last;
   type MB_Per_Second_Type is new Float range 0.0 .. Float'Last;
   type Compression_Ratio_Type is new Float range 0.0 .. Float'Last
      with Default_Value => 1.0;
```

#### Key Functions

**Speed Calculations:**
```ada
function Calculate_MB_Per_Second
   (Bytes    : SI_Bytes_Type;
    Duration : Standard.Duration) return MB_Per_Second_Type
   with Pre => Duration > 0.0;

-- Overload for legacy code compatibility
function Calculate_MB_Per_Second
   (Bytes    : Long_Long_Integer;
    Duration : Standard.Duration) return MB_Per_Second_Type
   with Pre => Bytes >= 0 and then Duration > 0.0;
```

**Compression Metrics:**
```ada
function Calculate_Compression_Ratio
   (Original_Size   : SI_Bytes_Type;
    Compressed_Size : SI_Bytes_Type) return Compression_Ratio_Type
   with Pre => Original_Size >= 0 and then Compressed_Size >= 0;

function Compression_Percentage_Saved
   (Ratio : Compression_Ratio_Type) return Percentage_Type;
```

**Float Conversions:**
```ada
-- Performance type conversions with explicit naming to avoid overload ambiguity
function MB_Speed_To_Float (Speed : MB_Per_Second_Type) return Float;
function Float_To_MB_Speed (Value : Float) return MB_Per_Second_Type;

function Processing_Speed_To_Float (Speed : Processing_Speed_Type) return Float;
function Float_To_Processing_Speed (Value : Float) return Processing_Speed_Type;

function Compression_Ratio_To_Float (Ratio : Compression_Ratio_Type) return Float;
function Float_To_Compression_Ratio (Value : Float) return Compression_Ratio_Type;
```

#### Math Module Integration
- Delegates generic percentage calculations to Math module
- Provides domain-specific wrapper for byte percentages
- Maintains consistent percentage precision across the library
- All percentage operations return `Math.Percentage_Type`

---

## 7. Deployment Design

### 7.1 Library Structure

```
libabohlib.a/
├── Domain Components
│   ├── Value Objects (Pure functions)
│   ├── Entities (Stateful objects)
│   ├── Services (Business logic)
│   └── Events (Domain events)
├── Application Components
│   ├── Use Cases (Workflows)
│   └── Services (Orchestration)
├── Infrastructure Components
│   ├── Adapters (External integrations)
│   ├── Testing (Test framework)
│   └── Resilience (Retry handlers)
└── Presentation Components
    ├── CLI (Command-line interfaces)
    └── APIs (Interface definitions)
```

### 7.2 Integration Patterns

**Static Linking (Default):**
- Single .a library file
- All dependencies included
- No runtime dependencies beyond Ada RTL
- Suitable for embedded deployment

**Dynamic Linking (Optional):**
- Shared .so library
- Reduced executable size
- Runtime dependency management required
- Better for system-wide installation

### 7.3 Configuration Design

**Alire Integration:**
```toml
[build-profiles]
"*" = "development"
"release" = "optimization"

[build-switches]
"*".style_checks = ["-gnatyM120", "-gnatyx"]
"*".contracts = "yes"
"release".optimization = ["-O2", "-gnatn"]
```

**Environment Variables:**
- `ABOHLIB_LOG_LEVEL`: Logging verbosity
- `ABOHLIB_CONFIG_PATH`: Configuration file location
- `ABOHLIB_CACHE_DIR`: Temporary file storage

---

## 8. Quality Attributes

### 8.1 Performance Design

**Zero-Cost Abstractions:**
- Generic instantiation at compile time
- Inlined small functions with pragma Inline
- Stack allocation preference over heap
- Compile-time constant folding

**Memory Management:**
- Bounded types for predictable allocation
- RAII pattern through controlled types
- Minimal heap allocation in core components
- Explicit lifetime management

**Benchmarking Strategy:**
```ada
procedure Benchmark_Pipeline_Stage is
   Config : constant Test_Config := (Buffer_Size => 1024, Workers => 4);
   Stage : Test_Pipeline_Stage := Create (Config);

   Start_Time : constant Ada.Calendar.Time := Ada.Calendar.Clock;

   for I in 1 .. 10_000 loop
      Result : constant Output_Result.Result := Stage.Process (Test_Input);
      pragma Assert (Result.Is_Ok);
   end loop;

   End_Time : constant Ada.Calendar.Time := Ada.Calendar.Clock;
   Duration_Per_Item : constant Duration := (End_Time - Start_Time) / 10_000;

   pragma Assert (Duration_Per_Item < 0.001); -- Less than 1ms per item
end Benchmark_Pipeline_Stage;
```

### 8.2 Reliability Design

**Error Recovery:**
- Retry patterns with exponential backoff
- Circuit breaker for cascading failures
- Graceful degradation mechanisms
- Health check interfaces

**Fault Tolerance:**
- Input validation at all boundaries
- Defensive programming practices
- Resource cleanup in exception handlers
- Atomic operations for critical sections

### 8.3 Security Design

**Input Validation:**
```ada
function Validate_File_Path (Path : String) return Boolean is
begin
   -- Check for path traversal attempts
   if Ada.Strings.Fixed.Index (Path, "..") > 0 then
      return False;
   end if;

   -- Check for absolute paths outside allowed directories
   if Path'Length > 0 and then Path (Path'First) = '/' then
      return False;
   end if;

   -- Additional validation logic...
   return True;
end Validate_File_Path;
```

**Data Sanitization:**
- SQL injection prevention in repository adapters
- File path validation to prevent directory traversal
- Input length limits to prevent buffer overflows
- Character encoding validation

### 8.4 Maintainability Design

**Code Organization:**
- Clear package hierarchy following architecture layers
- Consistent naming conventions
- Separation of concerns at package level
- Minimal inter-package dependencies

**Documentation Strategy:**
- Ada comment format for all public interfaces
- Usage examples in package specifications
- Architecture decision records
- API documentation generation

**Testing Strategy:**
- Unit tests for all public interfaces
- Integration tests for adapter implementations
- Performance regression tests
- Property-based testing where applicable

## 8. Test Architecture ✅ **IMPLEMENTED**

### 8.1 Comprehensive Test Suite

The library implements a **comprehensive 58-test suite** with 240+ individual test functions covering all architectural layers:

```
📁 tests/
├── unit/
│   ├── core/
│   │   ├── domain/
│   │   │   ├── utilities/           # ✅ ULID Helpers (9 tests)
│   │   │   └── errors/              # ✅ Result Pattern (5 tests)
│   │   ├── application/
│   │   │   └── errors/              # ✅ Application Errors (8 tests)
│   │   └── repositories/            # ✅ ACID Repository (10 tests)
│   └── infrastructure/
│       ├── resilience/              # ✅ Retry Handler (8 tests)
│       └── errors/                  # ✅ Infrastructure Errors (18 tests)
└── simple_test_runner.adb           # Test orchestration
```

### 8.2 Result-Based Testing Framework

All tests use the custom Result pattern eliminating exception propagation:

```ada
function Test_Operation return Test_Framework.Void_Result.Result is
begin
   -- Test implementation
   if not Expected_Condition then
      return Test_Framework.Void_Result.Err (Test_Error'(
         Kind        => Assertion_Failed,
         Message     => "Description of failure",
         Line_Number => 42,
         Test_Name   => "Test_Operation"
      ));
   end if;

   return Test_Framework.Void_Result.Ok (True);
end Test_Operation;
```

### 8.3 Test Coverage Achievement

- **✅ Domain Layer:** 90% coverage (ULID utilities, Result pattern)
- **✅ Infrastructure Layer:** 90%+ coverage (retry handlers, repositories, comprehensive error handling)
- **✅ Application Layer:** Full coverage (error handling, workflows)
- **✅ Integration Ready:** Mock framework and test harness implemented

## 9. Additional Design Considerations

### 9.1 Error Recovery Strategies

The library implements multiple error recovery patterns:

- **Result Pattern**: Primary mechanism for error propagation without exceptions
- **Retry Mechanisms**: Configurable retry handlers with exponential backoff
- **Circuit Breakers**: Prevent cascading failures in distributed operations
- **Fallback Values**: Or_Else pattern for providing default values
- **Compensation Logic**: Saga pattern for reversing partial operations

### 9.2 Logging and Monitoring Design

The library provides hooks for monitoring:

- **Structured Logging**: Type-safe log entry construction
- **Metrics Collection**: Performance counters and timing measurements
- **Trace Context**: Correlation IDs for distributed tracing
- **Health Checks**: Component status reporting interfaces
- **Event Streams**: Domain events as audit trail

### 9.3 Configuration Management

Configuration follows these principles:

- **Type-Safe Configuration**: Strongly typed configuration records
- **Compile-Time Defaults**: Sensible defaults in package specifications
- **Runtime Override**: Environment variable support for deployment
- **Validation**: Configuration validation at startup
- **Hot Reload**: Support for configuration changes without restart

### 9.4 Performance Optimization Strategies

The design optimizes for:

- **Zero-Cost Abstractions**: Generic instantiation without runtime overhead
- **Memory Efficiency**: Stack allocation preferred over heap
- **Cache Friendliness**: Data structure layout for cache lines
- **Parallelism**: Ada 2022 parallel blocks for CPU-bound operations
- **Lazy Evaluation**: Deferred computation where beneficial

### 9.5 Security Design Considerations

Security is built into the design:

- **Input Validation**: All external inputs validated at boundaries
- **Type Safety**: Strong typing prevents many security issues
- **Immutability**: Value objects prevent unauthorized modification
- **Access Control**: Protected types for thread-safe operations
- **Cryptographic Support**: SHA-256 hasher for integrity checks

---

## Document Control

**Version History:**

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0 | 2025-08-02 | Development Team | Initial version |

**Review and Approval:**

| Role | Name | Date | Signature |
|------|------|------|-----------|
| Lead Architect | TBD | TBD | TBD |
| Senior Developer | TBD | TBD | TBD |
| Quality Assurance | TBD | TBD | TBD |

**Distribution:**
- Development Team
- Architecture Review Board
- Quality Assurance Team
- Documentation Repository

---

*This document describes the detailed design of Abohlib v1.0.0 and serves as the primary reference for implementation and maintenance activities.*
