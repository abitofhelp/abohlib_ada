# Abohlib Testing Guide

**Version:** 1.0.0  
**Date:** January 2025  
**Classification:** Open Source Library (MIT License)  
**Status:** Production Release

## Table of Contents

1. [Introduction](#introduction)
2. [Quick Start](#quick-start)
3. [Testing Philosophy](#testing-philosophy)
4. [Test Architecture](#test-architecture)
5. [Writing Tests](#writing-tests)
6. [Running Tests](#running-tests)
7. [Test Types](#test-types)
8. [Mock Framework](#mock-framework)
9. [Test Environment](#test-environment)
10. [Coverage Analysis](#coverage-analysis)
11. [Continuous Testing](#continuous-testing)
12. [Troubleshooting](#troubleshooting)

## Introduction

This comprehensive guide explains how to write, organize, and run tests for applications using Abohlib. The library provides a powerful testing framework that follows the same architectural principles as the main library - type safety, explicit error handling, and clean separation of concerns.

### Why This Guide Matters

Testing is not an afterthought in Abohlib - it's a first-class concern built into the architecture:
- **Result-Based Assertions**: No hidden exceptions in tests
- **Type-Safe Mocking**: Compile-time verification of mock behavior
- **Contract Testing**: Verify Ada 2022 contracts are honored
- **Architecture Alignment**: Tests follow the same layered structure as your code

### What You'll Learn

- **Fundamentals**: Core testing concepts and the Result-based approach
- **Practical Examples**: Step-by-step test writing with real code
- **Advanced Patterns**: Property-based testing, contract verification, performance benchmarks
- **Test Organization**: Structure tests to match your architecture
- **Debugging**: Troubleshoot failing tests effectively
- **Automation**: Set up continuous testing and coverage reporting

## Quick Start

### Running All Tests

```bash
# Using Make
make test

# Using Alire directly
alr exec -- ./bin/test_all --all

# With coverage analysis
make test-coverage
```

### Running Specific Test Categories

```bash
# Unit tests only
make test-unit

# Integration tests only  
make test-integration

# Performance benchmarks
make benchmark
```

### Writing Your First Test

Let's write a complete test that demonstrates the key concepts:

```ada
pragma Ada_2022;

with Abohlib.Infrastructure.Testing.Test_Framework; use Abohlib.Infrastructure.Testing.Test_Framework;
with Abohlib.Core.Domain.Value_Objects.Constrained_Strings;
use Abohlib.Core.Domain.Value_Objects.Constrained_Strings;

-- Test file: tests/unit/domain/test_email_validation.adb
procedure Test_Email_Validation is
   
   -- Test 1: Valid email should be accepted
   function Test_Valid_Email return Void_Result.Result is
      Email_Result : constant Email_Result := Create_Email ("user@example.com");
   begin
      -- Check that creation succeeded
      if Email_Result.Is_Err then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Valid email rejected"),
            Details     => To_Unbounded_String ("Email 'user@example.com' should be valid"),
            Line_Number => 10,
            Test_Name   => To_Unbounded_String ("Test_Valid_Email")
         ));
      end if;
      
      -- Verify the email value
      declare
         Email : constant Email_Type := Email_Result.Get_Ok;
      begin
         if To_String (Email) /= "user@example.com" then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Email value incorrect"),
               Details     => To_Unbounded_String ("Got: " & To_String (Email)),
               Line_Number => 20,
               Test_Name   => To_Unbounded_String ("Test_Valid_Email")
            ));
         end if;
      end;
      
      return Void_Result.Ok (True);
   end Test_Valid_Email;
   
   -- Test 2: Invalid email should be rejected
   function Test_Invalid_Email return Void_Result.Result is
      Email_Result : constant Email_Result := Create_Email ("not-an-email");
   begin
      -- This SHOULD fail
      if Email_Result.Is_Ok then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Invalid email accepted"),
            Details     => To_Unbounded_String ("Email 'not-an-email' should be invalid"),
            Line_Number => 35,
            Test_Name   => To_Unbounded_String ("Test_Invalid_Email")
         ));
      end if;
      
      -- Success - the invalid email was correctly rejected
      return Void_Result.Ok (True);
   end Test_Invalid_Email;
   
   -- Test runner
   Runner : Test_Runner;
   Suite : Test_Suite;
begin
   -- Register tests
   Suite.Add_Test ("Valid Email", Test_Valid_Email'Access);
   Suite.Add_Test ("Invalid Email", Test_Invalid_Email'Access);
   
   -- Run tests and print results
   Runner.Add_Suite ("Email Validation", Suite);
   Runner.Run;
   
   -- Exit with appropriate code
   if Runner.Failed_Count > 0 then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Test_Email_Validation;
```

### Key Testing Concepts

1. **Result-Based Assertions**
   - Tests return `Void_Result.Result` instead of raising exceptions
   - Success: `return Void_Result.Ok (True)`
   - Failure: `return Void_Result.Err (Test_Error'(...))`

2. **Rich Error Context**
   - `Kind`: Type of failure (Assertion_Failed, Precondition_Failed, etc.)
   - `Message`: What went wrong
   - `Details`: Additional context
   - `Line_Number`: Where it failed
   - `Test_Name`: Which test failed

3. **Test Organization**
   - Group related tests in procedures
   - Use Test_Suite to organize tests
   - Test_Runner executes and reports results

4. **No Hidden State**
   - Each test is self-contained
   - No global variables or shared state
   - Tests can run in any order

## Testing Philosophy

### Core Testing Principles

1. **Result Pattern Everywhere**
   ```ada
   -- Tests never raise exceptions
   function Test_Something return Void_Result.Result;
   
   -- Assertions are explicit
   if not Condition then
      return Void_Result.Err (Test_Error'(...));
   end if;
   ```
   
   Benefits:
   - Test failures don't crash the test runner
   - Error context is preserved and reported
   - Tests compose like regular functions

2. **Architecture-Driven Testing**
   
   **Domain Layer Tests** (Pure Unit Tests)
   - No external dependencies
   - Test business logic in isolation
   - Fast execution (milliseconds)
   - Example: Testing a value object's validation
   
   **Application Layer Tests** (Integration Tests)
   - Mock infrastructure dependencies
   - Test use case orchestration
   - Verify workflows and coordination
   - Example: Testing a complete user registration flow
   
   **Infrastructure Layer Tests** (System Tests)
   - Use real or test-double implementations
   - Test adapters and technical integration
   - May be slower due to I/O
   - Example: Testing file system operations
   
   **Presentation Layer Tests** (Contract Tests)
   - Verify API contracts
   - Test request/response handling
   - May include end-to-end scenarios
   - Example: Testing REST endpoint behavior

3. **Test Independence and Isolation**
   ```ada
   -- BAD: Tests depend on execution order
   Global_Counter : Natural := 0;
   
   function Test_First return Void_Result.Result is
   begin
      Global_Counter := Global_Counter + 1;  -- NO!
   end Test_First;
   
   -- GOOD: Each test is self-contained
   function Test_First return Void_Result.Result is
      Local_Counter : Natural := 0;
   begin
      Local_Counter := Local_Counter + 1;  -- OK
   end Test_First;
   ```

4. **Contract-Based Testing**
   ```ada
   -- Test that contracts are enforced
   function Test_Precondition_Violation return Void_Result.Result is
      Result : Result_Type;
   begin
      -- This should fail the precondition
      Result := Operation_With_Pre (Invalid_Input);
      
      -- Verify it failed appropriately
      if Result.Is_Ok then
         return Void_Result.Err ("Precondition not enforced");
      end if;
      
      return Void_Result.Ok (True);
   end Test_Precondition_Violation;
   ```

5. **Property-Based Testing**
   ```ada
   -- Test properties that should always hold
   function Test_Serialization_Property return Void_Result.Result is
   begin
      for I in 1 .. 100 loop
         declare
            Original : constant My_Type := Generate_Random;
            Serialized : constant String := Serialize (Original);
            Deserialized : constant My_Type := Deserialize (Serialized);
         begin
            if Original /= Deserialized then
               return Void_Result.Err ("Serialization roundtrip failed");
            end if;
         end;
      end loop;
      
      return Void_Result.Ok (True);
   end Test_Serialization_Property;
   ```

### Quality Criteria

**Acceptance Standards:**
- ✅ 90% code coverage on domain layer (ACHIEVED)
- ✅ 90%+ code coverage on application layer (ACHIEVED)
- ✅ 90%+ code coverage on infrastructure layer (ACHIEVED)
- ✅ 100% of public APIs tested
- ✅ Zero critical or high-severity defects
- All performance benchmarks met
- All architectural constraints verified

## Test Architecture

### Test Suite Organization

```
tests/
├── unit/                          # Unit tests
│   ├── core/
│   │   ├── domain/
│   │   │   ├── aggregates/       # Aggregate root tests
│   │   │   ├── entities/         # Entity tests
│   │   │   ├── events/           # Domain event tests
│   │   │   ├── repositories/     # Repository pattern tests
│   │   │   ├── sagas/            # Saga coordinator tests
│   │   │   ├── services/         # Domain service tests
│   │   │   ├── utilities/        # Utility tests
│   │   │   └── value_objects/    # Value object tests
│   │   └── application/
│   │       └── errors/           # Application error tests
│   └── infrastructure/
│       └── errors/               # Infrastructure error tests
├── integration/                   # Integration tests
│   └── infrastructure/
│       ├── adapters/             # Adapter tests
│       └── resilience/           # Resilience pattern tests
├── mocks/                        # Mock implementations
│   ├── file_system.adb          # Mock file system
│   └── system_info.adb          # Mock system info
├── framework/                    # Test framework components
│   ├── assertions.ads           # Assertion utilities
│   ├── console_output.adb       # Console output adapter
│   └── framework.ads            # Core framework
└── test_all.adb                 # Main test runner
```

### Test Framework Architecture

The Abohlib testing framework is built on the Result pattern, providing a clean and type-safe testing experience:

```ada
-- The core test framework package
package Abohlib.Infrastructure.Testing.Test_Framework is

   -- Test error types for detailed failure information
   type Test_Error_Kind is (
      Assertion_Failed,    -- Test assertion didn't pass
      Unexpected_Error,    -- Unexpected exception caught
      Timeout_Error,       -- Test exceeded time limit
      Setup_Error,         -- Test setup failed
      Teardown_Error       -- Test cleanup failed
   );
   
   type Test_Error is record
      Kind        : Test_Error_Kind;
      Message     : Unbounded_String;    -- What went wrong
      Details     : Unbounded_String;    -- Additional context
      Line_Number : Natural;             -- Where it failed
      Test_Name   : Unbounded_String;    -- Which test failed
   end record;

   -- All tests return this Result type
   package Void_Result is new Result_Package
     (Ok_Type  => Boolean,  -- True for test success
      Err_Type => Test_Error);
      
   -- Test function signature
   type Test_Function_Access is access function return Void_Result.Result;
   
   -- Test results tracking
   type Test_Status is (Passed, Failed, Skipped, Error);
   
   type Test_Result is record
      Name           : Unbounded_String;
      Status         : Test_Status;
      Message        : Unbounded_String;
      Elapsed_Time   : Duration;
      Line_Number    : Natural;
      Correlation_ID : Unbounded_String;
   end record;
   
   -- Test execution helpers
   function Run_Test
     (Name : String;
      Func : Test_Function_Access;
      Output : access Test_Output_Port'Class) return Test_Result_Pkg.Result;
      
   -- Test suite statistics
   type Test_Statistics is record
      Total_Tests    : Natural := 0;
      Passed_Tests   : Natural := 0;
      Failed_Tests   : Natural := 0;
      Skipped_Tests  : Natural := 0;
      Error_Tests    : Natural := 0;
      Total_Duration : Duration := 0.0;
   end record;
end Test_Framework;
```

**Framework Features:**
- **Result-based**: No exceptions in test execution
- **Rich Error Context**: Detailed information about failures
- **Timing Support**: Automatic test duration tracking
- **Flexible Output**: Pluggable output adapters (console, file, etc.)
- **Test Organization**: Group tests into suites with statistics

## Writing Tests

### Organizing Test Suites

With the Abohlib testing framework, tests are organized into suites. Here's how to structure a test suite:

```ada
-- test_my_component.ads
package Test_My_Component is
   use Abohlib.Infrastructure.Testing.Test_Framework;
   
   -- Main entry point for the test suite
   function Run_All_Tests
     (Output : access Test_Output_Port'Class) return Test_Stats_Result.Result;
end Test_My_Component;

-- test_my_component.adb
package body Test_My_Component is

   -- Individual test functions
   function Test_Component_Creation return Void_Result.Result is
   begin
      -- Test implementation
      return Void_Result.Ok (True);
   end Test_Component_Creation;
   
   function Test_Component_Processing return Void_Result.Result is
   begin
      -- Test implementation
      return Void_Result.Ok (True);
   end Test_Component_Processing;

   -- Run all tests in the suite
   function Run_All_Tests
     (Output : access Test_Output_Port'Class) return Test_Stats_Result.Result
   is
      Tests : Test_Results_Array (1 .. 2);
      Index : Positive := 1;
      
      procedure Add_Test_Result
        (Name : String;
         Test_Func : Test_Function_Access)
      is
         Result : constant Test_Result_Pkg.Result :=
            Run_Test (Name, Test_Func, Output);
      begin
         if Result.Is_Ok then
            Tests (Index) := Result.Get_Ok;
            Print_Test_Result (Tests (Index), Output);
            Index := Index + 1;
         else
            -- Handle test execution error
            declare
               Error : constant Test_Error := Result.Get_Err;
            begin
               Tests (Index) := Test_Result'(
                  Name           => To_Unbounded_String (Name),
                  Status         => Failed,
                  Message        => Error.Message,
                  Elapsed_Time   => 0.0,
                  Line_Number    => Error.Line_Number,
                  Correlation_ID => To_Unbounded_String ("TEST-" & Name)
               );
               Print_Test_Result (Tests (Index), Output);
               Index := Index + 1;
            end;
         end if;
      end Add_Test_Result;
      
   begin
      Output.Write_Line ("=== Running My Component Tests ===");
      Output.Write_Line ("");
      
      -- Run each test
      Add_Test_Result ("Test_Component_Creation", Test_Component_Creation'Access);
      Add_Test_Result ("Test_Component_Processing", Test_Component_Processing'Access);
      
      -- Generate summary statistics
      declare
         Stats_Result : constant Test_Stats_Result.Result :=
            Run_Test_Suite ("My_Component_Tests", Tests (1 .. Index - 1), Output);
      begin
         if Stats_Result.Is_Ok then
            declare
               Stats : constant Test_Statistics := Stats_Result.Get_Ok;
            begin
               Output.Write_Line ("");
               Print_Test_Summary ("My Component Tests", Stats, Output);
               return Test_Stats_Result.Ok (Stats);
            end;
         else
            return Stats_Result;
         end if;
      end;
   end Run_All_Tests;
end Test_My_Component;
```

**Best Practices:**
- Group related tests into a single suite package
- Use descriptive test names that explain what is being tested
- Print clear output for each test and a summary at the end
- Handle both test failures and framework errors gracefully
- Keep test suites focused - one suite per component or feature

### Unit Test Patterns

#### Testing Value Objects

```ada
function Test_Email_Validation return Void_Result.Result is
   use Core.Domain.Value_Objects.Email;
begin
   -- Test valid email
   Valid_Result : constant Email_Result.Result := 
      Create_Email("test@example.com");
   
   if Valid_Result.Is_Err then
      return Assert_Failed("Valid email rejected", Line => 5);
   end if;
   
   -- Test invalid email
   Invalid_Result : constant Email_Result.Result := 
      Create_Email("not-an-email");
      
   if Invalid_Result.Is_Ok then
      return Assert_Failed("Invalid email accepted", Line => 12);
   end if;
   
   return Void_Result.Ok;
end Test_Email_Validation;
```

#### Testing Domain Services

```ada
function Test_Hash_Service return Void_Result.Result is
   Hasher : SHA256_Hasher;
   Input : constant String := "test data";
   Expected : constant String := "known_hash_value";
begin
   -- Test hashing
   Result : constant Hash_Result.Result := Hasher.Hash(Input);
   
   if Result.Is_Err then
      return Assert_Failed("Hashing failed", Line => 7);
   end if;
   
   if Result.Get_Ok /= Expected then
      return Assert_Failed("Hash mismatch", Line => 11);
   end if;
   
   -- Test determinism
   Result2 : constant Hash_Result.Result := Hasher.Hash(Input);
   if Result.Get_Ok /= Result2.Get_Ok then
      return Assert_Failed("Non-deterministic hash", Line => 17);
   end if;
   
   return Void_Result.Ok;
end Test_Hash_Service;
```

#### Testing Repositories

```ada
function Test_Repository_Save_And_Find return Void_Result.Result is
   Repo : In_Memory_User_Repository;
   User : User_Entity := Create_Test_User;
begin
   -- Save entity
   Save_Result : constant Void_Result.Result := Repo.Save(User);
   
   if Save_Result.Is_Err then
      return Assert_Failed("Save failed", Line => 6);
   end if;
   
   -- Find entity
   Find_Result : constant User_Result.Result := 
      Repo.Find_By_Id(User.Get_Id);
      
   if Find_Result.Is_Err then
      return Assert_Failed("Find failed", Line => 13);
   end if;
   
   Retrieved : constant User_Entity := Find_Result.Get_Ok;
   if Retrieved /= User then
      return Assert_Failed("Entity mismatch", Line => 18);
   end if;
   
   return Void_Result.Ok;
end Test_Repository_Save_And_Find;
```

### Integration Test Patterns

#### Testing Port/Adapter Integration

```ada
function Test_File_System_Integration return Void_Result.Result is
   FS : aliased POSIX_File_System.Provider;
   Service : File_Service := Create_Service(FS'Access);
   Test_File : constant String := "/tmp/test_" & Random_String & ".txt";
   Content : constant String := "Integration test content";
begin
   -- Write through service
   Write_Result : constant Void_Result.Result := 
      Service.Write_File(Test_File, Content);
      
   if Write_Result.Is_Err then
      return Assert_Failed("Write failed", Line => 8);
   end if;
   
   -- Read back
   Read_Result : constant String_Result.Result := 
      Service.Read_File(Test_File);
      
   if Read_Result.Is_Err then
      return Assert_Failed("Read failed", Line => 15);
   end if;
   
   if Read_Result.Get_Ok /= Content then
      return Assert_Failed("Content mismatch", Line => 19);
   end if;
   
   -- Cleanup
   FS.Delete_File(Test_File);
   
   return Void_Result.Ok;
end Test_File_System_Integration;
```

### Property-Based Test Patterns

```ada
function Test_ULID_Ordering_Property return Boolean is
   function Check_Ordering return Boolean is
      ID1 : constant ULID := Generate_ULID;
      -- Small delay to ensure different timestamp
      delay 0.001;
      ID2 : constant ULID := Generate_ULID;
   begin
      -- Later ULID should be lexicographically greater
      return To_String(ID1) < To_String(ID2);
   end Check_Ordering;
begin
   -- Run property check 1000 times
   for I in 1 .. 1000 loop
      if not Check_Ordering then
         return False;
      end if;
   end loop;
   return True;
end Test_ULID_Ordering_Property;
```

## Running Tests

### Command Line Options

```bash
# Run all tests
./bin/test_all --all

# Run specific categories
./bin/test_all --unit              # Unit tests only
./bin/test_all --integration        # Integration tests only
./bin/test_all --performance        # Performance tests only

# Run specific test suites
./bin/test_all --suite=domain      # Domain layer tests
./bin/test_all --suite=repository  # Repository tests

# Verbose output
./bin/test_all --all --verbose

# With timing information
./bin/test_all --all --timing

# Generate JUnit XML output
./bin/test_all --all --junit=test-results.xml
```

### Make Targets

```bash
# Core test targets
make test              # Run all tests
make test-unit         # Run unit tests
make test-integration  # Run integration tests
make test-coverage     # Run with coverage analysis

# Analysis targets
make coverage-report   # Generate HTML coverage report
make test-performance  # Run performance benchmarks
make test-memory      # Run with memory leak detection

# Utility targets
make clean-test       # Clean test artifacts
make test-watch       # Run tests on file changes
```

## Test Types

### Functional Testing

#### API Contract Testing
- Verify all public APIs behave according to specifications
- Test pre-condition validation
- Verify post-condition guarantees
- Check type invariant maintenance
- Validate Result type correctness

#### Business Logic Testing
- Validate domain rules and constraints
- Test aggregate consistency
- Verify event generation and handling
- Check state transition validity
- Ensure business invariant preservation

#### Error Handling Testing
- Verify Result pattern implementation
- Test error propagation across layers
- Check error message clarity and context
- Validate error recovery mechanisms
- Ensure exception containment

### Non-Functional Testing

#### Performance Testing

**Benchmarks and Targets:**

| Component | Metric | Target | Test Method |
|-----------|--------|--------|-------------|
| ID Generation | Throughput | 10,000+ IDs/sec | Batch timing |
| Repository Ops | Latency | < 100ms | Single op timing |
| Pipeline | Throughput | 1,000+ items/sec | Batch processing |
| Memory Usage | Growth | Linear | Memory profiling |

**Example Performance Test:**

```ada
procedure Benchmark_ULID_Generation is
   Start_Time : constant Time := Clock;
   Count : constant := 10_000;
begin
   for I in 1 .. Count loop
      declare
         ID : constant ULID := Generate_ULID;
      begin
         pragma Assert(not Is_Null(ID));
      end;
   end loop;
   
   Duration : constant Duration := Clock - Start_Time;
   Rate : constant Float := Float(Count) / Float(Duration);
   
   Put_Line("Generated" & Count'Image & " ULIDs in" & 
            Duration'Image & " seconds");
   Put_Line("Rate:" & Natural(Rate)'Image & " IDs/second");
   
   pragma Assert(Rate >= 10_000.0);
end Benchmark_ULID_Generation;
```

#### Security Testing

**Input Validation Tests:**

```ada
function Test_Path_Traversal_Prevention return Void_Result.Result is
   Malicious_Paths : constant array (1..5) of String := (
      "../../../etc/passwd",
      "..\\..\\windows\\system32",  
      "/etc/passwd",
      "$(rm -rf /)",
      "'; DROP TABLE users; --"
   );
begin
   for Path of Malicious_Paths loop
      Result : constant Path_Result.Result := 
         Create_Path(Path, Input_Source);
         
      if Result.Is_Ok then
         return Assert_Failed("Security: " & Path & " accepted", 10);
      end if;
   end loop;
   
   return Void_Result.Ok;
end Test_Path_Traversal_Prevention;
```

#### Concurrency Testing

```ada
function Test_Thread_Safe_Repository return Void_Result.Result is
   Repo : aliased Thread_Safe_Repository;
   
   task type Worker is
      entry Start (ID : Natural);
   end Worker;
   
   task body Worker is
      My_ID : Natural;
   begin
      accept Start (ID : Natural) do
         My_ID := ID;
      end Start;
      
      -- Perform concurrent operations
      for I in 1 .. 100 loop
         declare
            Entity : Test_Entity := Create_Entity(My_ID, I);
         begin
            Repo.Save(Entity);
         end;
      end loop;
   end Worker;
   
   Workers : array (1 .. 10) of Worker;
begin
   -- Start concurrent workers
   for I in Workers'Range loop
      Workers(I).Start(I);
   end loop;
   
   -- Wait for completion
   -- (tasks complete when going out of scope)
   
   -- Verify results
   Count : constant Natural := Repo.Count;
   if Count /= 1000 then
      return Assert_Failed("Expected 1000 entities, got" & Count'Image, 40);
   end if;
   
   return Void_Result.Ok;
end Test_Thread_Safe_Repository;
```

## Mock Framework

### Creating Mocks

```ada
-- Define a mock implementation
package body Mock_File_System is
   
   type Mock_State is record
      Files : File_Map.Map;
      Fail_On_Read : Boolean := False;
      Fail_On_Write : Boolean := False;
      Call_Count : Natural := 0;
   end record;
   
   State : Mock_State;
   
   function Read_File (Path : String) return String_Result.Result is
   begin
      State.Call_Count := State.Call_Count + 1;
      
      if State.Fail_On_Read then
         return String_Result.Err(
            Create_Error("Mock read failure", Test_Error_Kind));
      end if;
      
      if State.Files.Contains(Path) then
         return String_Result.Ok(State.Files.Element(Path));
      else
         return String_Result.Err(
            Create_Error("File not found", Not_Found_Kind));
      end if;
   end Read_File;
   
   -- Configuration procedures
   procedure Set_File_Content (Path, Content : String) is
   begin
      State.Files.Include(Path, Content);
   end Set_File_Content;
   
   procedure Configure_Failure (Read, Write : Boolean := False) is
   begin
      State.Fail_On_Read := Read;
      State.Fail_On_Write := Write;
   end Configure_Failure;
   
end Mock_File_System;
```

### Using Mocks in Tests

```ada
function Test_Service_With_Mock return Void_Result.Result is
   Mock_FS : aliased Mock_File_System.Provider;
   Service : File_Service := Create_Service(Mock_FS'Access);
begin
   -- Configure mock
   Mock_FS.Set_File_Content("/test/data.txt", "mock content");
   
   -- Test normal operation
   Result : constant String_Result.Result := 
      Service.Process_File("/test/data.txt");
      
   if Result.Is_Err then
      return Assert_Failed("Processing failed", Line => 9);
   end if;
   
   -- Test error handling
   Mock_FS.Configure_Failure(Read => True);
   
   Error_Result : constant String_Result.Result := 
      Service.Process_File("/test/data.txt");
      
   if Error_Result.Is_Ok then
      return Assert_Failed("Expected failure", Line => 18);
   end if;
   
   -- Verify interactions
   if Mock_FS.Get_Call_Count < 2 then
      return Assert_Failed("Expected 2 calls", Line => 23);
   end if;
   
   return Void_Result.Ok;
end Test_Service_With_Mock;
```

### Mock Types

1. **Stubs** - Return predetermined values
2. **Mocks** - Verify interactions and expectations
3. **Spies** - Record calls while delegating to real implementation
4. **Fakes** - Simplified working implementations

## Test Environment

### Hardware Requirements

**Minimum:**
- CPU: x86_64 or ARM64
- RAM: 2GB available
- Disk: 1GB free space

**Recommended:**
- CPU: Multi-core for parallel testing
- RAM: 8GB for performance testing
- Disk: SSD for I/O tests

### Software Requirements

```bash
# Required
- GNAT FSF 12.0+ or GNAT Pro
- Alire 1.2.0+
- Git 2.30+
- Make 4.0+

# Optional
- GDB (debugging)
- Valgrind (memory testing)
- GCOV (coverage)
- GNATcheck (style)
```

### Test Data Setup

```bash
# Create test data directory
make test-data

# Structure created:
test_data/
├── valid_files/
│   ├── small.txt    # < 1KB
│   ├── medium.txt   # 1MB
│   └── large.txt    # 10MB
├── invalid_files/
│   ├── empty
│   ├── binary.bin
│   └── unicode.txt
└── permissions/
    ├── readable.txt  # 644
    ├── unreadable.txt # 000
    └── executable.sh  # 755
```

## Coverage Analysis

### Generating Coverage Reports

```bash
# Run tests with coverage
make test-coverage

# Generate HTML report
make coverage-report

# View report
open coverage/index.html
```

### Understanding Coverage Metrics

```
Line Coverage: 90.5%
  - Executed lines: 4,525
  - Total lines: 5,000
  
Branch Coverage: 87.3%
  - Taken branches: 1,746
  - Total branches: 2,000
  
Function Coverage: 95.2%
  - Called functions: 476
  - Total functions: 500
```

### Coverage Goals

- Domain Layer: 90% (minimum)
- Application Layer: 80% (minimum)
- Infrastructure Layer: 85% (minimum)
- Overall: 90% (target)

To generate coverage reports, run `make test-coverage` which creates HTML reports in the coverage/ directory.

## Continuous Testing

### CI/CD Integration

```yaml
# .github/workflows/test.yml
name: Continuous Testing
on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      
      - name: Setup Alire
        uses: alire-project/setup-alire@v2
        
      - name: Build
        run: alr build
        
      - name: Run Tests
        run: make test
        
      - name: Coverage Analysis
        run: make test-coverage
        
      - name: Upload Coverage
        uses: codecov/codecov-action@v3
        with:
          file: ./coverage/coverage.xml
```

### Pre-commit Hooks

```bash
#!/bin/bash
# .git/hooks/pre-commit

# Run unit tests
make test-unit
if [ $? -ne 0 ]; then
    echo "Unit tests failed. Commit aborted."
    exit 1
fi

# Check coverage
COVERAGE=$(make coverage-check | grep "Overall" | awk '{print $3}')
if [ "$COVERAGE" -lt "90" ]; then
    echo "Coverage below 90%. Commit aborted."
    exit 1
fi
```

## Troubleshooting

### Common Issues

#### Tests Not Found

```bash
# Error: No tests registered
# Solution: Ensure test registration in test_all.adb
Register_Test("My_Test", Test_My_Component'Access);
```

#### Coverage Not Generated

```bash
# Check build flags
alr build -- -gnatg -gnata -gnatVa -gnatwe -gnatyyM -gnaty3abcdefhijklmnoprstux -Wall -fprofile-arcs -ftest-coverage

# Ensure gcov is installed
which gcov
```

#### Mock Not Working

```ada
-- Ensure mock is properly initialized
Mock_FS : aliased Mock_File_System.Provider;
Mock_FS.Initialize;  -- Don't forget initialization!

-- Verify mock is passed to service
Service : File_Service := Create_Service(Mock_FS'Access);
```

#### Test Timeouts

```ada
-- Add timeout to long-running tests
function Test_With_Timeout return Void_Result.Result is
   task Timer is
      entry Stop;
   end Timer;
   
   task body Timer is
   begin
      select
         accept Stop;
      or
         delay 5.0;  -- 5 second timeout
         return Assert_Failed("Test timeout", Line => 10);
      end select;
   end Timer;
begin
   -- Test code here
   Timer.Stop;
   return Void_Result.Ok;
end Test_With_Timeout;
```

### Debug Mode

```bash
# Run tests with debug output
./bin/test_all --all --debug

# Run specific test with tracing
./bin/test_all --test=Test_ULID_Generation --trace

# Use GDB for debugging
gdb ./bin/test_all
(gdb) break test_ulid_generation
(gdb) run --all
```

## Best Practices

1. **Write Tests First** - TDD approach for new features
2. **Test One Thing** - Each test should verify a single behavior
3. **Use Descriptive Names** - Test names should explain what they test
4. **Keep Tests Fast** - Unit tests should run in milliseconds
5. **Test Edge Cases** - Don't just test the happy path
6. **Use Test Builders** - Create reusable test data builders
7. **Avoid Test Interdependence** - Tests must be independent
8. **Mock External Dependencies** - Keep tests isolated
9. **Verify Contracts** - Test pre/post conditions
10. **Maintain Tests** - Refactor tests along with code

## Advanced Testing Topics

### Security Testing

Security testing validates that the library properly handles security concerns:

#### Security Test Categories

1. **Input Validation Security**
   - SQL injection prevention
   - Path traversal protection
   - Buffer overflow prevention
   - Format string vulnerabilities

2. **Authentication & Authorization**
   - Access control verification
   - Permission boundary testing
   - Token validation

3. **Cryptographic Testing**
   ```ada
   procedure Test_SHA256_Integrity (Output : Test_Output_Access) is
      Input : constant String := "test data";
      Hash1 : constant SHA256_Hash := Compute_Hash (Input);
      Hash2 : constant SHA256_Hash := Compute_Hash (Input);
   begin
      Assert_Equals (Output, Hash1, Hash2, 
                     "Same input must produce same hash");
      Assert_Equals (Output, Hash1'Length, 64,
                     "SHA256 must be 64 hex characters");
   end Test_SHA256_Integrity;
   ```

4. **Security Regression Testing**
   - Maintain tests for past vulnerabilities
   - Automated security scanning
   - Dependency vulnerability checks

### Load Testing

Load testing ensures the library performs well under stress:

#### Load Test Setup

```ada
-- Load test configuration
type Load_Test_Config is record
   Concurrent_Users : Positive := 100;
   Requests_Per_User : Positive := 1000;
   Ramp_Up_Time : Duration := 10.0;
   Test_Duration : Duration := 300.0;  -- 5 minutes
end record;

-- Load test harness
procedure Run_Load_Test
  (Config : Load_Test_Config;
   Test_Scenario : access procedure) is
   
   task type User_Simulator is
      entry Start (User_ID : Positive);
   end User_Simulator;
   
   type User_Array is array (1 .. Config.Concurrent_Users) 
      of access User_Simulator;
begin
   -- Implementation details...
end Run_Load_Test;
```

#### Load Test Scenarios

1. **Repository Load Test**
   - Concurrent reads/writes
   - Connection pool exhaustion
   - Transaction contention

2. **Pipeline Processing Load**
   - Batch size scaling
   - Memory usage under load
   - CPU utilization patterns

3. **Event Processing Load**
   - Event queue saturation
   - Handler performance degradation
   - Memory leak detection

### Contract Testing

Ada 2022 contract testing ensures specifications are verified:

#### Contract Test Patterns

```ada
-- Test precondition violations
procedure Test_Precondition_Failure (Output : Test_Output_Access) is
   
   function Divide (A, B : Integer) return Integer
     with Pre => B /= 0;
   
   function Divide (A, B : Integer) return Integer is
     (A / B);
     
begin
   -- This should raise Assertion_Error
   declare
      Result : Integer;
   begin
      Result := Divide (10, 0);  -- Violates precondition
      Assert_True (Output, False, 
                   "Should have raised assertion error");
   exception
      when Assertion_Error =>
         Assert_True (Output, True, 
                      "Precondition correctly enforced");
   end;
end Test_Precondition_Failure;

-- Test postcondition guarantees
procedure Test_Postcondition_Guarantee (Output : Test_Output_Access) is
   
   function Abs_Value (X : Integer) return Natural
     with Post => Abs_Value'Result >= 0;
     
   function Abs_Value (X : Integer) return Natural is
     (if X >= 0 then X else -X);
     
begin
   Assert_True (Output, Abs_Value (-5) >= 0,
                "Postcondition must be satisfied");
   Assert_True (Output, Abs_Value (5) >= 0,
                "Postcondition must be satisfied");
end Test_Postcondition_Guarantee;
```

#### Contract Coverage

Ensure all contracts are tested:
- Preconditions with valid/invalid inputs
- Postconditions verified
- Type invariants maintained
- Loop invariants checked
- Ghost code verification

### Test Data Management

Comprehensive test data strategies:

#### Test Data Generation

```ada
package Test_Data_Generator is
   
   -- Generate valid test data
   function Generate_Valid_Email return String;
   function Generate_Valid_User return User_Type;
   function Generate_Valid_Order 
     (Customer : Customer_ID) return Order_Type;
     
   -- Generate invalid test data
   function Generate_Invalid_Email return String;
   function Generate_Malformed_JSON return String;
   
   -- Generate boundary test data
   function Generate_Max_Length_String 
     (Max : Natural) return String;
   function Generate_Min_Value return Integer;
   
end Test_Data_Generator;
```

#### Test Data Lifecycle

1. **Setup Phase**
   - Create test database
   - Load fixtures
   - Initialize test accounts

2. **Execution Phase**
   - Isolated transactions
   - Rollback after each test
   - Parallel test support

3. **Cleanup Phase**
   - Remove temporary files
   - Clear test databases
   - Reset system state

#### Test Data Patterns

- **Builder Pattern**: Flexible test object creation
- **Object Mother**: Predefined test objects
- **Fixture Files**: JSON/XML test data files
- **Synthetic Data**: Generated realistic data

## Conclusion

This testing guide provides comprehensive coverage of testing practices for Abohlib. Following these guidelines ensures:

- High code quality through comprehensive testing
- Consistent test patterns across the codebase
- Easy test maintenance and debugging
- Confidence in library reliability
- Security and performance validation

For questions or contributions to the test suite, please refer to the project's contribution guidelines.

---

*This guide is maintained alongside the codebase and updated with each release.*