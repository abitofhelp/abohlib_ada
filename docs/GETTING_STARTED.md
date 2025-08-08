# Getting Started with ABOHLIB

**Version 1.0.0**

This guide will walk you through installing ABOHLIB, understanding its core concepts, and building your first application. By the end, you'll be comfortable using the library's main features and architectural patterns.

## What is ABOHLIB?

ABOHLIB (A Bit of Help Library) is a comprehensive Ada 2022 library that provides:
- Type-safe, reusable components for robust application development
- Modern architectural patterns (DDD, Clean Architecture, Hexagonal)
- Functional error handling without exceptions
- Zero-cost abstractions leveraging Ada's advanced type system

## Prerequisites

Before starting, ensure you have:

### Required Tools
- **Ada 2022 Compiler**: GNAT 2024 or later (includes Ada 2022 support)
  - Download from [AdaCore](https://www.adacore.com/download) or
  - Install via package manager: `apt install gnat` (Ubuntu/Debian) or `brew install gnat` (macOS)
- **Alire Package Manager**: Version 2.0 or later
  - Download from [alire.ada-lang.io](https://alire.ada-lang.io)
  - Verify installation: `alr version`
- **Make**: For build automation (usually pre-installed)

### Recommended Knowledge
- Basic Ada syntax and concepts
- Understanding of types, packages, and generics
- Familiarity with object-oriented concepts helps but isn't required

## Installation

### Quick Start

1. **Create a new Alire project**:
   ```bash
   alr init --bin hello_abohlib
   cd hello_abohlib
   ```

2. **Add ABOHLIB dependency**:
   ```bash
   alr with abohlib~1.0.0
   ```

3. **Build your project**:
   ```bash
   alr build
   ```

### Verifying Installation

Create a simple test program to verify everything works:

```ada
-- src/hello_abohlib.adb
pragma Ada_2022;

with Ada.Text_IO; use Ada.Text_IO;
with Abohlib.Core.Domain.Types; use Abohlib.Core.Domain.Types;

procedure Hello_Abohlib is
   Timeout : constant Timeout_Ms_Type := Timeout_Ms_Type (5000);
begin
   Put_Line ("ABOHLIB installed successfully!");
   Put_Line ("Timeout value: " & Timeout'Image & " ms");
end Hello_Abohlib;
```

Build and run:
```bash
alr run
```

You should see:
```
ABOHLIB installed successfully!
Timeout value:  5000 ms
```

## Core Concepts

ABOHLIB is built around several key concepts that work together to create robust, maintainable applications:

### 1. Result Pattern
Instead of exceptions, ABOHLIB uses Result types for explicit error handling. Every operation that can fail returns a Result containing either success (Ok) or error (Err) values.

**Learn more**: [Result Pattern Guide](guides/RESULT_PATTERN_GUIDE.md)

### 2. Type-Safe IDs
Every entity in your system gets its own ID type that cannot be accidentally mixed with others. This prevents an entire class of bugs at compile time.

```ada
-- These are different types - can't mix them!
User_1 : User_ID := New_User_ID;
Order_1 : Order_ID := New_Order_ID;

-- This won't compile (which is good!):
-- if User_1 = Order_1 then  -- Compilation error!
```

**See examples**: [Type-Safe IDs](EXAMPLES.md#type-safe-ids)

### 3. Strong Domain Types
ABOHLIB uses specific types for different purposes, preventing unit confusion and semantic errors:

```ada
-- Time values have specific meanings
Timeout : Timeout_Ms_Type := Timeout_Ms_Type (5000);      -- 5 second timeout
delay_time : Retry_Delay_Ms_Type := Retry_Delay_Ms_Type (100);  -- 100ms retry delay

-- Can't accidentally mix them:
-- timeout := delay_time;  -- Compilation error!

-- Byte calculations are explicit
file_size : SI_Bytes_Type := From_MB (100);    -- 100 MB (decimal)
memory : IEC_Bytes_Type := From_MiB (100);     -- 100 MiB (binary)

-- Validated strings
email : Email_Type := Create_Email ("user@example.com").Get_Ok;
path : File_Path_Type := Create ("/home/user/file.txt", Regular_File).Get_Ok;
```

**Learn more**: [Type Conversions Guide](guides/TYPE_CONVERSIONS_GUIDE.md)

### 4. Value Objects
Immutable objects that encapsulate validation rules and domain logic. Once created, they cannot be modified, ensuring consistency throughout your application.

**See examples**: [Value Objects](EXAMPLES.md#value-objects)

### 5. Architectural Patterns
ABOHLIB implements proven architectural patterns:
- **Repository Pattern**: Abstract data access behind interfaces
- **Pipeline Processing**: Composable data transformation stages
- **Saga Pattern**: Manage distributed transactions with compensation
- **Event Sourcing**: Capture all changes as domain events

**Learn more**: [Architecture Overview](SOFTWARE_DESIGN_DOCUMENT.md#2-architecture-overview)

## Your First ABOHLIB Program

Let's build a simple user registration system that demonstrates key ABOHLIB concepts:

```ada
pragma Ada_2022;

with Ada.Text_IO; use Ada.Text_IO;
with Abohlib.Core.Domain.Value_Objects.Common_Ids;
with Abohlib.Core.Domain.Value_Objects.Constrained_Strings;
with Abohlib.Core.Domain.Result;
with Abohlib.Core.Domain.Errors;

procedure User_Registration is
   use Abohlib.Core.Domain.Value_Objects.Common_Ids;
   use Abohlib.Core.Domain.Value_Objects.Constrained_Strings;
   use Abohlib.Core.Domain.Errors;
   
   -- Define a user record
   type User is record
      ID : User_ID;
      Email : Email_Type;
   end record;
   
   -- Create a Result type for user operations
   package User_Result is new Abohlib.Core.Domain.Result.Result_Package
     (Ok_Type => User, Err_Type => Domain_Error);
   use User_Result;
   
   -- Register a new user (demonstrates Result pattern)
   function Register_User (Email_Address : String) return Result is
      Email_Result : constant Email_Result := Create_Email (Email_Address);
   begin
      -- Validate email first
      if Email_Result.Is_Err then
         return Err (Validation_Error'
           (Error_ID => Generate_Error_ID,
            Timestamp => Clock,
            Message => "Invalid email format: " & Email_Address,
            Kind => Format_Invalid,
            Field_Name => "email",
            Invalid_Value => Email_Address));
      end if;
      
      -- Create user with validated email and unique ID
      declare
         New_User : constant User := 
           (ID => New_User_ID,
            Email => Email_Result.Get_Ok);
      begin
         return Ok (New_User);
      end;
   end Register_User;
   
   -- Try to register users
   Valid_Result : constant Result := Register_User ("alice@example.com");
   Invalid_Result : constant Result := Register_User ("not-an-email");
begin
   -- Handle successful registration
   if Valid_Result.Is_Ok then
      declare
         User : constant User_Registration.User := Valid_Result.Get_Ok;
      begin
         Put_Line ("Registered user: " & To_String (User.Email));
         Put_Line ("User ID: " & To_String (User.ID));
      end;
   end if;
   
   -- Handle failed registration
   if Invalid_Result.Is_Err then
      Put_Line ("Registration failed: " & Invalid_Result.Get_Err.Message);
   end if;
end User_Registration;
```

### Key Concepts Demonstrated

1. **Result Pattern**: Functions return `Result` instead of raising exceptions
2. **Type-Safe IDs**: Each user gets a unique `User_ID` that can't be mixed with other ID types
3. **Validated Values**: Email addresses are validated when created
4. **Explicit Error Handling**: All errors must be handled - can't ignore them

For more examples, see the [comprehensive examples](EXAMPLES.md).

## Project Structure

ABOHLIB follows a layered architecture. Here's how to organize your project:

```
my_project/
├── alire.toml              # Alire package manifest
├── my_project.gpr          # GNAT project file
├── Makefile                # Build automation
├── src/
│   ├── my_project.ads      # Root package spec
│   ├── domain/             # Core business logic (no external dependencies)
│   │   ├── entities/       # Business entities with identity
│   │   ├── value_objects/  # Immutable values without identity
│   │   ├── aggregates/     # Consistency boundaries
│   │   ├── events/         # Domain events
│   │   ├── repositories/   # Repository interfaces (not implementations!)
│   │   └── services/       # Domain services for complex logic
│   ├── application/        # Use cases and application flow
│   │   ├── use_cases/      # One package per use case
│   │   ├── services/       # Application services
│   │   └── dto/            # Data transfer objects
│   ├── infrastructure/     # Technical implementations
│   │   ├── persistence/    # Database repositories
│   │   ├── messaging/      # Message queue adapters
│   │   ├── http/           # HTTP clients
│   │   └── config/         # Configuration loading
│   └── presentation/       # User interfaces
│       ├── cli/            # Command-line interface
│       ├── rest/           # REST API endpoints
│       └── web/            # Web UI (if applicable)
├── tests/
│   ├── unit/               # Fast, isolated tests
│   ├── integration/        # Tests with real dependencies
│   ├── property/           # Property-based tests
│   └── e2e/                # End-to-end scenarios
└── docs/                   # Project documentation
```

### Dependency Rules

1. **Domain layer** depends on nothing (pure business logic)
2. **Application layer** depends only on Domain
3. **Infrastructure layer** depends on Domain (implements interfaces)
4. **Presentation layer** depends on Application and Domain

This ensures your business logic remains independent of technical details.

## Next Steps

Now that you understand the basics:

1. **Explore Examples**: Browse the [complete examples](EXAMPLES.md) to see patterns in action
2. **Deep Dive**: Read the guides for specific topics:
   - [Result Pattern Guide](guides/RESULT_PATTERN_GUIDE.md) - Master error handling
   - [Testing Guide](TESTING_GUIDE.md) - Write comprehensive tests
   - [Type Conversions Guide](guides/TYPE_CONVERSIONS_GUIDE.md) - Work with strong types
3. **Architecture**: Study the [Software Design Document](SOFTWARE_DESIGN_DOCUMENT.md) to understand the library's design
4. **Contribute**: Check [CONTRIBUTING.md](../CONTRIBUTING.md) to help improve ABOHLIB

## Common Patterns and Best Practices

### Always Use Result Types
```ada
-- Don't use exceptions for expected errors
function Parse_Config (Data : String) return Config_Result.Result;

-- Do use exceptions only for programming errors
pragma Assert (Index in Array'Range);
```

### Validate at Boundaries
```ada
-- Validate input as soon as it enters your system
function Handle_Request (Raw_Input : String) return Response_Result.Result is
   Validated : constant Input_Result.Result := Validate_Input (Raw_Input);
begin
   if Validated.Is_Err then
      return Response_Result.Err (Bad_Request);
   end if;
   -- Now work with validated data
   return Process (Validated.Get_Ok);
end Handle_Request;
```

### Use Type-Safe IDs Everywhere
```ada
-- Each entity gets its own ID type
type Product_ID is new Type_Safe_ID_Type;
type Order_ID is new Type_Safe_ID_Type;
type Customer_ID is new Type_Safe_ID_Type;
```

## Troubleshooting

### Common Issues

1. **"abohlib not found" error**:
   - Run `alr update` to refresh the package index
   - Check your alire.toml has the correct version

2. **Compilation errors with Ada 2022 features**:
   - Ensure you have GNAT 2024 or later
   - Add `pragma Ada_2022;` at the top of your files

3. **"discriminant check failed" at runtime**:
   - You're likely not initializing a Result properly
   - Always use constructor functions: `Ok(...)` or `Err(...)`

## Getting Help

- **Documentation**: Start with this guide, then explore [examples](EXAMPLES.md)
- **API Reference**: Check source code for detailed comments
- **Issues**: Report bugs at [GitHub Issues](https://github.com/abitofhelp/abohlib/issues)
- **Diagrams**: Visual learners should check [architecture diagrams](diagrams/)

---

[← Back to Documentation](README.md) | [Examples →](EXAMPLES.md)