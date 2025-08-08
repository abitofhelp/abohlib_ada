# ABOHLIB Code Examples

This document contains comprehensive code examples demonstrating the use of ABOHLIB components. Each example is complete and can be compiled and run.

## Table of Contents

1. [Basic Examples](#1-basic-examples)
   - [Hello World with Result Pattern](#hello-world-with-result-pattern)
   - [Type-Safe IDs](#type-safe-ids)
   - [Value Objects](#value-objects)
2. [Error Handling](#2-error-handling)
   - [Result Pattern Basics](#result-pattern-basics)
   - [Functional Operations](#functional-operations)
   - [Error Recovery](#error-recovery)
   - [Enhanced Error Handling](#enhanced-error-handling)
3. [Domain Modeling](#3-domain-modeling)
   - [Creating Entities](#creating-entities)
   - [Aggregate Roots](#aggregate-roots)
   - [Domain Events](#domain-events)
4. [Repository Pattern](#4-repository-pattern)
   - [Basic Repository](#basic-repository)
   - [Unit of Work](#unit-of-work)
5. [Pipeline Processing](#5-pipeline-processing)
   - [Simple Pipeline](#simple-pipeline)
   - [Batch Processing](#batch-processing)
6. [Testing](#6-testing)
   - [Unit Tests](#unit-tests)
   - [Contract Tests](#contract-tests)
7. [Advanced Examples](#7-advanced-examples)
   - [Saga Pattern](#saga-pattern)
   - [Event Sourcing](#event-sourcing)

---

## 1. Basic Examples

### Hello World with Result Pattern

```ada
pragma Ada_2022;

with Ada.Text_IO; use Ada.Text_IO;
with Abohlib.Core.Domain.Result;
with Abohlib.Core.Domain.Errors;

procedure Hello_Result is
   use Abohlib.Core.Domain.Errors;
   
   package String_Result is new Abohlib.Core.Domain.Result.Result_Package
     (Ok_Type => String,
      Err_Type => Domain_Error);
   use String_Result;
   
   function Get_Greeting (Name : String) return Result is
   begin
      if Name'Length = 0 then
         return Err (Validation_Error'
           (Error_ID => Generate_Error_ID,
            Timestamp => Clock,
            Message => "Name cannot be empty",
            Kind => Value_Out_Of_Range,
            Field_Name => "name",
            Invalid_Value => ""));
      else
         return Ok ("Hello, " & Name & "!");
      end if;
   end Get_Greeting;
   
   Greeting : constant Result := Get_Greeting ("Ada");
begin
   if Greeting.Is_Ok then
      Put_Line (Greeting.Get_Ok);
   else
      Put_Line ("Error: " & Greeting.Get_Err.Message);
   end if;
end Hello_Result;
```

### Type-Safe IDs

```ada
pragma Ada_2022;

with Abohlib.Core.Domain.Value_Objects.Type_Safe_Generic_Id;
with Abohlib.Core.Domain.Value_Objects.Common_Ids;
with Ada.Text_IO; use Ada.Text_IO;

procedure Type_Safe_Id_Example is
   use Abohlib.Core.Domain.Value_Objects.Common_Ids;
   
   -- Create different ID types
   User_1 : constant User_ID := New_User_ID;
   User_2 : constant User_ID := New_User_ID;
   Order_1 : constant Order_ID := New_Order_ID;
   
   -- Parse from string
   Parsed_Result : constant User_ID_Result := 
      From_String_User_ID ("USR-01J8Z3N4V5X6Y7W8R9P0Q1S2T3");
begin
   -- IDs are type-safe - this won't compile:
   -- if User_1 = Order_1 then  -- Compilation error!
   
   -- Convert to string
   Put_Line ("User ID: " & To_String (User_1));
   Put_Line ("Order ID: " & To_String (Order_1));
   
   -- Handle parsed result
   if Parsed_Result.Is_Ok then
      Put_Line ("Parsed: " & To_String (Parsed_Result.Get_Ok));
   end if;
end Type_Safe_Id_Example;
```

### Value Objects

```ada
pragma Ada_2022;

with Abohlib.Core.Domain.Value_Objects.Constrained_Strings;
with Abohlib.Core.Domain.Value_Objects.File_Path;
with Ada.Text_IO; use Ada.Text_IO;

procedure Value_Objects_Example is
   use Abohlib.Core.Domain.Value_Objects.Constrained_Strings;
   use Abohlib.Core.Domain.Value_Objects.File_Path;
   
   -- Create an email address
   Email_Result : constant Email_Result := Create_Email ("user@example.com");
   
   -- Create a file path
   Path_Result : constant File_Path_Result := 
      Create ("/home/user/document.txt", Regular_File);
begin
   if Email_Result.Is_Ok then
      Put_Line ("Email: " & To_String (Email_Result.Get_Ok));
   end if;
   
   if Path_Result.Is_Ok then
      declare
         Path : constant File_Path := Path_Result.Get_Ok;
      begin
         Put_Line ("File: " & Get_Name (Path));
         Put_Line ("Extension: " & Get_Extension (Path));
         Put_Line ("Directory: " & Get_Directory (Path));
      end;
   end if;
end Value_Objects_Example;
```

## 2. Error Handling

### Result Pattern Basics

```ada
pragma Ada_2022;

with Abohlib.Core.Domain.Result;
with Abohlib.Core.Domain.Errors;
with Ada.Text_IO; use Ada.Text_IO;

procedure Result_Basics is
   use Abohlib.Core.Domain.Errors;
   
   -- Define Result types
   package Integer_Result is new Abohlib.Core.Domain.Result.Result_Package
     (Ok_Type => Integer,
      Err_Type => Domain_Error);
   use Integer_Result;
   
   function Safe_Divide (A, B : Integer) return Result is
   begin
      if B = 0 then
         return Err (Business_Rule_Error'
           (Error_ID => Generate_Error_ID,
            Timestamp => Clock,
            Message => "Division by zero",
            Kind => Invalid_Operation,
            Rule_Name => "division_non_zero_divisor",
            Rule_Context => "arithmetic_operations"));
      else
         return Ok (A / B);
      end if;
   end Safe_Divide;
   
   Result_1 : constant Result := Safe_Divide (10, 2);
   Result_2 : constant Result := Safe_Divide (10, 0);
begin
   -- Pattern matching on results
   case Result_1 is
      when Ok_Result =>
         Put_Line ("10 / 2 = " & Result_1.Get_Ok'Image);
      when Err_Result =>
         Put_Line ("Error: " & Result_1.Get_Err.Message);
   end case;
   
   -- Using predicates
   if Result_2.Is_Err then
      Put_Line ("Expected error: " & Result_2.Get_Err.Message);
   end if;
end Result_Basics;
```

### Functional Operations

```ada
pragma Ada_2022;

with Abohlib.Core.Domain.Result;
with Abohlib.Core.Domain.Result.Functional;
with Abohlib.Core.Domain.Errors;
with Ada.Text_IO; use Ada.Text_IO;

procedure Functional_Operations is
   use Abohlib.Core.Domain.Errors;
   
   -- Define Result types
   package Integer_Result is new Abohlib.Core.Domain.Result.Result_Package
     (Ok_Type => Integer,
      Err_Type => Domain_Error);
      
   package Float_Result is new Abohlib.Core.Domain.Result.Result_Package
     (Ok_Type => Float,
      Err_Type => Domain_Error);
   
   -- Transform function
   function Int_To_Float (I : Integer) return Float is
     (Float (I));
   
   -- Instantiate Map function
   function Map_Int_To_Float is new 
      Abohlib.Core.Domain.Result.Functional.Map
        (Source_Result => Integer_Result,
         Target_Result => Float_Result,
         Transform => Int_To_Float);
   
   use Integer_Result;
   
   Original : constant Integer_Result.Result := Ok (42);
   Transformed : constant Float_Result.Result := Map_Int_To_Float (Original);
begin
   if Transformed.Is_Ok then
      Put_Line ("Transformed: " & Transformed.Get_Ok'Image);
   end if;
end Functional_Operations;
```

### Error Recovery

```ada
pragma Ada_2022;

with Abohlib.Core.Domain.Result;
with Abohlib.Core.Domain.Result.Functional;
with Abohlib.Core.Domain.Errors;
with Ada.Text_IO; use Ada.Text_IO;

procedure Error_Recovery is
   use Abohlib.Core.Domain.Errors;
   
   package String_Result is new Abohlib.Core.Domain.Result.Result_Package
     (Ok_Type => String,
      Err_Type => Domain_Error);
   use String_Result;
   
   function Fallback return String is ("default value");
   
   function Or_Else_Default is new 
      Abohlib.Core.Domain.Result.Functional.Or_Else
        (Result_Package => String_Result,
         Fallback => Fallback);
   
   Error_Result : constant Result := Err (Validation_Error'
     (Error_ID => Generate_Error_ID,
      Timestamp => Clock,
      Message => "Invalid input",
      Kind => Value_Out_Of_Range,
      Field_Name => "input",
      Invalid_Value => ""));
      
   Recovered : constant Result := Or_Else_Default (Error_Result);
begin
   Put_Line ("Recovered value: " & Recovered.Get_Ok);
end Error_Recovery;
```

### Enhanced Error Handling

```ada
pragma Ada_2022;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time;
with Abohlib.Core.Domain.Errors.Enhanced; use Abohlib.Core.Domain.Errors.Enhanced;
with Abohlib.Core.Domain.Errors; use Abohlib.Core.Domain.Errors;

procedure Enhanced_Errors_Example is

   --  Example function that might fail
   function Process_File (Path : String; User_ID : String) return Boolean is
      Start_Time : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;
   begin
      --  Simulate some processing
      delay 0.1;
      
      --  Simulate an error
      if Path = "/invalid/path" then
         declare
            Error : Enhanced_Error;
         begin
            --  Create error with builder pattern
            Error := Create_Error
              .With_Category (Resource_Error)
              .With_Severity (Error)
              .With_Message ("File not found: " & Path)
              .Add_Context ("operation", "file_processing")
              .Add_Context ("user_id", User_ID)
              .Add_Context ("path", Path)
              .Build
              .With_Duration (Ada.Real_Time.Clock - Start_Time);
            
            Put_Line (ASCII.LF & "=== ERROR OCCURRED ===" & ASCII.LF);
            Put_Line (To_Detailed_String (Error));
            Put_Line (ASCII.LF & "=== LOG FORMAT ===" & ASCII.LF);
            Put_Line (To_Log_Format (Error));
            Put_Line (ASCII.LF & "=== JSON FORMAT ===" & ASCII.LF);
            Put_Line (To_JSON (Error));
            
            return False;
         end;
      end if;
      
      return True;
   end Process_File;
   
   --  Example with cause chain
   procedure Database_Operation is
      
      function Connect_To_DB return Enhanced_Error is
      begin
         return Make_Enhanced_Error
           (Category => Resource_Error,
            Severity => Critical,
            Message => "Cannot connect to database",
            Recovery => "Check database connection settings")
           .With_Context ("host", "localhost")
           .With_Context ("port", "5432")
           .With_Context ("database", "myapp");
      end Connect_To_DB;
      
      function Execute_Query return Enhanced_Error is
         Connection_Error : constant Enhanced_Error := Connect_To_DB;
      begin
         return Make_Enhanced_Error
           (Category => Operation_Error,
            Severity => Error,
            Message => "Failed to execute user query")
           .With_Context ("query", "SELECT * FROM users WHERE active = true")
           .With_Context ("timeout", "30s")
           .With_Cause (Connection_Error);
      end Execute_Query;
      
      function Process_User_Request return Enhanced_Error is
         Query_Error : constant Enhanced_Error := Execute_Query;
      begin
         return Make_Enhanced_Error
           (Category => Business_Rule_Error,
            Severity => Warning,
            Message => "Cannot process user request")
           .With_Context ("request_id", "req-12345")
           .With_Context ("user_id", "user-789")
           .With_Context ("action", "list_active_users")
           .With_Cause (Query_Error);
      end Process_User_Request;
      
   begin
      Put_Line (ASCII.LF & "=== DATABASE OPERATION ERROR CHAIN ===" & ASCII.LF);
      
      declare
         Error : constant Enhanced_Error := Process_User_Request;
      begin
         Put_Line (To_Detailed_String (Error));
         Put_Line (ASCII.LF & "Chain Depth: " & Get_Chain_Depth (Error)'Image);
         
         if Has_Cause (Error) then
            Put_Line ("Root Cause: " & 
                     Error_Strings.To_String (Get_Root_Cause (Error).Message));
         end if;
      end;
   end Database_Operation;
   
   --  Example with resource error context
   procedure Resource_Error_Example is
      Error : Enhanced_Error;
   begin
      Put_Line (ASCII.LF & "=== RESOURCE ERROR EXAMPLE ===" & ASCII.LF);
      
      Error := Make_Enhanced_Error
                 (Category => Resource_Error,
                  Severity => Warning,
                  Message => "Memory usage approaching limit")
                 .With_Context ("component", "image_processor")
                 .With_Context ("action", "resize_batch")
                 .With_Context ("resource_usage", "85")
                 .With_Context ("resource_limit", "100")
                 .With_Context ("wait_time_ms", "1500")
                 .With_Context ("retry_attempts", "3");
      
      Put_Line ("Resource Usage: " & Error.Get_Context ("resource_usage", "0") & "%");
      Put_Line ("Wait Time: " & Error.Get_Context ("wait_time_ms", "0") & "ms");
      Put_Line ("Retry Attempts: " & Error.Get_Context ("retry_attempts", "0"));
   end Resource_Error_Example;
   
   --  Example showing error analysis
   procedure Error_Analysis_Example is
      Error1 : constant Enhanced_Error := 
        Make_Enhanced_Error (Validation_Error, Error, "Invalid email format")
        .With_Context ("field", "email")
        .With_Context ("value", "not-an-email");
      
      Error2 : constant Enhanced_Error :=
        Make_Enhanced_Error (Validation_Error, Error, "Invalid phone format")
        .With_Context ("field", "phone")
        .With_Context ("value", "123");
   begin
      Put_Line (ASCII.LF & "=== ERROR ANALYSIS ===" & ASCII.LF);
      
      Put_Line ("Error 1 matches Validation_Error pattern: " & 
                Matches_Pattern (Error1, Validation_Error)'Image);
      Put_Line ("Error 1 matches 'email' pattern: " & 
                Matches_Pattern (Error1, Validation_Error, "email")'Image);
      Put_Line ("Error 1 matches 'phone' pattern: " & 
                Matches_Pattern (Error1, Validation_Error, "phone")'Image);
      
      Put_Line (ASCII.LF & "Fingerprints for deduplication:");
      Put_Line ("Error 1: " & Get_Fingerprint (Error1));
      Put_Line ("Error 2: " & Get_Fingerprint (Error2));
   end Error_Analysis_Example;

begin
   Put_Line ("=== ENHANCED ERROR HANDLING EXAMPLES ===");
   
   --  Example 1: Basic error with context
   if not Process_File ("/invalid/path", "user-123") then
      Put_Line (ASCII.LF & "File processing failed!");
   end if;
   
   --  Example 2: Error with cause chain
   Database_Operation;
   
   --  Example 3: Specialized error types
   Resource_Error_Example;
   
   --  Example 4: Error analysis
   Error_Analysis_Example;
   
   Put_Line (ASCII.LF & "=== END OF EXAMPLES ===");
end Enhanced_Errors_Example;
```

## 3. Domain Modeling

### Creating Entities

```ada
pragma Ada_2022;

with Abohlib.Core.Domain.Value_Objects.Type_Safe_Generic_Id;
with Abohlib.Core.Domain.Value_Objects.Common_Ids;
with Abohlib.Core.Domain.Result;
with Abohlib.Core.Domain.Errors;
with Ada.Calendar; use Ada.Calendar;

package Customer_Entity is
   use Abohlib.Core.Domain.Value_Objects.Common_Ids;
   use Abohlib.Core.Domain.Errors;
   
   type Customer is private;
   
   package Customer_Result is new Abohlib.Core.Domain.Result.Result_Package
     (Ok_Type => Customer,
      Err_Type => Domain_Error);
   use Customer_Result;
   
   function Create 
     (Name : String;
      Email : String) return Result;
      
   function Get_ID (C : Customer) return Customer_ID;
   function Get_Name (C : Customer) return String;
   function Get_Email (C : Customer) return String;
   function Get_Created_At (C : Customer) return Time;
   
private
   type Customer is record
      ID : Customer_ID;
      Name : String (1 .. 100);
      Name_Length : Natural;
      Email : String (1 .. 255);
      Email_Length : Natural;
      Created_At : Time;
   end record;
end Customer_Entity;
```

### Aggregate Roots

```ada
pragma Ada_2022;

with Abohlib.Core.Domain.Aggregates.Aggregate_Root;
with Abohlib.Core.Domain.Events;
with Customer_Entity; use Customer_Entity;

package Order_Aggregate is
   use Abohlib.Core.Domain.Events;
   
   -- Define events
   type Order_Created is new Domain_Event with record
      Customer_ID : Customer_Entity.Customer_ID;
      Total_Amount : Natural;
   end record;
   
   type Order_Item_Added is new Domain_Event with record
      Product_ID : String (1 .. 30);
      Quantity : Positive;
      Price : Natural;
   end record;
   
   -- Define aggregate
   type Order is new Aggregate_Root with private;
   
   function Create (Customer : Customer_Entity.Customer) return Order;
   
   procedure Add_Item 
     (O : in out Order;
      Product_ID : String;
      Quantity : Positive;
      Price : Natural);
      
private
   type Order is new Aggregate_Root with record
      Customer_ID : Customer_Entity.Customer_ID;
      Total_Amount : Natural := 0;
      Item_Count : Natural := 0;
   end record;
end Order_Aggregate;
```

### Domain Events

```ada
pragma Ada_2022;

with Abohlib.Core.Domain.Events;
with Ada.Text_IO; use Ada.Text_IO;

procedure Domain_Events_Example is
   use Abohlib.Core.Domain.Events;
   
   -- Event handler
   procedure Handle_Order_Event (Event : Domain_Event'Class) is
   begin
      Put_Line ("Event: " & Event.Event_Name);
      Put_Line ("Aggregate: " & Event.Aggregate_ID);
      Put_Line ("Time: " & Event.Occurred_At'Image);
   end Handle_Order_Event;
   
   -- Create event dispatcher
   Dispatcher : Event_Dispatcher;
begin
   -- Subscribe to events
   Dispatcher.Subscribe ("OrderCreated", Handle_Order_Event'Access);
   
   -- Publish event
   declare
      Event : constant Order_Created := 
        (Event_ID => Generate_Event_ID,
         Aggregate_ID => "ORD-123",
         Occurred_At => Clock,
         Customer_ID => New_Customer_ID,
         Total_Amount => 100);
   begin
      Dispatcher.Publish (Event);
   end;
end Domain_Events_Example;
```

## 4. Repository Pattern

### Basic Repository

```ada
pragma Ada_2022;

with Abohlib.Core.Domain.Repositories.Generic_Repository;
with Customer_Entity; use Customer_Entity;

package Customer_Repository is
   use Abohlib.Core.Domain.Value_Objects.Common_Ids;
   
   -- Instantiate generic repository
   package Customer_Repo is new 
      Abohlib.Core.Domain.Repositories.Generic_Repository
        (Entity_Type => Customer,
         Id_Type => Customer_ID);
         
   -- Create in-memory implementation
   type In_Memory_Customer_Repository is 
      new Customer_Repo.Repository_Interface with private;
      
   overriding function Save 
     (Repo : in out In_Memory_Customer_Repository;
      Entity : Customer) return Customer_Repo.Id_Result;
      
   overriding function Find_By_Id
     (Repo : In_Memory_Customer_Repository;
      ID : Customer_ID) return Customer_Repo.Entity_Result;
      
private
   type Customer_Array is array (1 .. 100) of Customer;
   
   type In_Memory_Customer_Repository is 
      new Customer_Repo.Repository_Interface with record
         Customers : Customer_Array;
         Count : Natural := 0;
      end record;
end Customer_Repository;
```

### Unit of Work

```ada
pragma Ada_2022;

with Abohlib.Core.Domain.Repositories.Unit_Of_Work;
with Customer_Repository; use Customer_Repository;

procedure Unit_Of_Work_Example is
   use Abohlib.Core.Domain.Repositories.Unit_Of_Work;
   
   UoW : Unit_Of_Work;
   Repo : In_Memory_Customer_Repository;
   
   -- Create customers
   Customer_1 : constant Customer := Create ("Alice", "alice@example.com").Get_Ok;
   Customer_2 : constant Customer := Create ("Bob", "bob@example.com").Get_Ok;
begin
   -- Begin transaction
   UoW.Begin_Transaction;
   
   -- Register new entities
   UoW.Register_New (Customer_1);
   UoW.Register_New (Customer_2);
   
   -- Commit changes
   declare
      Result : constant Commit_Result := UoW.Commit (Repo);
   begin
      if Result.Is_Ok then
         Put_Line ("Transaction committed successfully");
      else
         Put_Line ("Transaction failed: " & Result.Get_Err.Message);
      end if;
   end;
end Unit_Of_Work_Example;
```

## 5. Pipeline Processing

### Simple Pipeline

```ada
pragma Ada_2022;

with Abohlib.Core.Domain.Services.Generic_Pipeline_Stage;
with Ada.Text_IO; use Ada.Text_IO;

procedure Simple_Pipeline is
   -- Define pipeline configuration
   type Config_Type is record
      Multiplier : Integer;
   end record;
   
   -- Define state type
   type State_Type is record
      Processed_Count : Natural := 0;
   end record;
   
   -- Processing function
   function Process_Element
     (State : in out State_Type;
      Config : Config_Type;
      Input : Integer) return Integer is
   begin
      State.Processed_Count := State.Processed_Count + 1;
      return Input * Config.Multiplier;
   end Process_Element;
   
   -- Instantiate pipeline stage
   package Integer_Pipeline is new 
      Abohlib.Core.Domain.Services.Generic_Pipeline_Stage
        (Input_Type => Integer,
         Output_Type => Integer,
         State_Type => State_Type,
         Config_Type => Config_Type,
         Process_Element => Process_Element);
         
   use Integer_Pipeline;
   
   Stage : Pipeline_Stage;
   Config : constant Config_Type := (Multiplier => 2);
   Result : constant Process_Result := Stage.Process (Config, 21);
begin
   if Result.Is_Ok then
      Put_Line ("Result: " & Result.Get_Ok'Image);
   end if;
   
   Put_Line ("Processed: " & Stage.Get_Statistics.Items_Processed'Image);
end Simple_Pipeline;
```

### Batch Processing

```ada
pragma Ada_2022;

with Abohlib.Core.Domain.Services.Generic_Pipeline_Stage;
with Ada.Text_IO; use Ada.Text_IO;

procedure Batch_Processing is
   -- [Previous type definitions from Simple Pipeline]
   
   package Integer_Pipeline is new 
      Abohlib.Core.Domain.Services.Generic_Pipeline_Stage
        (Input_Type => Integer,
         Output_Type => Integer,
         State_Type => State_Type,
         Config_Type => Config_Type,
         Process_Element => Process_Element);
         
   use Integer_Pipeline;
   
   Stage : Pipeline_Stage;
   Config : constant Config_Type := (Multiplier => 3);
   
   -- Input batch
   Inputs : constant Integer_Array := (1, 2, 3, 4, 5);
   
   -- Process batch
   Result : constant Batch_Process_Result := 
      Stage.Process_Batch (Config, Inputs, Parallel => True);
begin
   Put_Line ("Successful: " & Result.Success_Count'Image);
   Put_Line ("Failed: " & Result.Error_Count'Image);
   
   -- Display results
   for I in Result.Outputs'Range loop
      if Result.Success_Flags (I) then
         Put_Line ("Output " & I'Image & ": " & 
                   Result.Outputs (I)'Image);
      else
         Put_Line ("Error " & I'Image & ": " & 
                   Result.Errors (I).Message);
      end if;
   end loop;
end Batch_Processing;
```

## 6. Testing

### Unit Tests

```ada
pragma Ada_2022;

with Abohlib.Infrastructure.Testing.Test_Framework;
with Customer_Entity; use Customer_Entity;

package Customer_Tests is
   use Abohlib.Infrastructure.Testing.Test_Framework;
   
   type Customer_Test_Suite is new Test_Suite with null record;
   
   overriding function Name 
     (Suite : Customer_Test_Suite) return String is
     ("Customer Entity Tests");
   
   procedure Test_Create_Valid_Customer (Output : Test_Output_Access);
   procedure Test_Create_Invalid_Email (Output : Test_Output_Access);
   procedure Test_Customer_Equality (Output : Test_Output_Access);
   
   function Create_Suite return Test_Suite_Access;
end Customer_Tests;

package body Customer_Tests is
   procedure Test_Create_Valid_Customer (Output : Test_Output_Access) is
      Result : constant Customer_Result.Result := 
         Create ("John Doe", "john@example.com");
   begin
      Assert_True (Output, Result.Is_Ok, 
                   "Valid customer should be created");
      
      if Result.Is_Ok then
         declare
            C : constant Customer := Result.Get_Ok;
         begin
            Assert_Equals (Output, Get_Name (C), "John Doe",
                          "Customer name should match");
            Assert_Equals (Output, Get_Email (C), "john@example.com",
                          "Customer email should match");
         end;
      end if;
   end Test_Create_Valid_Customer;
   
   -- [Other test implementations...]
   
   function Create_Suite return Test_Suite_Access is
      Suite : constant access Customer_Test_Suite := 
         new Customer_Test_Suite;
   begin
      Suite.Add_Test ("Create Valid Customer", 
                      Test_Create_Valid_Customer'Access);
      Suite.Add_Test ("Create Invalid Email", 
                      Test_Create_Invalid_Email'Access);
      Suite.Add_Test ("Customer Equality", 
                      Test_Customer_Equality'Access);
      return Test_Suite_Access (Suite);
   end Create_Suite;
end Customer_Tests;
```

### Contract Tests

```ada
pragma Ada_2022;

with Abohlib.Infrastructure.Testing.Test_Framework;

package Contract_Tests is
   use Abohlib.Infrastructure.Testing.Test_Framework;
   
   procedure Test_Precondition_Violation (Output : Test_Output_Access);
   procedure Test_Postcondition_Guarantee (Output : Test_Output_Access);
   procedure Test_Type_Invariant (Output : Test_Output_Access);
   
private
   -- Type with contracts
   type Percentage is new Natural range 0 .. 100
     with Type_Invariant => Percentage <= 100;
     
   function Calculate_Discount 
     (Price : Natural; 
      Discount : Percentage) return Natural
     with Pre => Price > 0 and then Discount <= 100,
          Post => Calculate_Discount'Result <= Price;
end Contract_Tests;
```

## 7. Advanced Examples

### Saga Pattern

```ada
pragma Ada_2022;

with Abohlib.Core.Domain.Sagas.Saga_Coordinator;

package Order_Processing_Saga is
   use Abohlib.Core.Domain.Sagas.Saga_Coordinator;
   
   -- Define saga steps
   type Reserve_Inventory_Step is new Saga_Step with record
      Product_ID : String (1 .. 30);
      Quantity : Positive;
   end record;
   
   overriding function Execute 
     (Step : Reserve_Inventory_Step;
      Context : in out Saga_Context) return Step_Result;
      
   overriding function Compensate
     (Step : Reserve_Inventory_Step;
      Context : in out Saga_Context) return Step_Result;
      
   -- Other steps: Charge_Payment_Step, Ship_Order_Step, etc.
   
   -- Create saga
   function Create_Order_Saga return Saga_Access;
end Order_Processing_Saga;
```

### Event Sourcing

```ada
pragma Ada_2022;

with Abohlib.Core.Domain.Events;
with Abohlib.Core.Domain.Aggregates.Aggregate_Root;

package Account_Aggregate is
   use Abohlib.Core.Domain.Events;
   
   -- Events
   type Account_Opened is new Domain_Event with record
      Initial_Balance : Natural;
   end record;
   
   type Money_Deposited is new Domain_Event with record
      Amount : Positive;
   end record;
   
   type Money_Withdrawn is new Domain_Event with record
      Amount : Positive;
   end record;
   
   -- Aggregate
   type Account is new Aggregate_Root with private;
   
   -- Event sourced operations
   function Open_Account 
     (Initial_Balance : Natural) return Account;
     
   procedure Deposit 
     (A : in out Account; Amount : Positive);
     
   procedure Withdraw 
     (A : in out Account; Amount : Positive);
     
   -- Reconstruction from events
   function Reconstruct_From_Events 
     (Events : Event_Array) return Account;
     
private
   type Account is new Aggregate_Root with record
      Balance : Natural := 0;
   end record;
   
   -- Event handlers
   procedure Apply_Account_Opened 
     (A : in out Account; Event : Account_Opened);
   procedure Apply_Money_Deposited 
     (A : in out Account; Event : Money_Deposited);
   procedure Apply_Money_Withdrawn 
     (A : in out Account; Event : Money_Withdrawn);
end Account_Aggregate;
```

---

## Running the Examples

All examples can be compiled and run using:

```bash
# For standalone examples
gnatmake -P abohlib.gpr example_name.adb

# For examples using the library
alr with abohlib
alr build
gnatmake -P your_project.gpr example_name.adb
```

## See Also

- [Getting Started Guide](GETTING_STARTED.md) - Installation and setup
- [Result Pattern Guide](guides/RESULT_PATTERN_GUIDE.md) - Deep dive into error handling
- [Testing Guide](TESTING_GUIDE.md) - Writing tests for your code
- [Architecture Diagrams](architecture/DIAGRAMS.md) - Visual representation of patterns

---

[← Back to Documentation](README.md)