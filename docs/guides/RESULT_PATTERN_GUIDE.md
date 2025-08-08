# Result Pattern Guide

This guide explains the Result pattern, a fundamental error handling mechanism in Abohlib that makes errors explicit and composable.

## Table of Contents

1. [Introduction](#introduction)
2. [Basic Usage](#basic-usage)
3. [Creating Result Types](#creating-result-types)
4. [Working with Results](#working-with-results)
5. [Functional Operations](#functional-operations)
6. [Best Practices](#best-practices)
7. [Common Patterns](#common-patterns)
8. [Migration from Exceptions](#migration-from-exceptions)

## Introduction

The Result pattern is a way to handle errors without using exceptions. Instead of throwing exceptions, functions return a Result type that can be either:
- **Ok**: Contains the successful value
- **Err**: Contains error information

### Why Use Result Pattern?

**Traditional Exception Approach:**
```ada
function Read_Config (Path : String) return Config_Type is
begin
   -- Might raise File_Not_Found, Parse_Error, etc.
   -- Caller has no idea what exceptions to handle!
   return Parse_Config_File (Path);
end Read_Config;
```

**Result Pattern Approach:**
```ada
function Read_Config (Path : String) return Config_Result.Result is
begin
   -- Errors are explicit in the return type
   -- Caller must handle both success and failure
   if not File_Exists (Path) then
      return Config_Result.Err ("File not found: " & Path);
   end if;
   
   return Parse_Config_File (Path);  -- Also returns Result
end Read_Config;
```

**Benefits:**
- Errors are visible in function signatures
- Can't accidentally ignore errors
- Errors can be transformed and combined
- No hidden control flow
- Better for concurrent programming

## Basic Usage

### Simple Example

```ada
with Abohlib.Core.Domain.Errors;
use Abohlib.Core.Domain.Errors;

-- Define a Result type for integers with string errors
package Int_Result is new Result_Package
  (Ok_Type  => Integer,
   Err_Type => Unbounded_String);

-- Function that might fail
function Safe_Divide (A, B : Integer) return Int_Result.Result is
begin
   if B = 0 then
      return Int_Result.Err (To_Unbounded_String ("Division by zero"));
   else
      return Int_Result.Ok (A / B);
   end if;
end Safe_Divide;

-- Using the function
declare
   Result : constant Int_Result.Result := Safe_Divide (10, 2);
begin
   if Result.Is_Ok then
      Put_Line ("Result: " & Result.Get_Ok'Image);
   else
      Put_Line ("Error: " & To_String (Result.Get_Err));
   end if;
end;
```

### Pattern Matching Style

```ada
-- Check result and extract value in one step
case Result.Is_Ok is
   when True =>
      declare
         Value : constant Integer := Result.Get_Ok;
      begin
         Put_Line ("Success: " & Value'Image);
      end;
      
   when False =>
      declare
         Error : constant Unbounded_String := Result.Get_Err;
      begin
         Put_Line ("Failed: " & To_String (Error));
      end;
end case;
```

## Creating Result Types

### Basic Result Types

```ada
-- Result with simple error messages
package String_Result is new Result_Package
  (Ok_Type  => Unbounded_String,
   Err_Type => Unbounded_String);

-- Result with custom error type
type File_Error is (Not_Found, Permission_Denied, Disk_Full);

package File_Result is new Result_Package
  (Ok_Type  => File_Handle,
   Err_Type => File_Error);
```

### Domain-Specific Error Types

```ada
-- Rich error information
type Database_Error is record
   Code    : Error_Code;
   Message : Unbounded_String;
   Query   : Unbounded_String;
   Time    : Ada.Calendar.Time;
end record;

package DB_Result is new Result_Package
  (Ok_Type  => Query_Result,
   Err_Type => Database_Error);
```

### Void Results

For operations that don't return a value:

```ada
-- Using the predefined Void_Result
function Delete_File (Path : String) return Void_Result.Result is
begin
   if not File_Exists (Path) then
      return Void_Result.Err (To_Unbounded_String ("File not found"));
   end if;
   
   -- Perform deletion
   OS_Delete (Path);
   return Void_Result.Ok (True);
end Delete_File;
```

## Working with Results

### Extracting Values Safely

```ada
-- Get value with a default
function Get_Or_Default (R : Int_Result.Result; Default : Integer) return Integer is
begin
   if R.Is_Ok then
      return R.Get_Ok;
   else
      return Default;
   end if;
end Get_Or_Default;

-- Get value or raise exception (use sparingly!)
function Unwrap (R : Int_Result.Result) return Integer is
begin
   if R.Is_Ok then
      return R.Get_Ok;
   else
      raise Program_Error with "Unwrap called on Err: " & To_String (R.Get_Err);
   end if;
end Unwrap;
```

### Early Returns

```ada
function Process_Data (Input : String) return Data_Result.Result is
   -- Validate input
   Validation : constant Void_Result.Result := Validate_Input (Input);
begin
   if Validation.Is_Err then
      return Data_Result.Err (Validation.Get_Err);
   end if;
   
   -- Parse data
   Parse_Result : constant Parsed_Result.Result := Parse (Input);
   if Parse_Result.Is_Err then
      return Data_Result.Err (To_Unbounded_String ("Parse failed: " & 
                                                   To_String (Parse_Result.Get_Err)));
   end if;
   
   -- Process parsed data
   Data : constant Data_Type := Transform (Parse_Result.Get_Ok);
   return Data_Result.Ok (Data);
end Process_Data;
```

## Functional Operations

Abohlib provides functional operations for Result types through the `Result.Functional` package:

### Map - Transform Success Values

```ada
with Abohlib.Core.Domain.Result.Functional;

-- Transform an integer to a string
function Int_To_String is new Map
  (Source_Result => Int_Result,
   Target_Result => String_Result,
   Transform     => Integer'Image);

-- Usage
Original : constant Int_Result.Result := Int_Result.Ok (42);
Mapped   : constant String_Result.Result := Int_To_String (Original);
-- Mapped contains Ok(" 42")
```

### Map_Err - Transform Error Values

```ada
-- Add context to errors
function Add_Context (Err : Unbounded_String) return Error_Info is
begin
   return Error_Info'(
      Message => Err,
      Context => To_Unbounded_String ("During configuration loading"),
      Time    => Clock
   );
end Add_Context;

function Enhance_Error is new Map_Err
  (Source_Result => String_Result,
   Target_Result => Info_Result,
   Transform     => Add_Context);
```

### And_Then - Chain Operations

```ada
-- Chain operations that return Results
function Load_And_Parse (Path : String) return Config_Result.Result is
   
   function Load is new And_Then
     (Source_Result => Path_Result,
      Target_Result => String_Result,
      Operation     => Read_File);
      
   function Parse is new And_Then
     (Source_Result => String_Result,
      Target_Result => Config_Result,
      Operation     => Parse_Config);
      
begin
   return Validate_Path (Path)  -- Returns Path_Result
          |> Load              -- Returns String_Result
          |> Parse;            -- Returns Config_Result
end Load_And_Parse;
```

### Or_Else - Provide Alternatives

```ada
-- Try cache, then database, then default
function Get_User_Preference (Key : String) return Pref_Result.Result is
   
   function Try_Database (E : Unbounded_String) return Pref_Result.Result is
   begin
      return Load_From_Database (Key);
   end Try_Database;
   
   function Use_Default (E : Unbounded_String) return Pref_Result.Result is
   begin
      return Pref_Result.Ok (Default_Preferences);
   end Use_Default;
   
   function From_DB is new Or_Else (Pref_Result, Try_Database);
   function Or_Default is new Or_Else (Pref_Result, Use_Default);
   
begin
   return Load_From_Cache (Key)
          |> From_DB
          |> Or_Default;
end Get_User_Preference;
```

## Best Practices

### 1. Use Descriptive Error Types

```ada
-- Bad: Generic error messages
return Result.Err (To_Unbounded_String ("Error"));

-- Good: Specific error information
type Validation_Error is record
   Field   : Unbounded_String;
   Value   : Unbounded_String;
   Reason  : Unbounded_String;
end record;

return Result.Err (Validation_Error'(
   Field  => To_Unbounded_String ("email"),
   Value  => To_Unbounded_String (Input),
   Reason => To_Unbounded_String ("Invalid format")
));
```

### 2. Fail Fast

```ada
function Process_Order (Order : Order_Type) return Order_Result.Result is
begin
   -- Validate early
   if not Order.Is_Valid then
      return Order_Result.Err (Invalid_Order);
   end if;
   
   if not Has_Inventory (Order.Items) then
      return Order_Result.Err (Insufficient_Stock);
   end if;
   
   -- All validations passed, proceed with processing
   ...
end Process_Order;
```

### 3. Convert Exceptions at Boundaries

```ada
function Safe_File_Operation (Path : String) return File_Result.Result is
begin
   return File_Result.Ok (Perform_Operation (Path));
exception
   when E : IO_Exception =>
      return File_Result.Err (To_Unbounded_String (Exception_Message (E)));
   when others =>
      return File_Result.Err (To_Unbounded_String ("Unknown error"));
end Safe_File_Operation;
```

### 4. Use Type-Specific Results

```ada
-- Define specific Result types for your domain
package User_Result is new Result_Package (User_Type, User_Error);
package Order_Result is new Result_Package (Order_Type, Order_Error);
package Payment_Result is new Result_Package (Payment_Type, Payment_Error);

-- This makes your APIs clear and type-safe
function Find_User (ID : User_ID) return User_Result.Result;
function Place_Order (Order : Order_Type) return Order_Result.Result;
function Process_Payment (Payment : Payment_Type) return Payment_Result.Result;
```

## Common Patterns

### Collecting Multiple Results

```ada
function Validate_All (Items : Item_Array) return Validation_Result.Result is
   Errors : Error_List;
begin
   for Item of Items loop
      declare
         Result : constant Item_Result.Result := Validate_Item (Item);
      begin
         if Result.Is_Err then
            Errors.Append (Result.Get_Err);
         end if;
      end;
   end loop;
   
   if Errors.Is_Empty then
      return Validation_Result.Ok (Items);
   else
      return Validation_Result.Err (Errors);
   end if;
end Validate_All;
```

### Transaction Pattern

```ada
function Transfer_Funds 
  (From, To : Account_ID; 
   Amount : Money_Type) return Transaction_Result.Result 
is
   -- Start transaction
   TX : Transaction := Begin_Transaction;
begin
   -- Debit source account
   declare
      Debit_Result : constant Account_Result.Result := 
         Debit_Account (From, Amount);
   begin
      if Debit_Result.Is_Err then
         Rollback (TX);
         return Transaction_Result.Err (Debit_Result.Get_Err);
      end if;
   end;
   
   -- Credit destination account
   declare
      Credit_Result : constant Account_Result.Result := 
         Credit_Account (To, Amount);
   begin
      if Credit_Result.Is_Err then
         Rollback (TX);
         return Transaction_Result.Err (Credit_Result.Get_Err);
      end if;
   end;
   
   -- Both operations succeeded
   Commit (TX);
   return Transaction_Result.Ok (Create_Receipt (From, To, Amount));
end Transfer_Funds;
```

### Pipeline Pattern

```ada
type Pipeline_Stage is access function (Data : Data_Type) return Data_Result.Result;

function Run_Pipeline 
  (Input : Data_Type; 
   Stages : Stage_Array) return Data_Result.Result 
is
   Current : Data_Result.Result := Data_Result.Ok (Input);
begin
   for Stage of Stages loop
      exit when Current.Is_Err;
      
      if Current.Is_Ok then
         Current := Stage (Current.Get_Ok);
      end if;
   end loop;
   
   return Current;
end Run_Pipeline;
```

## Migration from Exceptions

### Before (Exception-Based)

```ada
function Load_Config (Path : String) return Config_Type is
   File : File_Type;
begin
   Open (File, In_File, Path);
   declare
      Content : constant String := Read_All (File);
   begin
      Close (File);
      return Parse_Config (Content);
   end;
exception
   when Name_Error =>
      raise Config_Error with "File not found: " & Path;
   when Parse_Error =>
      raise Config_Error with "Invalid config format";
   when others =>
      Close (File);
      raise;
end Load_Config;
```

### After (Result-Based)

```ada
function Load_Config (Path : String) return Config_Result.Result is
   
   function Read_File_Content return String_Result.Result is
   begin
      if not Exists (Path) then
         return String_Result.Err ("File not found: " & Path);
      end if;
      
      declare
         Content : constant String := Read_All_Safe (Path);
      begin
         return String_Result.Ok (Content);
      end;
   end Read_File_Content;
   
begin
   -- Read file
   declare
      Content_Result : constant String_Result.Result := Read_File_Content;
   begin
      if Content_Result.Is_Err then
         return Config_Result.Err (Content_Result.Get_Err);
      end if;
      
      -- Parse content
      return Parse_Config (Content_Result.Get_Ok);
   end;
end Load_Config;
```

## Summary

The Result pattern makes error handling:
- **Explicit**: Errors are visible in types
- **Composable**: Chain operations with functional combinators
- **Reliable**: Can't forget to handle errors
- **Testable**: Easy to test both success and failure paths

Start using Result types in your code, and you'll find your programs become more reliable and easier to understand. The initial verbosity pays off in maintainability and correctness.