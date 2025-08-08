# ULID (Universally Unique Lexicographically Sortable Identifier) Guide

**Version 1.0.0**

## Overview

ULIDs are a modern alternative to UUIDs that provide global uniqueness while being sortable by creation time. Abohlib integrates ULID support for distributed systems that need time-ordered unique identifiers.

## Table of Contents

1. [What is a ULID?](#what-is-a-ulid)
2. [ULID vs UUID](#ulid-vs-uuid)
3. [Using ULIDs in Abohlib](#using-ulids-in-abohlib)
4. [Type-Safe Entity IDs](#type-safe-entity-ids)
5. [Best Practices](#best-practices)
6. [Common Use Cases](#common-use-cases)

## What is a ULID?

A ULID (Universally Unique Lexicographically Sortable Identifier) is a 128-bit identifier with special properties:

### ULID Structure
```
 01AN4Z07BY      79KA1307SR9X4MV3
|----------|    |----------------|
 Timestamp          Randomness
   48bits             80bits
```

### Example ULID
```
01J8Z3N4V5X6Y7W8R9P0Q1S2T3
```

### Key Properties

1. **Time-Ordered**: ULIDs can be sorted by creation time
2. **Globally Unique**: 80 bits of randomness prevent collisions
3. **URL-Safe**: Uses Crockford's base32 encoding
4. **Compact**: 26 characters vs 36 for UUID strings
5. **Monotonic**: Multiple ULIDs in same millisecond maintain order

## ULID vs UUID

### Comparison Table

| Feature | ULID | UUID v4 |
|---------|------|---------|
| Length | 26 chars | 36 chars |
| Sortable | Yes (by time) | No |
| Time component | Yes (48 bits) | No |
| Randomness | 80 bits | 122 bits |
| Database indexing | Efficient | Less efficient |
| Human readable | More readable | Less readable |

### When to Use Each

**Use ULID when:**
- You need time-ordered identifiers
- Database indexing performance matters
- You want to infer creation order from IDs
- You're building event-sourced systems

**Use UUID when:**
- You need maximum randomness
- Time information should not be exposed
- You're interfacing with UUID-only systems
- You need specific UUID versions (v1, v5, etc.)

## Using ULIDs in Abohlib

### Basic ULID Operations

```ada
with Abohlib.Core.Domain.Utilities.ULID_Helpers;
use Abohlib.Core.Domain.Utilities.ULID_Helpers;

-- Generate a new ULID
New_ID : constant ULID_Type := Generate_ULID;

-- Convert to string
ID_String : constant String := To_String (New_ID);
-- Example: "01J8Z3N4V5X6Y7W8R9P0Q1S2T3"

-- Parse from string
Parsed_Result : constant ULID_Result := From_String (ID_String);
if Parsed_Result.Is_Ok then
   Parsed_ID : constant ULID_Type := Parsed_Result.Get_Ok;
end if;

-- Extract timestamp
Timestamp : constant Time := Get_Timestamp (New_ID);
Put_Line ("Created at: " & Image (Timestamp));

-- Compare ULIDs (time-ordered)
if Newer_ID > Older_ID then
   Put_Line ("Newer_ID was created after Older_ID");
end if;
```

### ULID Components

```ada
-- Get individual components
declare
   ID : constant ULID_Type := Generate_ULID;
begin
   -- Timestamp component (milliseconds since epoch)
   Timestamp_MS : constant Unsigned_64 := Get_Timestamp_MS (ID);
   
   -- Random component
   Random_Bytes : constant Random_Array := Get_Random_Part (ID);
   
   -- Human-readable breakdown
   Put_Line ("ULID: " & To_String (ID));
   Put_Line ("Time: " & Image (Get_Timestamp (ID)));
   Put_Line ("Random: " & To_Hex (Random_Bytes));
end;
```

## Type-Safe Entity IDs

Abohlib combines ULIDs with type safety for domain entities:

### Defining Entity ID Types

```ada
with Abohlib.Core.Domain.Value_Objects.Type_Safe_Generic_Id;

-- Define type-safe ID types for your entities
package Customer_ID_Package is new Type_Safe_Generic_Id
  (Tag_Type => Customer_Tag,
   Prefix => "CUS");

package Order_ID_Package is new Type_Safe_Generic_Id
  (Tag_Type => Order_Tag,
   Prefix => "ORD");

package Product_ID_Package is new Type_Safe_Generic_Id
  (Tag_Type => Product_Tag,
   Prefix => "PRD");

-- Use the types
subtype Customer_ID is Customer_ID_Package.ID_Type;
subtype Order_ID is Order_ID_Package.ID_Type;
subtype Product_ID is Product_ID_Package.ID_Type;
```

### Using Type-Safe IDs

```ada
-- Generate new IDs
Customer_1 : constant Customer_ID := Customer_ID_Package.New_ID;
Order_1 : constant Order_ID := Order_ID_Package.New_ID;

-- IDs include readable prefixes
Put_Line (Customer_ID_Package.To_String (Customer_1));
-- Output: "CUS-01J8Z3N4V5X6Y7W8R9P0Q1S2T3"

-- Type safety prevents mixing
-- This won't compile:
-- if Customer_1 = Order_1 then  -- Compilation error!

-- Parse from strings
Input : constant String := "CUS-01J8Z3N4V5X6Y7W8R9P0Q1S2T3";
Parse_Result : constant Customer_ID_Result := 
   Customer_ID_Package.From_String (Input);

if Parse_Result.Is_Ok then
   Customer : constant Customer_ID := Parse_Result.Get_Ok;
   -- Use the parsed ID
end if;
```

### ID Validation

```ada
-- Validate ID format
function Is_Valid_Customer_ID (S : String) return Boolean is
begin
   -- Must start with correct prefix
   if S'Length < 30 or else S (S'First .. S'First + 3) /= "CUS-" then
      return False;
   end if;
   
   -- Validate ULID part
   declare
      ULID_Part : constant String := S (S'First + 4 .. S'Last);
      Result : constant ULID_Result := From_String (ULID_Part);
   begin
      return Result.Is_Ok;
   end;
end Is_Valid_Customer_ID;
```

## Best Practices

### 1. Use Type-Safe Wrappers

Always wrap ULIDs in type-safe domain types:

```ada
-- Don't use raw ULIDs
procedure Process_Order (Order_ID : ULID_Type);  -- Bad

-- Use type-safe IDs
procedure Process_Order (Order_ID : Order_ID_Type);  -- Good
```

### 2. Include Semantic Prefixes

Prefixes make IDs human-readable and debuggable:

```ada
-- Hard to debug
"01J8Z3N4V5X6Y7W8R9P0Q1S2T3"

-- Easy to understand
"USR-01J8Z3N4V5X6Y7W8R9P0Q1S2T3"  -- User
"ORD-01J8Z3N4V5X6Y7W8R9P0Q1S2T3"  -- Order
"INV-01J8Z3N4V5X6Y7W8R9P0Q1S2T3"  -- Invoice
```

### 3. Leverage Time Ordering

Use ULID's time ordering for efficient queries:

```ada
-- Get orders created after a specific time
function Get_Recent_Orders (Since : Time) return Order_Array is
   -- Create a ULID with the target timestamp
   Cutoff_ULID : constant ULID_Type := ULID_From_Time (Since);
   Cutoff_ID : constant Order_ID := Order_ID_From_ULID (Cutoff_ULID);
begin
   -- Database query can use index efficiently
   return Query_Orders_Where ("id > ?", Cutoff_ID);
end Get_Recent_Orders;
```

### 4. Handle Clock Skew

In distributed systems, handle potential clock differences:

```ada
-- Add tolerance for clock skew
function Is_Recent (ID : ULID_Type; 
                   Within : Duration := 5.0) return Boolean is
   ID_Time : constant Time := Get_Timestamp (ID);
   Now : constant Time := Clock;
begin
   -- Allow for clock skew in distributed systems
   return abs (Now - ID_Time) <= Within;
end Is_Recent;
```

## Common Use Cases

### Event Sourcing

ULIDs are perfect for event IDs in event-sourced systems:

```ada
type Domain_Event is abstract tagged record
   Event_ID : Event_ID_Type;        -- ULID-based
   Aggregate_ID : Aggregate_ID_Type;
   Occurred_At : Time;
   Version : Natural;
end record;

-- Events are naturally ordered by ID
function Get_Events_After (Last_Event_ID : Event_ID_Type) 
                          return Event_Array is
begin
   -- Efficient query using ULID ordering
   return Query_Events_Where ("event_id > ?", Last_Event_ID);
end Get_Events_After;
```

### Distributed Tracing

Use ULIDs for trace and span IDs:

```ada
type Trace_Context is record
   Trace_ID : Trace_ID_Type;    -- Root trace
   Span_ID : Span_ID_Type;      -- Current span
   Parent_Span : Span_ID_Type;  -- Parent span
end record;

-- Generate correlated IDs
function Start_Span (Parent : Trace_Context) return Trace_Context is
begin
   return (Trace_ID => Parent.Trace_ID,
           Span_ID => New_Span_ID,     -- New ULID
           Parent_Span => Parent.Span_ID);
end Start_Span;
```

### Audit Logs

Time-ordered IDs simplify audit queries:

```ada
type Audit_Entry is record
   Entry_ID : Audit_ID_Type;     -- ULID for time ordering
   User_ID : User_ID_Type;
   Action : Audit_Action;
   Resource : Resource_ID_Type;
   Details : JSON_Value;
end record;

-- Query audit log by time range
function Get_Audit_Trail (Start_Time, End_Time : Time) 
                         return Audit_Array is
   Start_ID : constant Audit_ID_Type := 
      Audit_ID_From_ULID (ULID_From_Time (Start_Time));
   End_ID : constant Audit_ID_Type := 
      Audit_ID_From_ULID (ULID_From_Time (End_Time));
begin
   return Query_Audit_Where ("entry_id BETWEEN ? AND ?", 
                            Start_ID, End_ID);
end Get_Audit_Trail;
```

### Database Migrations

Migrate from auto-increment to ULIDs:

```ada
-- Migration procedure
procedure Migrate_To_ULIDs is
begin
   -- Add new ULID column
   Execute_SQL ("ALTER TABLE orders ADD COLUMN ulid_id VARCHAR(30)");
   
   -- Populate with ULIDs based on creation time
   for Row in Query ("SELECT id, created_at FROM orders ORDER BY id") loop
      declare
         -- Generate ULID with historical timestamp
         Historical_ULID : constant Order_ID_Type := 
            Order_ID_From_ULID (ULID_From_Time (Row.Created_At));
      begin
         Execute_SQL ("UPDATE orders SET ulid_id = ? WHERE id = ?",
                     To_String (Historical_ULID), Row.ID);
      end;
   end loop;
   
   -- Switch primary key
   Execute_SQL ("ALTER TABLE orders DROP PRIMARY KEY");
   Execute_SQL ("ALTER TABLE orders ADD PRIMARY KEY (ulid_id)");
end Migrate_To_ULIDs;
```

## Performance Considerations

### Database Indexing

ULIDs provide better index locality than random UUIDs:

```sql
-- B-tree indexes work efficiently with ULIDs
CREATE INDEX idx_orders_id ON orders(id);

-- Range queries are fast
SELECT * FROM orders 
WHERE id > 'ORD-01J8Z3N4V5X6Y7W8R9P0Q1S2T3'
ORDER BY id 
LIMIT 100;
```

### Storage Efficiency

Store ULIDs efficiently:

```ada
-- String storage (26 bytes + prefix)
ID_String : String := "CUS-01J8Z3N4V5X6Y7W8R9P0Q1S2T3";

-- Binary storage (16 bytes)
ID_Binary : ULID_Binary := To_Binary (Customer_ID);

-- Database: Use BINARY(16) for storage efficiency
-- Application: Use string format for readability
```

## Summary

ULIDs provide an excellent solution for distributed systems that need:
- Time-ordered unique identifiers
- Efficient database indexing
- Human-readable IDs
- Type-safe entity identification

Combine ULIDs with Abohlib's type-safe ID system for maximum safety and clarity in your domain model.

## See Also

- [Type-Safe IDs Example](../EXAMPLES.md#type-safe-ids)
- [Value Objects Guide](VALUE_OBJECTS_GUIDE.md)
- [Domain Modeling Examples](../EXAMPLES.md#3-domain-modeling)