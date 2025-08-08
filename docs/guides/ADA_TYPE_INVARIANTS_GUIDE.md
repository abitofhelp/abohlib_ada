# Ada Type Invariants Guide

## Overview

Type invariants in Ada 2022 provide a powerful mechanism for ensuring that objects of a type maintain certain properties throughout their lifetime. However, there are important limitations to understand when using them with private types.

## The Private Type Limitation

### The Issue

When a type is declared as private in the public part of a package specification, Type_Invariants cannot access the private components of that type. This is because the invariant expression is evaluated in the context where the type is visible, not where it is fully defined.

### Example of What Doesn't Work

```ada
package Example is
   type Counter_Type is private
   with Type_Invariant => 
     Counter_Type.Count >= 0;  -- ERROR: Can't access private component
     
private
   type Counter_Type is record
      Count : Integer := 0;
   end record;
end Example;
```

### Why This Happens

1. **Visibility Rules**: Type_Invariant is part of the public specification
2. **Private Components**: The record components are only visible in the private part
3. **Ada Design**: This prevents breaking encapsulation

## Solutions and Workarounds

### 1. Use Public Functions in Invariants

```ada
package Example is
   type Counter_Type is private
   with Type_Invariant => 
     Get_Count(Counter_Type) >= 0;  -- OK: Uses public function
     
   function Get_Count (C : Counter_Type) return Integer;
     
private
   type Counter_Type is record
      Count : Integer := 0;
   end record;
end Example;
```

### 2. Type_Invariant'Class (for tagged types)

```ada
package Example is
   type Counter_Type is tagged private;
   -- Invariant defined in private part
     
private
   type Counter_Type is tagged record
      Count : Integer := 0;
   end record
   with Type_Invariant'Class => 
     Counter_Type.Count >= 0;  -- OK: In private part
end Example;
```

### 3. Document Invariants

When Type_Invariants cannot be used, document the invariants as comments and ensure they are maintained by the implementation:

```ada
type Buffer_Manager_Type is limited private;
--  Note: Type_Invariant would ensure:
--    Sum of all buffer states = Total_Buffers
--  This invariant is maintained internally by all operations
```

## Examples from Abohlib

### State Machine
```ada
type State_Machine_Type is tagged private;
--  Invariant: Total_Transitions >= Failed_Transitions
--  Maintained internally by Transition_To operation
```

### Object Pool
```ada
type Object_Pool is new Limited_Controlled with private;
--  Invariant: Allocated_Total <= Max_Size
--  Invariant: Available_Count <= Allocated_Total
--  Maintained by Get and Return_To_Pool operations
```

### Buffer Manager
```ada
type Buffer_Manager_Type is limited new Limited_Controlled with private;
--  Invariant: Free + Reading + Ready + Consuming + Error = Total
--  Maintained by protected operations
```

### Ring Buffer
```ada
type Ring_Buffer_Type is new Limited_Controlled with private;
--  Invariant: Size < Buffer_Size (when initialized)
--  Maintained by atomic head/tail operations
```

## Best Practices

1. **Prefer Pre/Post Conditions**: Use preconditions and postconditions on operations to enforce invariants
   ```ada
   function Get (Pool : in out Object_Pool) return Object_Access
   with Post => Allocated_Count(Pool) <= Max_Size;
   ```

2. **Internal Checks**: Add assertions in the implementation to verify invariants
   ```ada
   procedure Internal_Operation (Self : in out Type_Name) is
   begin
      pragma Assert (Self.Count >= 0);
      -- operation code
      pragma Assert (Self.Count >= 0);
   end Internal_Operation;
   ```

3. **Document Thoroughly**: When Type_Invariants can't be used, clearly document what invariants should hold

4. **Test Invariants**: Write specific tests to verify invariants are maintained
   ```ada
   -- Test that buffer count invariant holds
   Assert (Manager.Get_Statistics.Free_Buffers_Count + 
           Manager.Get_Statistics.Ready_Buffers_Count + 
           ... = Buffer_Count);
   ```

## Type Invariant Rules Summary

- **Can Access**: Public functions, public components, discriminants
- **Cannot Access**: Private components (when type declared as private)
- **Checked**: After initialization, before and after public operations
- **Not Checked**: During private operations, during finalization

## Conclusion

While Type_Invariants have limitations with private types, Ada provides multiple mechanisms to ensure type safety:
- Pre/Post conditions on operations
- Static_Predicates for compile-time constraints  
- Dynamic_Predicates for runtime constraints
- Internal assertions and checks
- Careful API design

The key is choosing the right tool for each situation and documenting invariants that cannot be expressed directly in the language.