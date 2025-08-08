# Ada 2022 Ownership Features - Future Implementation Notes

## Overview

Ada 2022 introduces ownership features inspired by Rust's borrow checker, providing compile-time memory safety guarantees. While these features are still experimental in GNAT, this document outlines how Abohlib could leverage them once stable.

## Current Status

As of 2025, Ada 2022 ownership features are:
- Not fully implemented in GNAT
- Subject to change in specification
- Experimental and not recommended for production use

## Potential Applications in Abohlib

### 1. Access Type Safety

Current code that could benefit:
```ada
-- In Generic_SHA256_Hasher
type Hash_Context_Access is access Hash_Context_Type;
```

Future with ownership:
```ada
type Hash_Context_Access is access Hash_Context_Type
  with Ownership => True;
```

### 2. Unique Ownership

For exclusive access patterns:
```ada
-- Unit of Work could enforce unique ownership
type Unit_Of_Work_Access is access all Unit_Of_Work
  with Ownership => Unique;
```

### 3. Borrowing Semantics

For temporary access without ownership transfer:
```ada
procedure Process_With_Borrowed_State
  (State : borrowed State_Type;
   Data  : Input_Type)
with Pre => not null State;
```

### 4. Move Semantics

For efficient resource transfer:
```ada
function Transfer_Ownership
  (From : in out owned Resource_Type) return owned Resource_Type
with Post => From = null;
```

## Benefits When Available

1. **Compile-time Memory Safety**: Prevent use-after-free and double-free errors
2. **Data Race Prevention**: Enforce exclusive mutable access
3. **Resource Leak Prevention**: Compiler ensures proper cleanup
4. **Zero Runtime Cost**: All checks at compile time

## Implementation Strategy

When ownership features stabilize:

1. **Phase 1**: Add ownership annotations to critical access types
   - SHA256 hasher context
   - Repository handles
   - Event store connections

2. **Phase 2**: Refactor resource management
   - Replace manual reference counting
   - Eliminate need for some controlled types
   - Simplify finalization logic

3. **Phase 3**: Full adoption
   - Ownership-based APIs
   - Compile-time enforced RAII
   - Safe concurrent access patterns

## Current Best Practices

Until ownership features are stable, Abohlib uses:
- Controlled types for RAII
- Minimal access type usage
- Clear ownership boundaries through architecture
- Protected types for thread safety

## Monitoring Progress

Track Ada 2022 ownership feature development:
- GNAT release notes
- Ada Reference Manual updates
- AdaCore blog posts
- ISO/IEC 8652:2023 amendments

## Conclusion

While Ada 2022 ownership features show promise for enhanced memory safety, Abohlib currently achieves similar goals through disciplined use of existing Ada features. The architecture is designed to easily adopt ownership features when they become production-ready.