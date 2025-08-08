# Multi-Language Porting Guide
## Porting Abohlib Architecture to Go and Rust

**Version:** 1.0
**Date:** August 2, 2025
**Purpose:** Guide for porting Abohlib's hybrid architecture patterns to Go and Rust

---

## Table of Contents

1. [Overview](#overview)
2. [Architecture Pattern Mapping](#architecture-pattern-mapping)
3. [Go Implementation Guide](#go-implementation-guide)
4. [Rust Implementation Guide](#rust-implementation-guide)
5. [Language-Specific Adaptations](#language-specific-adaptations)
6. [Implementation Checklist](#implementation-checklist)

---

## Overview

Abohlib's hybrid architecture (DDD + Clean Architecture + Hexagonal Architecture) can be successfully ported to Go and Rust with language-specific adaptations. This guide provides implementation strategies for core patterns.

### Core Patterns to Port
- **Result Pattern** for functional error handling
- **Type-Safe IDs** using language-specific type safety mechanisms
- **Repository Pattern** with ports/adapters
- **Generic Pipeline Stages** for processing workflows
- **Event Sourcing** with domain events
- **Saga Pattern** for distributed transactions

---

## Architecture Pattern Mapping

### Layer Structure (Universal)
```
┌─────────────────────────────────────────────────────────────┐
│                    Infrastructure Layer                      │
│  (Adapters: File System, Network, Database, External APIs)  │
├─────────────────────────────────────────────────────────────┤
│                    Application Layer                         │
│      (Use Cases, Application Services, Orchestration)       │
├─────────────────────────────────────────────────────────────┤
│                      Domain Layer                            │
│  (Entities, Value Objects, Domain Services, Repositories)   │
└─────────────────────────────────────────────────────────────┘
```

**Key Principles (Language-Agnostic):**
- Dependencies point inward toward domain
- Domain layer has no external dependencies
- External systems integrate through ports/adapters
- All error handling explicit (no hidden exceptions)

---

## Go Implementation Guide

### 1. Result Pattern Implementation

**Ada Original:**
```ada
package String_Result is new Result_Package
  (Ok_Type => String, Err_Type => String);

function Safe_Divide(A, B : Float) return String_Result.Result;
```

**Go Translation:**
```go
// Generic Result type using Go 1.18+ generics
type Result[T, E any] struct {
    value T
    err   E
    isOk  bool
}

func Ok[T, E any](value T) Result[T, E] {
    var zero E
    return Result[T, E]{value: value, err: zero, isOk: true}
}

func Err[T, E any](err E) Result[T, E] {
    var zero T
    return Result[T, E]{value: zero, err: err, isOk: false}
}

func (r Result[T, E]) IsOk() bool { return r.isOk }
func (r Result[T, E]) IsErr() bool { return !r.isOk }

func (r Result[T, E]) Unwrap() T {
    if !r.isOk {
        panic("called Unwrap on Err result")
    }
    return r.value
}

func (r Result[T, E]) UnwrapErr() E {
    if r.isOk {
        panic("called UnwrapErr on Ok result")
    }
    return r.err
}

// Usage example
func SafeDivide(a, b float64) Result[string, string] {
    if b == 0.0 {
        return Err[string, string]("Division by zero")
    }
    return Ok[string, string](fmt.Sprintf("%f", a/b))
}
```

### 2. Type-Safe IDs

**Ada Original:**
```ada
type User_Category is null record;
package User_ID is new Generic_ID_Type
  (Category => User_Category, Category_Name => "User", Prefix => "usr_");
```

**Go Translation:**
```go
// Use type aliases with underlying types for compile-time safety
type UserID string
type OrderID string
type ProductID string

// Cannot accidentally mix these at compile time
func processUser(id UserID) { /* ... */ }
func processOrder(id OrderID) { /* ... */ }

// Example usage - this would be a compile error:
// processUser(OrderID("order_123")) // Compile error!

// ID generation with ULID
func NewUserID() UserID {
    return UserID("usr_" + ulid.Make().String())
}

func NewOrderID() OrderID {
    return OrderID("ord_" + ulid.Make().String())
}

// ID validation
func (id UserID) IsValid() bool {
    return strings.HasPrefix(string(id), "usr_") && len(id) == 30
}
```

### 3. Repository Pattern

**Ada Original:**
```ada
type Repository_Interface is limited interface;
function Save(Self : Repository_Interface; Entity : Entity_Type)
  return Result is abstract;
```

**Go Translation:**
```go
// Domain port (interface)
type Repository[T Entity, ID comparable] interface {
    Save(ctx context.Context, entity T) Result[ID, error]
    FindByID(ctx context.Context, id ID) Result[T, error]
    FindAll(ctx context.Context) Result[[]T, error]
    Delete(ctx context.Context, id ID) Result[bool, error]
}

// Entity constraint
type Entity interface {
    GetID() interface{}
    IsValid() bool
}

// Infrastructure adapter implementation
type DatabaseUserRepository struct {
    db *sql.DB
}

func (r *DatabaseUserRepository) Save(ctx context.Context, user User) Result[UserID, error] {
    query := "INSERT INTO users (id, email, name) VALUES ($1, $2, $3)"
    _, err := r.db.ExecContext(ctx, query, user.ID, user.Email, user.Name)
    if err != nil {
        return Err[UserID, error](fmt.Errorf("failed to save user: %w", err))
    }
    return Ok[UserID, error](user.ID)
}

func (r *DatabaseUserRepository) FindByID(ctx context.Context, id UserID) Result[User, error] {
    var user User
    query := "SELECT id, email, name FROM users WHERE id = $1"
    err := r.db.QueryRowContext(ctx, query, id).Scan(&user.ID, &user.Email, &user.Name)
    if err != nil {
        if errors.Is(err, sql.ErrNoRows) {
            return Err[User, error](fmt.Errorf("user not found: %s", id))
        }
        return Err[User, error](fmt.Errorf("failed to find user: %w", err))
    }
    return Ok[User, error](user)
}
```

### 4. Generic Pipeline Stage

**Go Translation:**
```go
type PipelineStage[Input, Output, State, Config any] struct {
    config    Config
    state     State
    processor func(State, Config, Input) Result[Output, error]
}

func NewPipelineStage[Input, Output, State, Config any](
    config Config,
    initialState State,
    processor func(State, Config, Input) Result[Output, error],
) *PipelineStage[Input, Output, State, Config] {
    return &PipelineStage[Input, Output, State, Config]{
        config:    config,
        state:     initialState,
        processor: processor,
    }
}

func (p *PipelineStage[Input, Output, State, Config]) Process(input Input) Result[Output, error] {
    return p.processor(p.state, p.config, input)
}

func (p *PipelineStage[Input, Output, State, Config]) ProcessBatch(inputs []Input) ([]Result[Output, error], error) {
    results := make([]Result[Output, error], len(inputs))
    for i, input := range inputs {
        results[i] = p.Process(input)
    }
    return results, nil
}

// Parallel processing using goroutines
func (p *PipelineStage[Input, Output, State, Config]) ProcessParallel(inputs []Input, workers int) ([]Result[Output, error], error) {
    if workers <= 0 {
        workers = runtime.NumCPU()
    }

    inputCh := make(chan struct{ index int; input Input })
    resultCh := make(chan struct{ index int; result Result[Output, error] })

    // Start workers
    for i := 0; i < workers; i++ {
        go func() {
            for item := range inputCh {
                result := p.Process(item.input)
                resultCh <- struct{ index int; result Result[Output, error] }{item.index, result}
            }
        }()
    }

    // Send inputs
    go func() {
        defer close(inputCh)
        for i, input := range inputs {
            inputCh <- struct{ index int; input Input }{i, input}
        }
    }()

    // Collect results
    results := make([]Result[Output, error], len(inputs))
    for i := 0; i < len(inputs); i++ {
        result := <-resultCh
        results[result.index] = result.result
    }

    return results, nil
}
```

### Go-Specific Considerations

**Strengths:**
- ✅ Simple, readable syntax
- ✅ Excellent concurrency with goroutines
- ✅ Good generics support (Go 1.18+)
- ✅ Built-in testing framework
- ✅ Strong ecosystem and tooling

**Challenges:**
- ⚠️ No built-in Result type (need custom implementation)
- ⚠️ Limited compile-time safety compared to Ada/Rust
- ⚠️ Garbage collector may affect predictable performance
- ⚠️ Interface{} for generic constraints less expressive

**Adaptations Needed:**
1. Implement custom Result type with generics
2. Use type aliases for ID safety
3. Leverage interfaces for ports/adapters
4. Use goroutines for parallel processing
5. Context package for cancellation and timeouts

---

## Rust Implementation Guide

### 1. Result Pattern Implementation

**Ada Original → Rust Translation:**

Rust has native `Result<T, E>` - no custom implementation needed!

```rust
// Rust's built-in Result type is perfect
fn safe_divide(a: f64, b: f64) -> Result<String, String> {
    if b == 0.0 {
        Err("Division by zero".to_string())
    } else {
        Ok(format!("{}", a / b))
    }
}

// Usage with pattern matching
match safe_divide(10.0, 2.0) {
    Ok(result) => println!("Result: {}", result),
    Err(error) => println!("Error: {}", error),
}

// Or with combinators
let result = safe_divide(10.0, 2.0)
    .map(|s| format!("Success: {}", s))
    .unwrap_or_else(|e| format!("Failed: {}", e));
```

### 2. Type-Safe IDs with Zero-Cost Abstractions

**Rust Translation:**
```rust
use std::marker::PhantomData;
use ulid::Ulid;

// Phantom type approach for compile-time safety
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypedId<T> {
    id: Ulid,
    _phantom: PhantomData<T>,
}

impl<T> TypedId<T> {
    pub fn new() -> Self {
        Self {
            id: Ulid::new(),
            _phantom: PhantomData,
        }
    }

    pub fn from_str(s: &str) -> Result<Self, ulid::DecodeError> {
        Ok(Self {
            id: Ulid::from_string(s)?,
            _phantom: PhantomData,
        })
    }

    pub fn to_string(&self) -> String {
        self.id.to_string()
    }
}

// Zero-cost type safety with phantom types
pub struct User;
pub struct Order;
pub struct Product;

pub type UserId = TypedId<User>;
pub type OrderId = TypedId<Order>;
pub type ProductId = TypedId<Product>;

// This prevents mixing IDs at compile time
fn process_user(id: UserId) { /* ... */ }
fn process_order(id: OrderId) { /* ... */ }

// This would be a compile error:
// process_user(OrderId::new()); // Compile error!

// With prefixes for human readability
impl TypedId<User> {
    pub fn to_prefixed_string(&self) -> String {
        format!("usr_{}", self.id)
    }
}

impl TypedId<Order> {
    pub fn to_prefixed_string(&self) -> String {
        format!("ord_{}", self.id)
    }
}
```

### 3. Repository Pattern with Traits

**Rust Translation:**
```rust
use async_trait::async_trait;
use std::collections::HashMap;
use tokio::sync::RwLock;

// Domain port (trait)
#[async_trait]
pub trait Repository<T, ID>: Send + Sync
where
    T: Send + Sync,
    ID: Send + Sync + Clone,
{
    type Error: std::error::Error + Send + Sync;

    async fn save(&self, entity: T) -> Result<ID, Self::Error>;
    async fn find_by_id(&self, id: ID) -> Result<Option<T>, Self::Error>;
    async fn find_all(&self) -> Result<Vec<T>, Self::Error>;
    async fn delete(&self, id: ID) -> Result<bool, Self::Error>;
    async fn exists(&self, id: ID) -> Result<bool, Self::Error>;
}

// Example entity
#[derive(Debug, Clone)]
pub struct User {
    pub id: UserId,
    pub email: String,
    pub name: String,
}

// Infrastructure adapter implementation
pub struct InMemoryUserRepository {
    storage: RwLock<HashMap<UserId, User>>,
}

impl InMemoryUserRepository {
    pub fn new() -> Self {
        Self {
            storage: RwLock::new(HashMap::new()),
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum RepositoryError {
    #[error("Entity not found")]
    NotFound,
    #[error("Storage error: {0}")]
    StorageError(String),
}

#[async_trait]
impl Repository<User, UserId> for InMemoryUserRepository {
    type Error = RepositoryError;

    async fn save(&self, entity: User) -> Result<UserId, Self::Error> {
        let mut storage = self.storage.write().await;
        let id = entity.id.clone();
        storage.insert(id.clone(), entity);
        Ok(id)
    }

    async fn find_by_id(&self, id: UserId) -> Result<Option<User>, Self::Error> {
        let storage = self.storage.read().await;
        Ok(storage.get(&id).cloned())
    }

    async fn find_all(&self) -> Result<Vec<User>, Self::Error> {
        let storage = self.storage.read().await;
        Ok(storage.values().cloned().collect())
    }

    async fn delete(&self, id: UserId) -> Result<bool, Self::Error> {
        let mut storage = self.storage.write().await;
        Ok(storage.remove(&id).is_some())
    }

    async fn exists(&self, id: UserId) -> Result<bool, Self::Error> {
        let storage = self.storage.read().await;
        Ok(storage.contains_key(&id))
    }
}
```

### 4. Generic Pipeline Stage

**Rust Translation:**
```rust
use std::sync::Arc;
use tokio::task::JoinSet;

pub struct PipelineStage<Input, Output, State, Config> {
    config: Config,
    state: Arc<RwLock<State>>,
    processor: Arc<dyn Fn(&State, &Config, Input) -> Result<Output, Box<dyn std::error::Error + Send + Sync>> + Send + Sync>,
}

impl<Input, Output, State, Config> PipelineStage<Input, Output, State, Config>
where
    Input: Send + 'static,
    Output: Send + 'static,
    State: Send + Sync + 'static,
    Config: Send + Sync + 'static,
{
    pub fn new<F>(config: Config, initial_state: State, processor: F) -> Self
    where
        F: Fn(&State, &Config, Input) -> Result<Output, Box<dyn std::error::Error + Send + Sync>> + Send + Sync + 'static,
    {
        Self {
            config,
            state: Arc::new(RwLock::new(initial_state)),
            processor: Arc::new(processor),
        }
    }

    pub async fn process(&self, input: Input) -> Result<Output, Box<dyn std::error::Error + Send + Sync>> {
        let state = self.state.read().await;
        (self.processor)(&*state, &self.config, input)
    }

    pub async fn process_batch(&self, inputs: Vec<Input>) -> Vec<Result<Output, Box<dyn std::error::Error + Send + Sync>>> {
        let mut results = Vec::with_capacity(inputs.len());
        for input in inputs {
            results.push(self.process(input).await);
        }
        results
    }

    pub async fn process_parallel(&self, inputs: Vec<Input>) -> Vec<Result<Output, Box<dyn std::error::Error + Send + Sync>>> {
        let mut join_set = JoinSet::new();

        for input in inputs {
            let state = self.state.clone();
            let config = &self.config as *const Config;
            let processor = self.processor.clone();

            join_set.spawn(async move {
                let state = state.read().await;
                // Safe because we know config lives long enough
                let config = unsafe { &*config };
                processor(&*state, config, input)
            });
        }

        let mut results = Vec::new();
        while let Some(result) = join_set.join_next().await {
            match result {
                Ok(process_result) => results.push(process_result),
                Err(join_error) => results.push(Err(Box::new(join_error) as Box<dyn std::error::Error + Send + Sync>)),
            }
        }

        results
    }
}

// Usage example
async fn example_usage() -> Result<(), Box<dyn std::error::Error>> {
    // Define processing function
    let processor = |_state: &i32, _config: &String, input: i32| -> Result<String, Box<dyn std::error::Error + Send + Sync>> {
        Ok(format!("Processed: {}", input * 2))
    };

    let stage = PipelineStage::new("config".to_string(), 0, processor);

    // Process single item
    let result = stage.process(5).await?;
    println!("{}", result); // "Processed: 10"

    // Process in parallel
    let inputs = vec![1, 2, 3, 4, 5];
    let results = stage.process_parallel(inputs).await;

    for result in results {
        match result {
            Ok(output) => println!("{}", output),
            Err(error) => eprintln!("Error: {}", error),
        }
    }

    Ok(())
}
```

### Rust-Specific Considerations

**Strengths:**
- ✅ Native Result<T, E> type with excellent ergonomics
- ✅ Zero-cost abstractions with compile-time guarantees
- ✅ Powerful type system with traits and generics
- ✅ Memory safety without garbage collection
- ✅ Excellent async/await support
- ✅ Pattern matching for Result handling

**Challenges:**
- ⚠️ Steeper learning curve (ownership, lifetimes)
- ⚠️ More complex async trait implementations
- ⚠️ Compilation times can be longer
- ⚠️ Some patterns require different approaches due to ownership

**Adaptations Needed:**
1. Use traits instead of Ada interfaces
2. Leverage ownership system for memory management
3. Use Arc/Mutex for shared state in concurrent scenarios
4. async/await for non-blocking operations
5. Pattern matching for Result handling

---

## Language-Specific Adaptations

### Error Handling Philosophy

| **Language** | **Approach** | **Abohlib Mapping** |
|--------------|--------------|-------------------|
| **Ada** | Result types, no exceptions across boundaries | ✅ Direct implementation |
| **Go** | Multiple return values, explicit error checking | ⚠️ Custom Result type recommended |
| **Rust** | Native Result<T,E>, ? operator, pattern matching | ✅ Perfect match |

### Concurrency Models

| **Language** | **Model** | **Adaptation Strategy** |
|--------------|-----------|------------------------|
| **Ada** | Tasks, protected objects | Use for pipeline parallelism |
| **Go** | Goroutines, channels | Map to pipeline workers and communication |
| **Rust** | async/await, tokio tasks | Use for non-blocking pipeline processing |

### Type Safety Approaches

| **Language** | **Mechanism** | **ID Safety Implementation** |
|--------------|---------------|----------------------------|
| **Ada** | Phantom types, generic packages | Direct translation possible |
| **Go** | Type aliases, interfaces | Good approximation with type aliases |
| **Rust** | Phantom types, zero-cost abstractions | Perfect translation with PhantomData |

---

## Implementation Checklist

### Phase 1: Core Patterns
- [ ] **Result Pattern**
  - [ ] Go: Custom generic Result implementation
  - [ ] Rust: Use native Result<T,E>
- [ ] **Type-Safe IDs**
  - [ ] Go: Type aliases with validation
  - [ ] Rust: PhantomData-based TypedId
- [ ] **Basic Repository Pattern**
  - [ ] Go: Interface-based ports
  - [ ] Rust: Trait-based ports

### Phase 2: Domain Layer
- [ ] **Value Objects**
  - [ ] Go: Struct types with validation methods
  - [ ] Rust: Struct types with associated functions
- [ ] **Domain Events**
  - [ ] Go: Interface-based events with timestamp
  - [ ] Rust: Trait-based events with chrono
- [ ] **Aggregates**
  - [ ] Go: Struct with embedded event list
  - [ ] Rust: Struct with Vec<Event> and methods

### Phase 3: Application Layer
- [ ] **Use Cases**
  - [ ] Go: Function-based or struct-based
  - [ ] Rust: Function-based or impl blocks
- [ ] **Pipeline Processing**
  - [ ] Go: Generic pipeline with goroutines
  - [ ] Rust: Generic pipeline with async/await

### Phase 4: Infrastructure Layer
- [ ] **Repository Implementations**
  - [ ] Go: Database, file, memory implementations
  - [ ] Rust: Async database, file, memory implementations
- [ ] **Testing Framework**
  - [ ] Go: Built-in testing with Result assertions
  - [ ] Rust: Built-in testing with Result assertions

### Phase 5: Advanced Patterns
- [ ] **Saga Pattern**
  - [ ] Go: Step interface with goroutine coordination
  - [ ] Rust: Step trait with async coordination
- [ ] **Event Sourcing**
  - [ ] Go: Event store interface implementations
  - [ ] Rust: Async event store trait implementations

---

## Conclusion

The Abohlib architecture is **highly portable** to both Go and Rust:

**Go Suitability: ⭐⭐⭐⭐☆**
- Excellent for microservices and web applications
- Good concurrency model for pipeline processing
- Requires some custom implementations (Result type)
- Great ecosystem and tooling support

**Rust Suitability: ⭐⭐⭐⭐⭐**
- Perfect match for system-level applications
- Native Result type and pattern matching
- Zero-cost abstractions maintain performance
- Excellent type safety and memory management

Both languages can successfully implement the hybrid architecture with their respective strengths, providing type-safe, maintainable, and performant applications following the same design principles established in the Ada implementation.
