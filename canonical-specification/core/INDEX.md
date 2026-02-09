# Core Foundation Index

**Purpose**: Core foundation provides shared domain terminology, entity relationships, and architecture decisions that apply across all features.

---

## Quick Navigation

| Need to... | Read |
|------------|------|
| Understand domain terms | [`foundation/vocabulary.md`](foundation/vocabulary.md) |
| See why route registry was chosen | [`decisions/ADR-001-route-registry-architecture.md`](decisions/ADR-001-route-registry-architecture.md) |
| Understand no-middleware philosophy | [`decisions/ADR-002-no-middleware-by-design.md`](decisions/ADR-002-no-middleware-by-design.md) |
| Learn why validation uses HTTP 422 | [`decisions/ADR-003-http-422-for-validation-errors.md`](decisions/ADR-003-http-422-for-validation-errors.md) |
| Understand Snooze library choice | [`decisions/ADR-004-snooze-as-routing-library.md`](decisions/ADR-004-snooze-as-routing-library.md) |

---

## Contents

### Foundation

| File | Purpose | Lines | Confidence |
|------|---------|-------|------------|
| [`vocabulary.md`](foundation/vocabulary.md) | Canonical terminology and definitions | ~423 | 0.92 |

**Key concepts defined**:
- **Core Domain**: API, Route, URI Pattern, Path Parameter, Route Macro, Handler
- **Request/Response**: Request, Request Context, Request Body, Response, Response Helper
- **Validation**: Validation, Validator, Validation Error, validate Macro
- **JSON**: JSON Serialization, Hash Table (for JSON objects)
- **Database**: Database Connection, Query, Execute, with-db Macro
- **Special Variables**: *request*, *body*, *db*, *route-registry*
- **Error Handling**: Error Response, HTTP Status Codes, Error Format

### Architecture Decisions

| File | Decision | Status | Confidence |
|------|----------|--------|------------|
| [`ADR-001-route-registry-architecture.md`](decisions/ADR-001-route-registry-architecture.md) | Route registry for path parameter extraction | Accepted | 0.95 |
| [`ADR-002-no-middleware-by-design.md`](decisions/ADR-002-no-middleware-by-design.md) | No middleware system by design | Accepted | 0.95 |
| [`ADR-003-http-422-for-validation-errors.md`](decisions/ADR-003-http-422-for-validation-errors.md) | HTTP 422 for validation failures | Accepted | 0.97 |
| [`ADR-004-snooze-as-routing-library.md`](decisions/ADR-004-snooze-as-routing-library.md) | Snooze chosen as routing library | Accepted | 0.97 |

---

## Architecture Decision Summaries

### ADR-001: Route Registry Architecture

**Context**: Need automatic path parameter extraction (e.g., `/users/:id` → `id` variable).

**Decision**: Implement route registry with:
- Hash table mapping (method . uri-pattern) → route metadata
- `parse-uri-pattern` function parsing URI templates
- `match-uri-to-route` function for runtime matching
- Automatic lexical variable binding for path parameters

**Alternatives Rejected**:
- Manual parameter extraction (too verbose)
- Compile-time only routing (less flexible)
- Direct Snooze usage (exposes internals)

**Impact**: Users write clean routes like `(api-get "/users/:id" () (ok :id id))` without manual extraction.

---

### ADR-002: No Middleware By Design

**Context**: Most frameworks provide middleware/interceptor systems.

**Decision**: quickAPI intentionally does NOT provide middleware hooks.

**Rationale**:
- Middleware adds complexity (plugin systems, execution order, error propagation)
- Users needing middleware should use full frameworks
- Philosophy: "5 macros, not 50 abstractions"
- Advanced users can use Snooze/Hunchentoot hooks directly

**Impact**: Keeps codebase small (~1500 LoC). Users wanting middleware must drop to Snooze level.

---

### ADR-003: HTTP 422 for Validation Errors

**Context**: Validation failures could return 400 (Bad Request) or 422 (Unprocessable Entity).

**Decision**: Use HTTP 422 for validation errors.

**Rationale**:
- **HTTP 400**: Malformed request (syntax error, invalid JSON)
- **HTTP 422**: Well-formed but semantically invalid (validation failure)
- Semantic correctness: Request was parsed successfully, data just doesn't meet constraints
- Rails, Django REST Framework, FastAPI use 422

**Impact**: `validate` macro signals `validation-error` condition → HTTP 422 response.

---

### ADR-004: Snooze as Routing Library

**Context**: Need HTTP routing library for quickAPI.

**Decision**: Use Snooze as the routing foundation.

**Criteria Evaluated**:
- Code size (prefer small)
- CLOS integration
- Hunchentoot compatibility
- Active maintenance
- Flexibility

**Alternatives Considered**:
- **Clack**: Too heavyweight, brings own abstractions
- **Caveman**: Full framework, conflicts with philosophy
- **Hunchentoot dispatch**: Too low-level, more boilerplate
- **Custom routing**: Reinventing the wheel

**Why Snooze Won**:
- Small (~850 LoC) - can understand in one sitting
- CLOS-based - clean, extensible
- Hunchentoot integration - works with existing server
- HTTP condition system - proper error handling
- Resource-based routing - good abstraction

**Impact**: quickAPI builds thin layer over Snooze. Users can drop to Snooze when needed.

---

## Dependencies

This layer depends on:
- None (foundation)

This layer is used by:
- All features

---

*Generated on 2026-01-25 from Canon v0.2.0*
