---
name: quickapi-dev
description: Development guide for quickAPI library contributors
version: 0.1.0
author: Quasilabs
type: dev
---

# quickAPI Development Guide

quickAPI is a curated stack for building JSON APIs in Common Lisp. It composites proven libraries (Clack, Lack, jzon, inquisitio) with thin glue code and zero boilerplate.

## What is quickAPI

A Common Lisp library that eliminates boilerplate from REST API development. Core:
- `defmodel`: Declare data schema → auto-generates CRUD functions + table
- Route macros (`api-get`, `api-post`, etc.): Define endpoints with automatic JSON parsing/serialization
- Auth helpers: JWT, sessions, API keys in one line
- Validation framework: Required fields, types, lengths, ranges, regex patterns

**Key insight**: Not a framework. A thin glue layer over battle-tested libraries (Clack/Lack for HTTP, jzon for JSON, inquisitio for SQLite).

## Quick Reference

### Build & Load
```lisp
;; In REPL
(ql:quickload :quickapi)

;; Run all tests
(asdf:test-system :quickapi)
```

### File Structure
| Module | Purpose |
|--------|---------|
| `src/package.lisp` | Exports, API surface |
| `src/core.lisp` | Route macros, server control, route registry |
| `src/models.lisp` | `defmodel` macro, model metadata, CRUD generation |
| `src/validation.lisp` | Validation framework, error collection |
| `src/response.lisp` | HTTP response helpers (ok, created, bad-request, etc) |
| `src/auth.lisp` | JWT, password hashing, session/API-key helpers |
| `src/sqlite.lisp` | Database connection wrapper |
| `src/lack-app.lisp` | Middleware integration (CORS, logging, sessions) |
| `src/conditions.lisp` | Error condition hierarchy, signaling |
| `src/config.lisp` | .env file support, environment variables |
| `src/deploy.lisp` | Deployment script generators (systemd, smoke tests) |

## Architecture

### Layer Model

```
┌─────────────────────────────────────────┐
│ User Code (defapi, api-get, defmodel)   │  ← Declarative surface
├─────────────────────────────────────────┤
│ Route Registry + Handler Dispatch       │  ← Routing
├─────────────────────────────────────────┤
│ Lack Middleware Stack                   │  ← Cross-cutting (logging, CORS, sessions)
├─────────────────────────────────────────┤
│ Clack HTTP Server (Hunchentoot)         │  ← Transport
├─────────────────────────────────────────┤
│ jzon (JSON), inquisitio (SQLite)        │  ← External libraries
└─────────────────────────────────────────┘
```

### Key Data Structures

**Route Registry**: Internal hash table of method+uri → `route-entry`
- Parsed at macro expansion time
- URI patterns like `/todos/:id` parse into segment lists: `((:literal . "todos") (:param . id))`
- Matching extracts parameters as lexical bindings in route handler

**API Definition** (`api` class):
- Metadata: name, version, description
- Middlewares list: applied in order to request pipeline

**Model Metadata** (`*models*`): Registry of declared models
- Stores field definitions, constraints, CRUD function names
- Used by `migrate-models` to generate SQL

**Request Context** (special variables):
- `*request*`: Current Lack request env (hash table)
- `*body*`: Parsed JSON request body (hash table)
- `*db*`: Current SQLite connection
- `*current-user*`: User object if authenticated

## Rules & Invariants

| Rule | Enforcement |
|------|-------------|
| All routes MUST use one of: `api-get`, `api-post`, `api-put`, `api-delete`, `api-patch` | Macro restriction |
| `*body*` MUST be nil for GET/DELETE requests | Lack middleware parsing |
| `*db*` MUST be bound before any model CRUD operation | Runtime check (would raise db-error) |
| Response MUST be alist or hash-table (auto-serializes to JSON) | Handler contract |
| Validation errors MUST be collected, not signaled immediately | `validate` macro design |
| Parameters in URI patterns (`:id`) are auto-bound as lexical variables | Macro expansion |
| All CRUD functions for a model use consistent naming: `<action>-<modelname>` | `defmodel` macro |

## Testing Strategy

### Test Hierarchy
| Level | Location | Tools | Purpose |
|-------|----------|-------|---------|
| Unit | `tests/validation-tests.lisp`, `tests/response-tests.lisp` | fiveam | Validators, response formatters |
| Integration | `tests/model-tests.lisp`, `tests/sqlite-tests.lisp` | fiveam, real db | CRUD generation, schema |
| API | `tests/integration-tests.lisp` | fiveam, live server | Route matching, auth, serialization |

### Running Tests
```lisp
(asdf:test-system :quickapi)         ; Full suite (preferred)
```

### Test Conventions
- All test suites inherit from `:quickapi-tests` master suite
- Use `fiveam:test` to define test cases (e.g., `(fiveam:test my-test-case ...)`)
- Use `fiveam:is` for assertions inside tests (e.g., `(fiveam:is (= 2 (+ 1 1)))`)
- Setup/teardown via `fiveam:with-fixture` for database isolation
- Naming: `<component>-<scenario>` (e.g., `require-fields-missing-field`)

## Guidelines

### When Adding a Feature

1. **Add tests first** (TDD). Put in appropriate test file (`*-tests.lisp`).
2. **Update exports** in `package.lisp` if public.
3. **Update canonical docs** in `canonical-specification/features/` (e.g., for new validation rule, update `validation/` feature).
4. **Update README** if user-facing.
5. **Run full test suite** before commit.

### Code Style
- Soft tabs (2-space indent) for readability
- Docstrings on exported functions (one line, no prose)
- Comments explain WHY, not WHAT (code is readable)
- Use `eval-when` for compile-time helper functions (e.g., `parse-uri-pattern`)
- Thread special variables (`*request*`, `*body*`) through macros, not global state

### Documentation Conventions
- **Declarative sections first** (what, not how)
- **Implementation details last** (architecture, internals)
- **Links to canonical-specification/** for definitive behavior
- **Code examples must be executable** (verified by `nitpicker` before merge)

## Debugging Tips

**Route not matching?**
Check the route registry (internal data structure) for registered patterns. Run `(asdf:test-system :quickapi)` integration tests to verify routing works.

**JSON parsing failed?**
- Check Content-Type header is `application/json`
- Check `*body*` is nil after parsing (Lack responsibility)

**Model CRUD function not found?**
Ensure `defmodel` was evaluated in the same package as your routes. CRUD functions are interned in the package where `defmodel` runs. Check the exported function names match the expected pattern: `create-<name>`, `find-<name>`, `list-<name>s`, etc.

**Database locked?**
- SQLite locks on write. Use `with-db` for proper connection cleanup.
- Check for hung connections from previous REPL sessions.

## References

| Document | Scope |
|----------|-------|
| `canonical-specification/` | Contains feature specs for auth, routing, models, error handling |
| `README.md` | User-facing quickstart and overview |

See CLAUDE.md for integration skill (using the library).
