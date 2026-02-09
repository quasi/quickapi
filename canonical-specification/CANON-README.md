# AGENT.md

**Project**: quickAPI  
**Version**: 0.1.0  
**Language**: Common Lisp  
**Canon Version**: 0.2.0  
**Last Updated**: 2026-01-25

---

## Project Context

quickAPI is a curated REST API framework for Common Lisp that makes building JSON APIs as simple as FastAPI in Python. It's not a heavyweight framework—it's a thin, opinionated layer over proven libraries (Snooze, Hunchentoot, jzon) with excellent documentation and minimal boilerplate.

**Tagline**: Build JSON APIs in Common Lisp with minimal boilerplate

**Target Use Cases**:
- Small-to-medium JSON APIs
- SQLite-backed REST endpoints
- Rapid prototyping and MVP development
- Projects wanting FastAPI-like simplicity in Lisp

**Non-Goals**:
- Heavy framework features (middleware, configuration files)
- Non-JSON content types
- Complex authentication/authorization
- Large-scale enterprise applications

---

## Build Commands

### Load System

```lisp
;; Using Quicklisp
(ql:quickload :quickapi)

;; Or using ASDF
(asdf:load-system :quickapi)
```

### Run Tests

```lisp
;; Load test system
(asdf:load-system :quickapi/tests)

;; Run all tests
(asdf:test-system :quickapi)

;; Or using FiveAM directly
(fiveam:run! :quickapi-tests)
```

**Expected Output**: All 168 tests should pass (100% pass rate as of 2026-01-25)

### Interactive Development

```lisp
;; Start a simple test server
(ql:quickload :quickapi)
(defpackage :my-api (:use :cl :quickapi))
(in-package :my-api)

(defapi my-api
  :name "Test API"
  :version "1.0"
  :description "Testing quickAPI")

(api-get "/hello" ()
  (ok :message "Hello, World!"))

(start :port 8080)

;; Test: curl http://localhost:8080/hello
;; Stop: (stop)
```

---

## Code Conventions

### Naming Conventions

| Element | Convention | Example |
|---------|------------|---------|
| Packages | lowercase, hyphenated | `quickapi`, `quickapi-tests` |
| Functions | lowercase, hyphenated | `parse-uri-pattern`, `match-uri-to-route` |
| Macros | lowercase, hyphenated | `api-get`, `validate`, `with-db` |
| Special variables | `*earmuffs*` | `*request*`, `*body*`, `*db*` |
| Constants | `+plus-signs+` | (not currently used) |
| Structs | lowercase, hyphenated | `route-entry` |

### File Organization

```
src/
├── package.lisp       # Package definitions and exports
├── response.lisp      # Response helpers (ok, created, not-found, etc.)
├── validation.lisp    # Validation framework
├── core.lisp          # Route macros, registry, API definition
└── sqlite.lisp        # Database helpers

tests/
├── package.lisp       # Test package definition
├── response-tests.lisp
├── validation-tests.lisp
├── route-registry-tests.lisp
├── json-parsing-tests.lisp
├── sqlite-tests.lisp
├── integration-tests.lisp
└── tests.lisp         # Test suite runner
```

### ABOUTME Comments

**RULE**: Every source file MUST start with an ABOUTME comment.

**Format**:
```lisp
;;;; ABOUTME: <One-line description of file purpose>
```

**Examples**:
```lisp
;;;; ABOUTME: Response helpers for HTTP status codes and JSON responses
;;;; ABOUTME: Validation framework - simple validation without DSL
;;;; ABOUTME: Route registry and URI pattern matching for path parameters
```

### Hash Tables for JSON

**RULE**: JSON objects MUST use hash tables with `:test 'equal`.

**Correct**:
```lisp
(let ((user (make-hash-table :test 'equal)))
  (setf (gethash "id" user) "123")
  (setf (gethash "name" user) "Alice")
  (ok user))
```

**Violation**:
```lisp
;; WRONG: Default :test 'eql won't work with string keys
(let ((user (make-hash-table)))
  (setf (gethash "id" user) "123")  ; Won't retrieve correctly!
  ...)
```

**Rationale**: JSON object keys are strings. Common Lisp hash tables default to `eql` comparison, which doesn't work for strings. Always use `:test 'equal` for JSON data.

### Special Variables Scoping

**RULE**: Special variables MUST be dynamically bound, never globally mutated.

**Correct**:
```lisp
;; Route macros bind *request* and *body* dynamically
(define-json-route (method uri lambda-list)
  `(let ((*request* ...)
         (*body* ...))
     ,@body))
```

**Violation**:
```lisp
;; WRONG: Don't globally setf special variables
(setf *request* new-request)  ; Breaks concurrent requests!
```

### Error Handling

**RULE**: Use Snooze HTTP conditions for error responses, not explicit response construction.

**Correct**:
```lisp
;; Signal a condition that Snooze converts to HTTP response
(not-found "User not found")

;; Or use validation-error for 422
(signal 'validation-error :errors collected-errors)
```

**Violation**:
```lisp
;; WRONG: Don't manually construct error responses
(list 404 '() (jzon:stringify ...))  ; Bypasses error system
```

---

## Architecture Rules

### RULE: Thin Veneer Over Proven Libraries

quickAPI is NOT a framework. It's glue code connecting proven libraries.

**Correct**: Use underlying libraries directly when quickAPI doesn't provide abstraction
```lisp
;; Access Snooze request object directly
(let ((method (snooze:http-method *request*))
      (uri (snooze:request-uri *request*)))
  ...)
```

**Violation**: Adding heavy abstractions that hide underlying libraries
```lisp
;; WRONG: Don't wrap everything in quickAPI abstractions
(defun quickapi-get-header (name) ...)  ; Just use Snooze!
```

**Rationale**: Philosophy - "5 route macros, not 50 abstractions". Users should be able to drop down to Snooze/Hunchentoot when needed.

### RULE: No Middleware System

quickAPI intentionally does NOT provide a middleware system.

**Rationale**: See `canon/core/decisions/ADR-002-no-middleware-by-design.md`

**Implication**: Don't add middleware hooks, plugin systems, or request/response interceptors. Users who need that should use a full framework.

### RULE: Automatic JSON, No Content Negotiation

All requests and responses are JSON. No content-type negotiation.

**Correct**:
```lisp
;; Routes automatically handle JSON
(api-post "/users" ()
  ;; *body* is already parsed JSON
  (let ((name (gethash "name" *body*)))
    (ok :id 123 :name name)))
```

**Violation**:
```lisp
;; WRONG: Don't add XML, form-data, or other content types
(api-post "/users" ()
  (case (content-type *request*)
    ("application/json" ...)
    ("application/xml" ...)))  ; Out of scope!
```

**Rationale**: Focus on one thing and do it well. JSON-only keeps the codebase small and maintainable.

### RULE: Route Registry is Global Per-Process

The `*route-registry*` is a package-level special variable, not per-API.

**Implication**: Multiple `defapi` calls in the same process share routes.

**Correct**:
```lisp
;; This is fine - routes are global
(defapi api-v1 ...)
(api-get "/users" () ...)

(defapi api-v2 ...)
(api-get "/users" () ...)  ; Overwrites previous!
```

**Recommendation**: Use separate processes or URI prefixes for multiple APIs.

### RULE: HTTP 422 for Validation Errors

Validation failures MUST return HTTP 422 (Unprocessable Entity), not 400.

**Correct**:
```lisp
(validate
  (require-fields *body* "name" "email"))
;; → HTTP 422 on failure
```

**Violation**:
```lisp
;; WRONG: Don't use 400 for validation failures
(unless (gethash "name" *body*)
  (bad-request "Name required"))  ; Should be 422!
```

**Rationale**: See `canon/core/decisions/ADR-003-http-422-for-validation-errors.md`. HTTP 400 is for malformed requests (syntax errors), 422 is for semantic validation failures.

---

## File Locations

| Type | Location | Description |
|------|----------|-------------|
| **Source** | `src/*.lisp` | Implementation code |
| **Tests** | `tests/*.lisp` | FiveAM test suites |
| **Examples** | `examples/*.lisp` | Complete working examples |
| **Documentation** | `docs/*.md` | User-facing documentation |
| **Canon** | `canon/` | Formal specifications |
| **System Definition** | `quickapi.asd` | ASDF system definition |

---

## Invariants

These properties MUST hold at all times. If you find a violation, it's a bug.

### Route Uniqueness
**Property**: No two routes can have the same (method, URI pattern) combination.

**Check**:
```lisp
;; Registry keys are (method . uri-pattern) - must be unique
(= (hash-table-count *route-registry*)
   (length (remove-duplicates (hash-table-keys *route-registry*) :test 'equal)))
```

### Hash Table Test
**Property**: All hash tables representing JSON objects use `:test 'equal`.

**Check**: Manual code review or static analysis.

### Validation Error Collection
**Property**: The `validate` macro MUST run all validators before signaling, not fail-fast.

**Check**:
```lisp
;; This test verifies non-fail-fast behavior
(signals validation-error
  (validate
    (require-fields data "a" "b")
    (require-type data "c" 'string)))
;; Both validators must have run
```

### Response Content-Type
**Property**: All route responses have `Content-Type: application/json`.

**Check**: Integration tests verify response headers.

### Special Variable Binding
**Property**: `*request*` is bound in all route handlers. `*body*` is bound for POST/PUT/PATCH.

**Check**: Integration tests access these variables without error.

### Database Connection Cleanup
**Property**: `with-db` MUST close database connections even on error.

**Check**:
```lisp
;; Test that error doesn't leak connection
(signals error
  (with-db (db "test.db")
    (error "Test error")))
;; Connection should be closed
```

---

## Dependencies

### Required Libraries

| Library | Version | Purpose | Why Chosen |
|---------|---------|---------|------------|
| **snooze** | latest | HTTP routing | CLOS-based, small (~850 LoC), flexible |
| **hunchentoot** | latest | HTTP server | Mature, widely used, battle-tested |
| **com.inuoe.jzon** | latest | JSON parsing | Modern, fast, better API than cl-json |
| **cl-ppcre** | latest | Regex | Standard CL regex library |
| **sqlite** | latest | Database | Simple, embedded, perfect for small APIs |

### Development Dependencies

| Library | Purpose |
|---------|---------|
| **fiveam** | Test framework |

---

## Testing Guidelines

### Test Organization

- One test file per source file (e.g., `validation.lisp` → `validation-tests.lisp`)
- Integration tests in `integration-tests.lisp`
- All tests in `:quickapi-tests` suite

### Writing Tests

```lisp
(in-package :quickapi-tests)

(def-suite your-feature-tests
  :in quickapi-tests
  :description "Tests for your feature")

(in-suite your-feature-tests)

(test your-test-name
  "Test description"
  (is (equal expected actual))
  (signals condition-type (error-causing-form)))
```

### Running Specific Suites

```lisp
;; Run specific suite
(fiveam:run! 'validation-tests)

;; Run specific test
(fiveam:run! 'test-require-fields)
```

---

## Navigation

For detailed navigation of Canon specifications, see **`canon/INDEX.md`**.

### Quick Canon Reference

| Need to... | Read |
|------------|------|
| Understand domain terms | `canon/core/foundation/vocabulary.md` |
| See architecture decisions | `canon/core/decisions/ADR-*.md` |
| Implement a feature | `canon/features/{feature}/` |
| Add validation logic | `canon/features/validation/` |
| Work with database | `canon/features/sqlite_integration/` |
| Understand routing | `canon/features/routing/` |

---

## Common Patterns

### Define an API

```lisp
(defapi my-api
  :name "My API"
  :version "1.0.0"
  :description "My awesome API")
```

### Define Routes

```lisp
;; Simple GET
(api-get "/health" ()
  (ok :status "healthy"))

;; Path parameter
(api-get "/users/:id" ()
  (ok :user-id id))

;; POST with validation
(api-post "/users" ()
  (validate
    (require-fields *body* "name" "email")
    (require-pattern *body* "email" "@"))
  (let ((user (create-user *body*)))
    (created :id (user-id user))))
```

### Database Operations

```lisp
(api-get "/todos" ()
  (with-db (db "todos.db")
    (ok :todos (query-all *db* "SELECT * FROM todos"))))

(api-post "/todos" ()
  (with-db (db "todos.db")
    (execute *db* "INSERT INTO todos (title) VALUES (?)"
             (gethash "title" *body*))
    (created :id (last-insert-rowid *db*))))
```

---

## Troubleshooting

### Tests Failing

1. Check ASDF can find the system: `(asdf:find-system :quickapi)`
2. Ensure dependencies loaded: `(ql:quickload :quickapi)`
3. Run tests verbose: `(asdf:test-system :quickapi)`

### Routes Not Found

1. Check route registered: `(hash-table-keys *route-registry*)`
2. Verify URI pattern syntax: `/users/:id` not `/users/{id}`
3. Ensure `defapi` called before routes

### JSON Parsing Issues

1. Verify Content-Type: `application/json`
2. Check hash table test: `(make-hash-table :test 'equal)`
3. Ensure valid JSON syntax

---

## Project Philosophy

**From the Canon**: quickAPI follows these design principles consistently:

1. **"Thin veneer over proven libraries"** - Don't reinvent, glue together best-of-breed
2. **"5 route macros, not 50 abstractions"** - Minimal API surface
3. **"Automatic JSON, automatic validation, automatic errors"** - Focus on business logic
4. **"Focus on business logic, not infrastructure"** - Zero configuration files

These aren't aspirational—they're enforced. Adding middleware, configuration systems, or heavy abstractions violates the philosophy.

---

*Generated from Canon v0.2.0 on 2026-01-25*
