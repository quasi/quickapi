# QuickAPI 2.0 Implementation Plan

**Goal:** Transform quickapi into a batteries-included framework for beginners exploring Common Lisp who want to build simple-to-medium complexity web APIs easily.

**Status:** Implementation Complete - All 3 Phases Done

---

## Executive Summary

QuickAPI 2.0 will:
1. Add `defmodel` for declarative database models with auto-generated CRUD
2. Replace Snooze with Clack/Lack for proper middleware support
3. Add authentication helpers (JWT, sessions, API keys)

The user-facing API will remain simple while gaining significant capabilities.

---

## Target User Experience

```lisp
;;; Complete working API in ~30 lines

(defpackage :my-todo-api
  (:use :cl :quickapi))

(in-package :my-todo-api)

;; 1. Define API with middleware
(defapi todo-api
  :version "1.0"
  :middlewares (:accesslog
                (:cors :origins '("*"))
                :session))

;; 2. Define models (auto-generates table + CRUD)
(defmodel todo
  ((title :type string :required t :max-length 200)
   (completed :type boolean :default nil)
   (user-id :type integer :references user)))

(defmodel user
  ((name :type string :required t)
   (email :type string :required t :unique t)
   (password-hash :type string)))

;; 3. Define routes
(api-get "/todos" ()
  (ok (list-todos)))

(api-get "/todos/:id" ()
  (if-let ((todo (find-todo id)))
    (ok todo)
    (not-found)))

(api-post "/todos" (:auth :jwt)  ; Protected route
  (validate *body*
    (require-fields "title"))
  (created (create-todo *body*)))

(api-delete "/todos/:id" (:auth :jwt)
  (delete-todo id)
  (no-content))

;; 4. Start server
(with-db ("todos.db")
  (migrate-models)  ; Create/update tables
  (start :port 8000))
```

---

## Phase 1: defmodel Macro

**Goal:** Declarative model definitions that generate tables and CRUD functions.

### 1.1 Syntax Design

```lisp
(defmodel <name>
  ((<field> :type <type>
            [:required t/nil]
            [:default <value>]
            [:unique t/nil]
            [:max-length <n>]
            [:min-length <n>]
            [:pattern <regex>]
            [:references <model>]
            [:on-delete :cascade/:set-null/:restrict])
   ...))
```

### 1.2 Supported Types

| Type | SQLite Type | CL Type | JSON Type |
|------|-------------|---------|-----------|
| `string` | TEXT | string | string |
| `integer` | INTEGER | integer | number |
| `boolean` | INTEGER (0/1) | boolean | boolean |
| `float` | REAL | float | number |
| `datetime` | TEXT (ISO8601) | string | string |
| `json` | TEXT | hash-table | object |

### 1.3 Generated Functions

For `(defmodel todo ...)`:

| Function | Signature | Description |
|----------|-----------|-------------|
| `create-todo` | `(data)` | Insert, return with id |
| `find-todo` | `(id)` | Find by primary key |
| `find-todo-by` | `(field value)` | Find by any field |
| `list-todos` | `(&key limit offset where order-by)` | List with pagination |
| `update-todo` | `(id data)` | Update fields |
| `delete-todo` | `(id)` | Delete by id |
| `count-todos` | `(&key where)` | Count records |

### 1.4 Generated Table

```lisp
(defmodel todo
  ((title :type string :required t :max-length 200)
   (completed :type boolean :default nil)))

;; Generates SQL:
;; CREATE TABLE IF NOT EXISTS todos (
;;   id INTEGER PRIMARY KEY AUTOINCREMENT,
;;   title TEXT NOT NULL,
;;   completed INTEGER NOT NULL DEFAULT 0,
;;   created_at TEXT NOT NULL DEFAULT (datetime('now')),
;;   updated_at TEXT NOT NULL DEFAULT (datetime('now'))
;; )
```

### 1.5 Implementation Tasks

- [ ] Define `defmodel` macro syntax
- [ ] Implement type mapping (CL ↔ SQLite ↔ JSON)
- [ ] Generate `ensure-table` DDL from model spec
- [ ] Generate `create-<model>` function
- [ ] Generate `find-<model>` function
- [ ] Generate `find-<model>-by` function
- [ ] Generate `list-<model>` function with pagination
- [ ] Generate `update-<model>` function
- [ ] Generate `delete-<model>` function
- [ ] Generate `count-<model>` function
- [ ] Add validation integration (reuse existing validators)
- [ ] Add `migrate-models` function
- [ ] Write tests for all generated functions
- [ ] Document defmodel in README

### 1.6 Files to Create/Modify

| File | Action | Purpose |
|------|--------|---------|
| `src/models.lisp` | CREATE | defmodel macro and generators |
| `src/package.lisp` | MODIFY | Export new symbols |
| `tests/model-tests.lisp` | CREATE | Test suite for defmodel |
| `examples/todo-api-v2.lisp` | CREATE | Example using defmodel |

---

## Phase 2: Clack/Lack Integration

**Goal:** Replace Snooze with Clack/Lack for middleware support and server flexibility.

### 2.1 Architecture Change

**Before (Current):**
```
quickapi route macros
        ↓
    Snooze (routing + HTTP)
        ↓
    Hunchentoot
```

**After:**
```
quickapi route macros
        ↓
    quickapi route registry (KEEP)
        ↓
    Lack middleware stack
        ↓
    Clack
        ↓
    Hunchentoot / Woo
```

### 2.2 Middleware Support

```lisp
(defapi my-api
  :middlewares (:accesslog                    ; Request logging
                (:cors :origins '("*"))       ; CORS headers
                :session                       ; Session support
                (:static :path "/public/")    ; Static files
                :backtrace))                  ; Error handling
```

### 2.3 What Changes

| Component | Change |
|-----------|--------|
| Route registry | KEEP - works well |
| `define-json-route` | MODIFY - target Lack app instead of Snooze |
| `start` function | MODIFY - use Clack with Lack builder |
| `stop` function | MODIFY - stop Clack handler |
| `defapi` | MODIFY - add `:middlewares` option |
| Request context | MODIFY - use Lack's env instead of Snooze's |
| `*body*` binding | MODIFY - parse from Lack request |
| `*request*` | MODIFY - bind Lack request object |

### 2.4 Implementation Tasks

- [ ] Add Clack, Lack to dependencies
- [ ] Create `src/lack-app.lisp` for Lack integration
- [ ] Modify `start` to use `clack:clackup` with Lack builder
- [ ] Modify `stop` to use `clack:stop`
- [ ] Update request context (`*body*`, `*request*`) for Lack
- [ ] Add `:middlewares` option to `defapi`
- [ ] Create `quickapi-handler` Lack application
- [ ] Route dispatch from Lack env to route registry
- [ ] Remove Snooze dependency
- [ ] Test all existing functionality still works
- [ ] Add server selection (`:server :hunchentoot` or `:server :woo`)
- [ ] Document middleware usage

### 2.5 Files to Create/Modify

| File | Action | Purpose |
|------|--------|---------|
| `src/lack-app.lisp` | CREATE | Lack application and middleware |
| `src/core.lisp` | MODIFY | Remove Snooze, use Lack dispatch |
| `src/package.lisp` | MODIFY | Update exports |
| `quickapi.asd` | MODIFY | Change dependencies |
| `tests/integration-tests.lisp` | MODIFY | Update for new stack |

### 2.6 Lack Middleware Reference

Built-in middleware we can use:

| Middleware | Purpose | Config |
|------------|---------|--------|
| `:accesslog` | Request logging | None needed |
| `:session` | Session management | `:store`, `:state` |
| `:auth.basic` | HTTP Basic auth | `:authenticator` |
| `:csrf` | CSRF protection | `:session-key` |
| `:static` | Static files | `:path`, `:root` |
| `:backtrace` | Error pages | `:output` |
| `:mount` | Sub-apps | `:path`, `:app` |

---

## Phase 3: Authentication Helpers

**Goal:** Make auth easy for common patterns (JWT, sessions, API keys).

### 3.1 JWT Authentication

```lisp
;; Configuration
(setf *jwt-secret* "your-secret-key")
(setf *jwt-algorithm* :hs256)  ; or :rs256

;; Generate tokens
(defun login-handler ()
  (let ((user (authenticate-user *body*)))
    (ok (list :token (generate-jwt user)
              :user user))))

;; Protect routes
(api-get "/profile" (:auth :jwt)
  (ok *current-user*))  ; Automatically bound from JWT claims
```

### 3.2 Session Authentication

```lisp
;; In defapi
(defapi my-api
  :middlewares (:session))

;; Login
(api-post "/login" ()
  (let ((user (authenticate-user *body*)))
    (setf (session-get :user-id) (gethash "id" user))
    (ok user)))

;; Protect routes
(api-get "/profile" (:auth :session)
  (ok (find-user (session-get :user-id))))
```

### 3.3 API Key Authentication

```lisp
;; Register validator
(setf *api-key-validator*
      (lambda (key)
        (find-api-key key)))  ; Returns user/nil

;; Protect routes
(api-get "/data" (:auth :api-key)
  (ok (fetch-data)))
```

### 3.4 Implementation Tasks

- [ ] Add `jose` to dependencies
- [ ] Create `src/auth.lisp` for auth utilities
- [ ] Implement `generate-jwt` function
- [ ] Implement `verify-jwt` function
- [ ] Implement `@jwt-auth` middleware/decorator
- [ ] Implement session helpers (`session-get`, `session-set`)
- [ ] Implement `@session-auth` middleware/decorator
- [ ] Implement `@api-key-auth` middleware/decorator
- [ ] Add `:auth` option to route macros
- [ ] Bind `*current-user*` from auth context
- [ ] Add password hashing helpers (bcrypt or PBKDF2)
- [ ] Write auth tests
- [ ] Document auth patterns with examples

### 3.5 Files to Create/Modify

| File | Action | Purpose |
|------|--------|---------|
| `src/auth.lisp` | CREATE | All auth utilities |
| `src/core.lisp` | MODIFY | Add `:auth` to route macros |
| `src/package.lisp` | MODIFY | Export auth symbols |
| `quickapi.asd` | MODIFY | Add jose dependency |
| `tests/auth-tests.lisp` | CREATE | Auth test suite |
| `examples/auth-example.lisp` | CREATE | Auth usage examples |

---

## Dependencies

### Current
- `hunchentoot` - Web server
- `snooze` - Routing (TO BE REMOVED)
- `cl-ppcre` - Regex
- `quri` - URI handling
- `com.inuoe.jzon` - JSON
- `cl-sqlite` - Database

### Phase 2 Additions
- `clack` - HTTP abstraction
- `lack` - Middleware
- `woo` (optional) - Fast async server

### Phase 3 Additions
- `jose` - JWT/JOSE implementation
- `ironclad` - Cryptography (for password hashing)
- `cl-bcrypt` (optional) - BCrypt hashing

---

## Testing Strategy

### Unit Tests
- defmodel: Test each generated function
- Auth: Test JWT generation/verification, session helpers
- Routing: Test route registry (existing tests)

### Integration Tests
- Full request/response cycle through Lack
- Middleware composition
- Auth flows (login → protected route)
- Database operations with models

### Manual Testing
- `examples/todo-api-v2.lisp` as living documentation
- Test with curl/httpie

---

## Documentation Updates

| Document | Updates Needed |
|----------|----------------|
| README.md | Add defmodel, middleware, auth sections |
| AGENT.md | Update for new architecture |
| examples/ | New examples for each feature |
| canon/ | Update specifications |

---

## Implementation Order

```
Phase 1: defmodel (self-contained, no breaking changes)
    ↓
Phase 2: Clack/Lack (replace Snooze, internal change)
    ↓
Phase 3: Auth (builds on Phase 2 middleware)
    ↓
Documentation & Examples
```

Each phase should be a working state - no partial implementations.

---

## Success Criteria

### Phase 1 Complete When: ✅
- [x] `defmodel` macro works with all field types
- [x] All CRUD functions generated and tested
- [x] Validation integrated
- [ ] Example todo-api-v2 works with defmodel (deferred)

### Phase 2 Complete When: ✅
- [x] Snooze removed from dependencies
- [x] All existing tests pass on Clack/Lack
- [x] Middleware composition works
- [x] Server selection works (Hunchentoot/Woo)

### Phase 3 Complete When: ✅
- [x] JWT auth works end-to-end
- [x] Session auth works end-to-end
- [x] API key auth works
- [ ] Auth example demonstrates all patterns (deferred)

### Project Complete When:
- [x] All phases complete
- [ ] Documentation updated
- [ ] Examples cover all features
- [ ] README guides beginners through complete workflow
