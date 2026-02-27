---
name: quickapi-integration
description: API guide for agents using quickAPI
version: 0.1.0
author: Quasilabs
type: integration
---

# quickAPI Integration Guide

quickAPI is a JSON API toolkit for Common Lisp. Declare models, define routes, authenticate—no boilerplate. Agents use this guide to build applications; for library development, see the dev skill.

## What is quickAPI

A curated stack combining Clack (HTTP), Lack (middleware), jzon (JSON), and inquisitio (SQLite) with thin glue code. Three abstractions eliminate most API boilerplate:
- **Models**: Declare schema → auto-generate CRUD + table
- **Routes**: HTTP verbs with auto-parsed JSON, path parameters, validation
- **Auth**: JWT, sessions, API keys in one line

Think of it as "Rails-like simplicity for Lisp APIs" — not a framework, but proven libraries + clear conventions.

## Quick Start

### Load & Define API

```lisp
(defpackage :my-api (:use :cl :quickapi))
(in-package :my-api)

(defapi my-api :version "1.0" :middlewares (:accesslog))

(defmodel user
  ((name :type string :required t)
   (email :type string :required t :unique t)))

(api-post "/users" ()
  (with-db ("app.db")
    (validate *body* (require-fields "name" "email"))
    (created (create-user *body*))))

(api-get "/users/:id" ()
  (with-db ("app.db")
    (ok (find-user id))))  ; Signals record-not-found → dispatcher converts to 404

(defun start-app ()
  (with-db ("app.db") (migrate-models))
  (start :port 8000))
```

### Initialize & Run

```lisp
(load "my-api.lisp")
(my-api:start-app)
;; Server on http://0.0.0.0:8000/

;; Test
(curl -X POST -H "Content-Type: application/json"
      -d '{"name":"Alice","email":"alice@ex.com"}'
      http://localhost:8000/users)
```

## Core Concepts

**Models** — Data with automatic CRUD:
```lisp
(defmodel todo
  ((title :type string :required t :max-length 200)
   (completed :type boolean :default nil)
   (user-id :type integer :required t)))
; Generates: create-todo, find-todo, find-todo-by, list-todos, update-todo, delete-todo, count-todos
```

**Routes** — HTTP with auto-parsing and parameter extraction:
```lisp
(api-get "/todos/:id" ()        ; :id automatically bound
  (ok (find-todo id)))

(api-post "/todos" ()            ; *body* auto-parsed as JSON
  (validate *body* (require-fields "title"))
  (created (create-todo *body*)))
```

**Success Responses** — Return HTTP responses (auto-serialize to JSON):
```lisp
(ok data)               ; 200 + {data}
(created data)          ; 201 + {data}
(no-content)            ; 204
```

**Error Responses** — Signal conditions (caught by dispatcher, converted to HTTP responses):
```lisp
(bad-request "msg")     ; 400 + error
(not-found)             ; 404
(conflict "msg")        ; 409
```

**Validation** — Collect errors before responding:
```lisp
(validate *body*
  (require-fields "name" "email")
  (require-type "age" 'integer)
  (require-length "name" :min 1 :max 100)
  (require-range "score" :min 0 :max 100)
  (require-pattern "email" "^[^@]+@[^@]+$"))
; Raises validation-error with all errors collected
```

**Database** — SQLite with connection binding:
```lisp
(with-db ("app.db")
  (migrate-models)                      ; Create all tables
  (let ((user-data (make-hash-table :test 'equal)))
    (setf (gethash "name" user-data) "Alice")
    (create-user user-data))
  (find-user 1)
  (list-users :limit 10 :offset 0 :order-by '(:created-at :desc)))
```

**Authentication** — Pick one, use one line:
```lisp
(defapi my-api :middlewares (:session))  ; Session-based
(verify-jwt token)                        ; JWT
(funcall *api-key-validator* key)        ; API key (implement validator)
```

## Key API

### Model Definition

| Syntax | Effect |
|--------|--------|
| `(defmodel name ((field :type type :required t) ...))` | Declare model, auto-create CRUD |
| `:type {string, integer, boolean, float}` | Field type (SQL cast) |
| `:required t` | NOT NULL constraint |
| `:unique t` | UNIQUE constraint |
| `:default value` | Default value |
| `:max-length N` | Validation constraint |

### CRUD Functions (Auto-Generated)

For model `user`:

| Function | Signature | Returns / Signals |
|----------|-----------|---------|
| `create-user` | `(hash-table) → hash-table` | Inserted row |
| `find-user` | `(id) → hash-table` | Row by ID, or signals `record-not-found` |
| `find-user-by` | `(field value) → hash-table \| nil` | Row by field, or nil if missing |
| `list-users` | `(&key limit offset order-by where) → list` | Rows with optional WHERE filter |
| `update-user` | `(id hash-table) → hash-table` | Updated row |
| `delete-user` | `(id) → T` | Always returns T on success |
| `count-users` | `() → integer` | Row count |

### Route Macros

```lisp
(api-get "/path/:param" ()
  (ok data))

(api-post "/path" ()
  (validate *body* ...)
  (created (create-thing *body*)))

(api-put "/path/:id" ()
  (update-thing id *body*)
  (ok ...))

(api-patch "/path/:id" ()
  (partial-update id *body*)
  (ok ...))

(api-delete "/path/:id" ()
  (delete-thing id)
  (no-content))
```

Parameters (`:param`) auto-bind as lexical variables. `*body*` available for POST/PUT/PATCH.

### Response Helpers — Success

Return HTTP responses directly (auto-serialize to JSON):

| Helper | Status | Body |
|--------|--------|------|
| `(ok data)` | 200 | Serialized data |
| `(created data)` | 201 | Serialized data |
| `(no-content)` | 204 | Empty |

### Response Helpers — Error

Signal conditions (caught by dispatcher and converted to HTTP responses):

| Helper | Status | Body |
|--------|--------|------|
| `(bad-request "msg")` | 400 | `{error: "bad_request", message: "msg"}` |
| `(not-found)` | 404 | `{error: "not_found", message: "Resource not found"}` |
| `(conflict "msg")` | 409 | `{error: "conflict", message: "msg"}` |
| `(error-response 500 "msg")` | Custom | `{error: "...", message: "msg"}` |

### Validation

`(validate *body* (validators...))`

| Validator | Effect |
|-----------|--------|
| `(require-fields "f1" "f2" ...)` | Error if missing |
| `(require-type "field" 'type)` | Error if wrong type |
| `(require-length "field" :min N :max M)` | Error if out of range |
| `(require-range "field" :min N :max M)` | Numeric range |
| `(require-pattern "field" "^...$")` | Regex match |

Collects all errors, raises `validation-error` at end.

### Database

```lisp
(with-db ("app.db")           ; Bind *db* to connection
  (migrate-models)            ; Create tables from all models
  (create-user ...)           ; CRUD calls
  (with-transaction           ; Atomic block
    (create-user ...)
    (update-user ...)))
```

### Authentication

```lisp
(defvar *jwt-secret* "your-key")

(generate-jwt (make-hash-table))  ; Create token
(verify-jwt token)                ; Validate & decode

(hash-password "plaintext")       ; PBKDF2-SHA256 via Ironclad
(verify-password "plaintext" hash) ; Check

(session-get "user-id")           ; Read session
```

## Common Patterns

### PATTERN-001: CRUD API from Model

**Scenario**: Define a complete REST API for a single resource.

```lisp
(defmodel article
  ((title :type string :required t)
   (body :type string :required t)
   (published :type boolean :default nil)))

(api-get "/articles" ()
  (with-db ("app.db")
    (ok (list-articles :limit 10))))

(api-post "/articles" ()
  (with-db ("app.db")
    (validate *body* (require-fields "title" "body"))
    (created (create-article *body*))))

(api-get "/articles/:id" ()
  (with-db ("app.db")
    (ok (or (find-article id) (not-found)))))

(api-put "/articles/:id" ()
  (with-db ("app.db")
    (update-article id *body*)
    (ok (find-article id))))

(api-delete "/articles/:id" ()
  (with-db ("app.db")
    (delete-article id)
    (no-content)))
```

**Rules Satisfied**: Automatic CRUD generation, validation, auto-serialization.

**Why This Shape**: `defmodel` eliminates SQL boilerplate. Route helpers auto-parse requests and serialize responses. Validation is declarative.

**Variations**:
| Scenario | Modification |
|----------|--------------|
| Pagination | Use `list-articles :limit 10 :offset (* page 10)` |
| Filtering | Add `find-article-by` lookup after validation |
| Soft delete | Don't use `delete-article`, set `deleted_at` via update |

---

### PATTERN-002: Authenticated Routes with JWT

**Scenario**: Protect routes with JWT tokens.

```lisp
(defapi my-api :middlewares (:accesslog))

(defvar *jwt-secret* "super-secret-key")
(defvar *jwt-algorithm* :hs256)

(api-post "/login" ()
  (with-db ("app.db")
    (handler-case
        (let ((user (find-user-by "email" (gethash "email" *body*))))
          (if (verify-password (gethash "password" *body*)
                               (gethash "password_hash" user))
              (let ((response (make-hash-table :test 'equal)))
                (setf (gethash "token" response) (generate-jwt user))
                (ok response))
              (unauthorized "Invalid credentials")))
      (quickapi:record-not-found ()
        (unauthorized "Invalid credentials")))))

(defun protected-handler (handler)
  "Middleware: verify JWT and bind *current-user*."
  (lambda (env)
    (let ((auth-header (gethash "HTTP_AUTHORIZATION" env)))
      (if (and auth-header (>= (length auth-header) 7) (string-equal (subseq auth-header 0 7) "Bearer "))
          (handler-case
              (let* ((token (subseq auth-header 7))
                     (claims (verify-jwt token)))
                (setf quickapi:*current-user* claims)
                (funcall handler env))
            (quickapi:http-error (e)
              (list (quickapi:http-error-status e)
                    '(:content-type "application/json")
                    (list (jzon:stringify (quickapi:http-error-details e))))))
          (list 401
                '(:content-type "application/json")
                (list "{\"error\":\"Missing authorization\"}")))))))

(api-get "/me" ()
  (ok quickapi:*current-user*))
```

**Rules Satisfied**: Secrets from environment, token validation before handler, no plaintext passwords.

**Why This Shape**: JWT is stateless (no session DB). Verify early in middleware. Store claims in `*current-user*` for route access.

---

### PATTERN-003: Batch Operations with Transaction

**Scenario**: Atomic multi-record operation.

```lisp
(api-post "/bulk-create-todos" ()
  (with-db ("app.db")
    (validate *body* (require-fields "todos"))
    (with-transaction
      (let ((results nil))
        (dolist (todo-data (gethash "todos" *body*))
          (push (create-todo todo-data) results))
        (created (nreverse results))))))
```

**Rules Satisfied**: Atomic updates, all-or-nothing semantics.

**Why This Shape**: `with-transaction` wraps SQLite transactions. If any insert fails, all roll back.

---

### PATTERN-004: Error Recovery with Fallback

**Scenario**: Handle validation failures gracefully.

```lisp
(api-post "/users" ()
  (with-db ("app.db")
    (handler-case
        (progn
          (validate *body* (require-fields "name" "email"))
          (created (create-user *body*)))
      (validation-error (e)
        (bad-request (format nil "Validation failed: ~a"
                             (validation-errors e))))
      (duplicate-record (e)
        (conflict (format nil "Email already exists")))
      (database-error (e)
        (error-response 500 "Database error")))))
```

**Rules Satisfied**: Specific error handling, user-friendly messages.

**Why This Shape**: `handler-case` catches expected errors. Validation-error provides detailed messages. Database errors return 500 (not user input issue).

---

### PATTERN-005: Pagination

**Scenario**: Large result sets with offset-limit pagination.

```lisp
(api-get "/users" ()
  (with-db ("app.db")
    (let* ((limit 20)
           (offset 0)  ; In practice, compute from query params if needed
           (data (list-users :limit limit :offset offset))
           (response (make-hash-table :test 'equal)))
      (setf (gethash "data" response) data)
      (setf (gethash "total" response) (count-users))
      (setf (gethash "limit" response) limit)
      (setf (gethash "offset" response) offset)
      (ok response))))
```

**Rules Satisfied**: Offset-limit pagination, row count for client-side UI.

**Why This Shape**: Return data + total count + limit/offset for clients to build pagination UI. Query param parsing is application-specific (parse from `*request*` if needed).

## Pitfalls

### PITFALL-001: Forgetting `with-db`

**Symptom**: `database-error: *db* is unbound`

**Root**: Routes run without binding `*db*`.

**Fix**: Wrap all CRUD calls in `(with-db ("path.db") ...)`. Do not rely on global `*db*` outside route context.

---

### PITFALL-002: Validation Doesn't Stop Execution

**Symptom**: Route handler continues after validation fails, crashes on nil.

**Root**: `validate` raises `validation-error` condition. If not caught, error handler responds. But if you continue logic, you bypass the error.

**Fix**: Call `validate` FIRST. Trust it raises on error.

```lisp
;; WRONG
(api-post "/items" ()
  (ok (create-item *body*))        ; No validation
  (validate *body* ...))           ; Never reached

;; RIGHT
(api-post "/items" ()
  (validate *body* ...)            ; Raises on error
  (ok (create-item *body*)))       ; Only if validation passed
```

---

### PITFALL-003: `*body*` is Nil for GET

**Symptom**: GET handler tries to read `*body*`, gets nil, crashes.

**Root**: Lack middleware only parses POST/PUT/PATCH bodies. GET requests have no body.

**Fix**: Only access `*body*` in POST/PUT/PATCH. For GET filtering, parse query string manually or design APIs to use path parameters.

```lisp
;; WRONG
(api-get "/search" ()
  (search (gethash "q" *body*)))    ; *body* is nil for GET

;; RIGHT - use path parameter
(api-get "/search/:query" ()
  (search query))                   ; Query extracted from URI

;; RIGHT - use POST if filtering requires complex data
(api-post "/search" ()
  (validate *body* ...)
  (search *body*))
```

---

### PITFALL-004: Silent Type Coercion in Database

**Symptom**: Inserted `user-id` as string "123", later comparisons fail.

**Root**: defmodel field `:type integer` does NOT validate at insertion—SQLite stores whatever you pass.

**Fix**: Use `require-type` in validation. Or cast before insert.

```lisp
(validate *body*
  (require-fields "user-id")
  (require-type "user-id" 'integer))  ; Catches string "123"
```

---

### PITFALL-005: Password Stored as Plaintext

**Symptom**: Database dump shows user passwords.

**Root**: Forgot to hash password on insert.

**Fix**: Hash before store. Never store plaintext.

```lisp
;; WRONG
(create-user *body*)                ; Stores password as-is

;; RIGHT
(let ((password (gethash "password" *body*)))
  (setf (gethash "password_hash" *body*) (hash-password password))
  (remhash "password" *body*)
  (create-user *body*))
```

## Further Reading

- **README.md** — Quickstart and feature overview
- **canonical-specification/** — Feature specifications, architecture decisions, and tutorials

---

**For library development**, see `.claude/skills/dev/SKILL.md`.
