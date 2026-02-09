# request_context Feature

**Status**: stable | **Confidence**: 0.93 | **Category**: core

---

## Overview

Special variables and extracted data available during request handling. Includes *request* (Snooze request object), *body* (parsed JSON), *db* (database connection), and automatically bound path parameters.

---

## Quick Reference

### Special Variables

```lisp
;; *request* - Snooze request object
(api-get "/debug" ()
  (let ((method (snooze:http-method *request*))
        (uri (snooze:request-uri *request*)))
    (ok :method method :uri uri)))

;; *body* - Parsed JSON (POST/PUT/PATCH only)
(api-post "/users" ()
  (let ((name (gethash "name" *body*)))
    (ok :name name)))

;; *db* - Database connection (within with-db)
(api-get "/todos" ()
  (with-db (db "todos.db")
    (ok :todos (query-all *db* "SELECT * FROM todos"))))
```

### Path Parameters

```lisp
;; Path parameters automatically bound
(api-get "/users/:user-id/posts/:post-id" ()
  ;; user-id and post-id are lexical variables
  (ok :user-id user-id :post-id post-id))
```

---

## Primary Contracts

| Contract | Purpose |
|----------|---------|
| `*request*` | Snooze request object (all routes) |
| `*body*` | Parsed JSON body (POST/PUT/PATCH) |
| `*db*` | Database connection (within with-db) |
| Path parameters | Auto-bound lexical variables |

---

## Core Properties

- *request* available in all route handlers
- *body* only bound for POST/PUT/PATCH with JSON content-type
- *db* only bound within with-db form
- Path parameters bound as lexical variables (not special)
- Parameter symbols interned in :quickapi package

---

## Files

| File | Purpose |
|------|---------|
| `feature.yaml` | Feature metadata and specification |
| `.context.yaml` | Agent context bundle (auto-generated) |

---

## Implementation

**Source**: `src/core.lisp:174-209` (special variable bindings), `src/core.lisp:49-58` (path extraction)
**Tests**: `tests/integration-tests.lisp`, `tests/sqlite-tests.lisp`

---

## Related Features

- [`routing`](../routing/) - Provides path parameter extraction
- [`json_serialization`](../json_serialization/) - Provides *body* parsing
- [`validation`](../validation/) - Validates *body* data
- [`sqlite_integration`](../sqlite_integration/) - Provides *db* binding

---

*Part of Canon v0.2.0 | [Features Index](../INDEX.md) | [Canon Root](../../INDEX.md)*
