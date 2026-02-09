# json_serialization Feature

**Status**: stable | **Confidence**: 0.95 | **Category**: core

---

## Overview

Automatic JSON serialization and deserialization for HTTP requests and responses. Route macros handle JSON body parsing for POST/PUT/PATCH and serialize return values to JSON automatically.

---

## Quick Reference

### Return Values (Automatic Serialization)

```lisp
;; Hash table → JSON object
(api-get "/user/:id" ()
  (let ((user (make-hash-table :test 'equal)))
    (setf (gethash "id" user) id)
    (setf (gethash "name" user) "Alice")
    (ok user)))
;; Returns: {"id": "123", "name": "Alice"}

;; List → JSON array
(api-get "/users" ()
  (ok :users (list user1 user2 user3)))
;; Returns: {"users": [...]}
```

### Request Bodies (Automatic Parsing)

```lisp
;; *body* contains parsed JSON hash table
(api-post "/users" ()
  (let ((name (gethash "name" *body*))
        (email (gethash "email" *body*)))
    (ok (create-user name email))))
```

---

## Primary Contracts

- Route return values automatically serialized to JSON
- POST/PUT/PATCH bodies automatically parsed as JSON
- *body* special variable contains parsed hash table
- Content-Type: application/json header auto-added
- Hash tables must use :test 'equal for JSON objects

---

## JSON Type Mapping

| Lisp | JSON |
|------|------|
| hash-table (:test equal) | object |
| list | array |
| string | string |
| number | number |
| t | true |
| nil | null |

---

## Files

| File | Purpose |
|------|---------|
| `feature.yaml` | Feature metadata and specification |
| `.context.yaml` | Agent context bundle (auto-generated) |

---

## Implementation

**Source**: `src/core.lisp:174-209` (define-json-route macro)
**Tests**: `tests/integration-tests.lisp`, `tests/json-parsing-tests.lisp`

---

## Related Features

- [`routing`](../routing/) - Route macros provide serialization hooks
- [`request_context`](../request_context/) - *body* contains parsed JSON
- [`validation`](../validation/) - Validate parsed JSON data
- [`error_handling`](../error_handling/) - JSON error responses

---

*Part of Canon v0.2.0 | [Features Index](../INDEX.md) | [Canon Root](../../INDEX.md)*
