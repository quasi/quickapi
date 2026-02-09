# response_helpers Feature

**Status**: stable | **Confidence**: 0.95 | **Category**: core

---

## Overview

Standardized HTTP response constructors with proper status codes, headers, and JSON bodies. Includes success helpers (ok, created, no-content) and error helpers (bad-request, not-found, error-response).

---

## Quick Reference

### Success Responses

```lisp
;; 200 OK
(api-get "/users/:id" ()
  (ok :id id :name "Alice" :email "alice@example.com"))

;; 201 Created (with Location header)
(api-post "/users" ()
  (created :id new-id :location (format nil "/users/~a" new-id)))

;; 204 No Content
(api-delete "/users/:id" ()
  (delete-user id)
  (no-content))
```

### Error Responses

```lisp
;; 404 Not Found
(api-get "/users/:id" ()
  (let ((user (find-user id)))
    (if user
        (ok :user user)
        (not-found "User not found"))))

;; 400 Bad Request
(bad-request "Invalid parameters")

;; Custom status
(error-response 422 "validation_error" "Invalid data" :field "email")
```

---

## Primary Contracts

| Contract | Purpose |
|----------|---------|
| `ok` | 200 OK with optional data (keyword args → JSON) |
| `created` | 201 Created with optional Location header |
| `no-content` | 204 No Content (empty response) |
| `bad-request` | 400 Bad Request with error message |
| `not-found` | 404 Not Found with error message |
| `error-response` | Custom status code with error details |

---

## Core Properties

- All responses include Content-Type: application/json
- Keyword arguments converted to JSON object fields
- Error responses follow standardized {error, message} format
- Success responses can include arbitrary data fields
- No Content (204) returns empty body (no JSON)

---

## Files

| File | Purpose |
|------|---------|
| `feature.yaml` | Feature metadata and specification |
| `.context.yaml` | Agent context bundle (auto-generated) |

---

## Implementation

**Source**: `src/response.lisp` (all)
**Tests**: `tests/response-tests.lisp` (8 comprehensive tests)

---

## Related Features

- [`json_serialization`](../json_serialization/) - Serializes response bodies
- [`error_handling`](../error_handling/) - Standardized error format
- [`routing`](../routing/) - Route handlers return responses

---

*Part of Canon v0.2.0 | [Features Index](../INDEX.md) | [Canon Root](../../INDEX.md)*
