# error_handling Feature

**Status**: stable | **Confidence**: 0.95 | **Category**: core

---

## Overview

Standardized JSON error responses with consistent format across all error types. Errors follow {error, message, details} structure with appropriate HTTP status codes. Validation errors signal HTTP 422, other errors use semantic status codes (400, 404, 500).

---

## Quick Reference

### Error Response Examples

```json
// Validation Error (422)
{
  "error": "validation_error",
  "message": "Validation failed",
  "details": [
    {"field": "name", "message": "required"},
    {"field": "age", "message": "must be at least 0"}
  ]
}

// Not Found (404)
{
  "error": "not_found",
  "message": "Resource not found"
}

// Bad Request (400)
{
  "error": "bad_request",
  "message": "Invalid request"
}
```

### Using Error Responses

```lisp
;; Return 404
(api-get "/users/:id" ()
  (let ((user (find-user id)))
    (if user
        (ok :user user)
        (not-found "User not found"))))

;; Custom error
(error-response 422 "validation_error" "Invalid data" :field "email")

;; Validation error (automatic 422)
(validate (require-fields *body* "name" "email"))
;; Signals validation-error condition → HTTP 422
```

---

## Primary Contracts

- All errors return JSON with {error, message} fields
- Validation errors return HTTP 422 with details array
- Not found errors return HTTP 404
- Bad request errors return HTTP 400
- Internal errors return HTTP 500

---

## HTTP Status Codes

| Status | Meaning | When to Use |
|--------|---------|-------------|
| 400 | Bad Request | Malformed request, invalid syntax |
| 404 | Not Found | Resource doesn't exist |
| 422 | Unprocessable Entity | Validation failure |
| 500 | Internal Server Error | Unexpected condition |

---

## Error Code Conventions

- lowercase_underscore format (e.g., `validation_error`, `not_found`)
- Descriptive and semantic (not generic like `bad_data`)
- Consistent across all error types

---

## Files

| File | Purpose |
|------|---------|
| `feature.yaml` | Feature metadata and specification |
| `.context.yaml` | Agent context bundle (auto-generated) |

---

## Implementation

**Source**: `src/validation.lisp:71-88` (validation-error), `src/response.lisp` (error helpers)
**Tests**: `tests/validation-tests.lisp`, `tests/response-tests.lisp`

---

## Related Features

- [`validation`](../validation/) - Validation errors (422)
- [`response_helpers`](../response_helpers/) - Error response constructors
- [`json_serialization`](../json_serialization/) - Error response serialization

---

## Design Decisions

- [ADR-003: HTTP 422 for Validation Errors](../../core/decisions/ADR-003-http-422-for-validation-errors.md)

---

*Part of Canon v0.2.0 | [Features Index](../INDEX.md) | [Canon Root](../../INDEX.md)*
