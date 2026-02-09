# Validation Feature

**Status**: stable | **Confidence**: 0.97 | **Category**: core

---

## Overview

Validate request data with declarative validators that collect all errors before signaling HTTP 422. Provides five validators covering common validation needs without requiring a DSL.

**Key Innovation**: Non-fail-fast validation collects all errors, providing better UX than one-error-at-a-time.

---

## Quick Reference

### Basic Validation

```lisp
(api-post "/users" ()
  (validate
    (require-fields *body* "name" "email")
    (require-type *body* "age" 'integer)
    (require-range *body* "age" :min 0 :max 120)
    (require-pattern *body* "email" "@"))
  (ok (create-user *body*)))
```

### Validators

```lisp
;; Required fields
(require-fields data "field1" "field2")

;; Type checking
(require-type data "age" 'integer)
(require-type data "tags" 'list)

;; String/list length
(require-length data "name" :min 1 :max 100)

;; Numeric range
(require-range data "age" :min 18)
(require-range data "score" :min 0 :max 100)

;; Regex pattern
(require-pattern data "email" "@")
(require-pattern data "phone" "^\\d{3}-\\d{4}$")
```

---

## Primary Contracts

| Contract | Purpose |
|----------|---------|
| `validate` | Run validators, collect errors, signal 422 if failures |
| `require-fields` | Check required fields present and non-empty |
| `require-type` | Check field type (string, number, integer, boolean, list, hash-table) |
| `require-length` | Check string/list length (min/max) |
| `require-range` | Check numeric value range (min/max) |
| `require-pattern` | Check string matches regex pattern |
| `validation-error` | Condition signaled on validation failure |

---

## Core Properties

- Validation errors collected, not fail-fast
- Validation failures signal HTTP 422
- Empty strings treated as missing for require-fields
- nil values skip type/length/range checks (allows optional fields)
- Error format is standardized and predictable

---

## Error Response Format

```json
HTTP 422 Unprocessable Entity
{
  "error": "validation_error",
  "message": "Validation failed",
  "details": [
    {"field": "name", "message": "required"},
    {"field": "age", "message": "must be at least 0"}
  ]
}
```

---

## Files

| File | Purpose |
|------|---------|
| `feature.yaml` | Feature metadata and specification |
| `.context.yaml` | Agent context bundle (auto-generated) |

---

## Implementation

**Source**: `src/validation.lisp` (all)
**Tests**: `tests/validation-tests.lisp` (20+ tests)
**Examples**: `examples/todo-api.lisp:74-77, 92-94`

---

## Related Features

- [`request_context`](../request_context/) - Validates *body* from parsed JSON
- [`error_handling`](../error_handling/) - Standardized 422 error format
- [`json_serialization`](../json_serialization/) - Validates parsed JSON data

---

## Design Decisions

- [ADR-003: HTTP 422 for Validation Errors](../../core/decisions/ADR-003-http-422-for-validation-errors.md)

---

*Part of Canon v0.2.0 | [Features Index](../INDEX.md) | [Canon Root](../../INDEX.md)*
