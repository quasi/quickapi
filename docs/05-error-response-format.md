# Error Response Format

quickAPI returns **consistent, structured error responses** for all error conditions. This document specifies the exact JSON schema and provides examples for all error types.

## Standard Error Schema

All error responses follow this JSON structure:

```json
{
  "error": "<error_type>",
  "message": "<human_readable_message>",
  "details": <optional_details>
}
```

### Fields

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `error` | string | ✓ | Machine-readable error type (snake_case) |
| `message` | string | ✓ | Human-readable error message |
| `details` | any | ✗ | Additional error context (structure varies by error type) |

---

## Error Types by HTTP Status

### 400 Bad Request

**When**: Request is malformed (invalid JSON syntax, wrong content-type)

```json
{
  "error": "bad_request",
  "message": "Bad request"
}
```

**Trigger**:
```lisp
(bad-request "Invalid request format")
```

---

### 404 Not Found

**When**: Requested resource doesn't exist

```json
{
  "error": "not_found",
  "message": "Todo not found"
}
```

**Trigger**:
```lisp
(not-found "Todo not found")
```

**Example in context**:
```lisp
(api-get "/todos/:id" ()
  (let ((todo (find-todo id)))
    (if todo
        todo
        (not-found "Todo not found"))))
```

---

### 422 Unprocessable Entity (Validation Errors)

**When**: Request is well-formed but data fails validation

```json
{
  "error": "validation_error",
  "message": "Validation failed",
  "details": [
    {
      "field": "email",
      "error": "field_required"
    },
    {
      "field": "age",
      "error": "value_out_of_range",
      "expected": "0-150",
      "actual": 200
    }
  ]
}
```

**Details Structure** (validation errors):
```typescript
details: Array<{
  field: string,           // Field name that failed
  error: string,           // Error type (see Validation Errors below)
  expected?: any,          // Expected value/range (for range/pattern errors)
  actual?: any,            // Actual value provided
  min?: number,            // Minimum (for length/range errors)
  max?: number             // Maximum (for length/range errors)
}>
```

**Trigger**:
```lisp
(validate *body*
  (require-fields "email" "name")
  (require-type "age" 'integer)
  (require-range "age" :min 0 :max 150))
```

---

### 500 Internal Server Error

**When**: Unhandled exception in server code

```json
{
  "error": "internal_server_error",
  "message": "Internal server error"
}
```

**Note**: quickAPI automatically catches unhandled exceptions and converts to 500 responses.

---

### Custom Error Status

**When**: You need a specific HTTP status code

```json
{
  "error": "payment_required",
  "message": "Payment required for premium feature"
}
```

**Trigger**:
```lisp
(error-response 402 "Payment required for premium feature")
```

---

## Validation Error Types

When validation fails, the `details` array contains objects with these `error` values:

### field_required

**When**: Required field is missing or empty

```json
{
  "field": "email",
  "error": "field_required"
}
```

**Triggered by**: `(require-fields "email")`

---

### invalid_type

**When**: Field has wrong type

```json
{
  "field": "age",
  "error": "invalid_type",
  "expected": "integer",
  "actual": "string"
}
```

**Triggered by**: `(require-type "age" 'integer)`

**Type names**:
- `string`
- `integer`
- `number` (integer or float)
- `boolean`

---

### invalid_length

**When**: String length outside allowed range

```json
{
  "field": "title",
  "error": "invalid_length",
  "min": 1,
  "max": 200,
  "actual": 250
}
```

**Triggered by**: `(require-length "title" :min 1 :max 200)`

---

### value_out_of_range

**When**: Numeric value outside allowed range

```json
{
  "field": "age",
  "error": "value_out_of_range",
  "min": 0,
  "max": 150,
  "actual": 200
}
```

**Triggered by**: `(require-range "age" :min 0 :max 150)`

---

### pattern_mismatch

**When**: String doesn't match required pattern

```json
{
  "field": "email",
  "error": "pattern_mismatch",
  "expected": ".*@.*\\..*"
}
```

**Triggered by**: `(require-pattern "email" ".*@.*\\..*")`

---

## Complete Example

### Request

```http
POST /users HTTP/1.1
Content-Type: application/json

{
  "name": "",
  "age": 200,
  "email": "invalid"
}
```

### Response

```http
HTTP/1.1 422 Unprocessable Entity
Content-Type: application/json

{
  "error": "validation_error",
  "message": "Validation failed",
  "details": [
    {
      "field": "name",
      "error": "field_required"
    },
    {
      "field": "age",
      "error": "value_out_of_range",
      "min": 0,
      "max": 150,
      "actual": 200
    },
    {
      "field": "email",
      "error": "pattern_mismatch",
      "expected": ".*@.*\\..*"
    }
  ]
}
```

### Handler Code

```lisp
(api-post "/users" ()
  (validate *body*
    (require-fields "name" "email")
    (require-type "age" 'integer)
    (require-range "age" :min 0 :max 150)
    (require-pattern "email" ".*@.*\\..*"))

  ;; If validation passes, create user
  (created (create-user *body*)))
```

---

## Client Error Handling

### JavaScript/TypeScript

```typescript
interface ErrorResponse {
  error: string;
  message: string;
  details?: ValidationError[];
}

interface ValidationError {
  field: string;
  error: string;
  expected?: any;
  actual?: any;
  min?: number;
  max?: number;
}

async function createUser(data: UserData) {
  const response = await fetch('/users', {
    method: 'POST',
    headers: {'Content-Type': 'application/json'},
    body: JSON.stringify(data)
  });

  if (!response.ok) {
    const error: ErrorResponse = await response.json();

    if (response.status === 422) {
      // Handle validation errors
      error.details?.forEach(err => {
        console.log(`${err.field}: ${err.error}`);
      });
    } else if (response.status === 404) {
      console.log('Resource not found');
    }
  }
}
```

---

## Design Decisions

### Why This Format?

1. **Consistent**: Same structure for all errors
2. **Machine-readable**: `error` field enables automated handling
3. **Human-friendly**: `message` field for display
4. **Detailed**: `details` provides actionable information
5. **Standard HTTP**: Uses proper HTTP status codes

### Why HTTP 422 for Validation?

- **422 Unprocessable Entity**: Request syntax valid, but semantics invalid
- **400 Bad Request**: Reserved for syntax errors (malformed JSON)
- **Semantic clarity**: Clients can distinguish error types

See [ADR-003: HTTP 422 for Validation Errors](../canon/core/decisions/ADR-003-http-422-for-validation-errors.md)

### Why Collect All Validation Errors?

quickAPI validates **all fields** before returning errors (doesn't fail-fast):

**Better UX**: User sees all problems at once
```json
{
  "details": [
    {"field": "name", "error": "field_required"},
    {"field": "email", "error": "pattern_mismatch"},
    {"field": "age", "error": "value_out_of_range"}
  ]
}
```

**Worse UX**: User fixes one error, submits, sees next error (repeat)
```json
{
  "details": [
    {"field": "name", "error": "field_required"}
  ]
}
```

---

## JSON Schema (for validation)

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "title": "quickAPI Error Response",
  "type": "object",
  "required": ["error", "message"],
  "properties": {
    "error": {
      "type": "string",
      "enum": [
        "bad_request",
        "not_found",
        "validation_error",
        "internal_server_error"
      ]
    },
    "message": {
      "type": "string"
    },
    "details": {
      "type": "array",
      "items": {
        "type": "object",
        "required": ["field", "error"],
        "properties": {
          "field": {"type": "string"},
          "error": {
            "type": "string",
            "enum": [
              "field_required",
              "invalid_type",
              "invalid_length",
              "value_out_of_range",
              "pattern_mismatch"
            ]
          },
          "expected": {},
          "actual": {},
          "min": {"type": "number"},
          "max": {"type": "number"}
        }
      }
    }
  }
}
```

---

## Testing Error Responses

```lisp
;; Test validation error format
(5am:test validation-error-format
  (let ((response (call-api :post "/users"
                             :body "{\"age\": 200}")))
    (5am:is (= 422 (response-status response)))
    (let ((error (parse-json (response-body response))))
      (5am:is (string= "validation_error" (gethash "error" error)))
      (5am:is (string= "Validation failed" (gethash "message" error)))
      (5am:is (> (length (gethash "details" error)) 0)))))
```

---

## Summary

**All quickAPI errors follow the same structure**:
- Consistent JSON format
- HTTP status codes indicate error category
- Machine-readable `error` field
- Human-readable `message` field
- Optional `details` for additional context

**For API clients**: Always check HTTP status first, then parse error JSON for details.

**For API developers**: Use helper functions (`bad-request`, `not-found`, `validate`) to ensure consistent errors.
