# ADR-003: HTTP 422 for Validation Errors

**Status**: Accepted
**Date**: 2026-01 (extracted from implementation)
**Deciders**: quickAPI core team
**Extraction Confidence**: 0.97

## Context

When request validation fails (missing required fields, type mismatches, pattern failures, etc.), the API must return an HTTP error status. The choice of status code affects API semantics and client error handling.

## Decision

**Use HTTP 422 Unprocessable Entity for validation errors.**

When the `validate` macro detects validation failures, quickAPI signals a `validation-error` condition that results in a 422 response with structured error details.

## Rationale

HTTP 422 is semantically correct for validation errors:

1. **Semantic accuracy**: 422 means "request well-formed but semantically invalid"
2. **Distinguishes from 400**: 400 Bad Request is for malformed syntax (e.g., invalid JSON), 422 is for invalid data
3. **Rails convention**: Ruby on Rails popularized this usage, widely understood
4. **JSON:API spec**: JSON:API standard uses 422 for validation errors
5. **Clear client handling**: Clients can distinguish malformed requests from invalid business data

## Alternatives Considered

### HTTP 400 Bad Request
**Rejected**: Use 400 for all client errors
```lisp
;; Rejected approach
(validate *body*
  (require-fields "email"))
; → 400 Bad Request if email missing
```
**Why rejected**:
- Conflates syntax errors (malformed JSON) with semantic errors (missing field)
- Less precise error handling for clients
- Harder to distinguish error types in monitoring

### HTTP 200 OK with Error Body
**Rejected**: Return 200 with `{success: false, errors: [...]}`
**Why rejected**:
- Violates HTTP semantics
- Breaks REST conventions
- Complicates client error handling
- Prevents HTTP-level error handling (proxies, middleware)

### Custom 4xx Status (e.g., 499)
**Rejected**: Invent custom status code for validation
**Why rejected**:
- Non-standard, confusing
- Requires documentation
- Breaks HTTP client assumptions

## Consequences

### Positive
- ✅ **Semantic clarity**: Status code clearly indicates validation failure
- ✅ **Client error handling**: Clients can handle 422 differently from 400/500
- ✅ **Standards alignment**: Matches Rails, JSON:API, common practice
- ✅ **Monitoring**: Easy to track validation errors vs other errors
- ✅ **Debugging**: Clear distinction between malformed and invalid requests

### Negative
- ⚠️ **Not universal**: Some REST APIs use 400 for all client errors (less precise)
- ⚠️ **HTTP spec ambiguity**: 422 is from WebDAV spec, not core HTTP (though widely adopted)

### Response Format

Validation errors return structured JSON:
```json
{
  "error": "validation_error",
  "message": "Validation failed",
  "details": [
    {"field": "email", "error": "field_required"},
    {"field": "age", "error": "value_out_of_range", "expected": "0-150", "actual": 200}
  ]
}
```

## Implementation

**Location**: `src/validation.lisp:60-69`

Key code:
```lisp
(define-condition validation-error (snooze:http-condition)
  ((errors :initarg :errors :accessor validation-errors))
  (:default-initargs :status-code 422))  ; ← HTTP 422
```

**Tests**: `tests/validation-tests.lisp` verifies 422 responses
**Examples**: `examples/todo-api.lisp` demonstrates validation with 422 errors

## HTTP Status Code Usage Summary

| Status | quickAPI Usage |
|--------|----------------|
| 200 OK | Successful GET requests |
| 201 Created | Successful POST (resource created) |
| 204 No Content | Successful DELETE |
| 400 Bad Request | Malformed JSON, invalid content-type |
| 404 Not Found | Resource doesn't exist |
| **422 Unprocessable Entity** | **Validation failures** |
| 500 Internal Server Error | Unhandled exceptions |

## Error Hierarchy

```
Client Errors (4xx)
├── 400 Bad Request        ← Syntax errors (malformed JSON)
├── 404 Not Found          ← Resource not found
└── 422 Unprocessable      ← Semantic errors (validation)
```

## Standards References

- **RFC 4918 (WebDAV)**: Section 11.2 defines 422 Unprocessable Entity
- **JSON:API**: Uses 422 for validation errors in specification
- **Rails Active Record**: Default for model validation failures

## Related Decisions

- ADR-004: Error handling standardization (consistent error format)
- Validation framework: Collect all errors (don't fail-fast) before returning 422

## Client Example

```javascript
// Client handling
fetch('/users', {
  method: 'POST',
  body: JSON.stringify({name: '', age: 200})
})
.then(response => {
  if (response.status === 422) {
    // Handle validation errors
    return response.json().then(errors => {
      displayValidationErrors(errors.details);
    });
  } else if (response.status === 400) {
    // Handle malformed request
    alert('Request malformed');
  }
});
```

## Notes

The choice of 422 is **intentional and opinionated**. While 400 is also valid, 422 provides better semantic precision for JSON APIs.

**Design principle**: Errors should be as specific as possible to enable smart client handling.

**Extracted via**: Canon initiation multi-source triangulation
**Confidence**: 0.97 (code + tests + docs explicitly use 422)
