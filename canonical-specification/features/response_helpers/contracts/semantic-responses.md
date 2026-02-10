---
type: contract
name: semantic-responses
version: 1.0.0
feature: response_helpers
---

# Semantic Response Helpers

## Purpose

Provide semantic functions for common HTTP responses instead of manual status code construction.

## Interface

```lisp
(ok data)              ; 200 OK
(created data)         ; 201 Created
(no-content)           ; 204 No Content
(bad-request msg)      ; 400 Bad Request
(unauthorized msg)     ; 401 Unauthorized
(forbidden msg)        ; 403 Forbidden
(not-found &optional msg)  ; 404 Not Found
(conflict msg)         ; 409 Conflict
(error-response status msg)  ; Custom status
```

## Requirements

**R1**: Response helpers MUST set appropriate HTTP status code.

**R2**: Success helpers (ok, created) MUST serialize data to JSON.

**R3**: Error helpers MUST return structured error format.

**R4**: Helper functions MUST be usable as direct return values from handlers.

## Guarantees

**G1**: All responses include correct Content-Type header.

**G2**: Error responses match format specified in error-response-format.md.

**G3**: Helpers return Lack-compatible response tuples.
