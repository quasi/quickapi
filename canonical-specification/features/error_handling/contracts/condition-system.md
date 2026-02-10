---
type: contract
name: condition-system
version: 1.0.0
feature: error_handling
---

# Condition System for HTTP Errors

## Purpose

Map Common Lisp conditions to HTTP error responses with structured error format.

## Interface

Condition hierarchy:
```
quickapi-error (base)
  ├── http-error
  │   ├── client-error (4xx)
  │   │   ├── bad-request-error (400)
  │   │   ├── unauthorized-error (401)
  │   │   ├── forbidden-error (403)
  │   │   ├── not-found-error (404)
  │   │   ├── conflict-error (409)
  │   │   └── validation-error (422)
  │   └── server-error (5xx)
  │       └── internal-server-error (500)
  └── database-error
      ├── record-not-found
      └── duplicate-record
```

## Requirements

**R1**: All quickAPI errors MUST inherit from `quickapi-error`.

**R2**: HTTP errors MUST include `:status` slot with HTTP status code.

**R3**: Condition handlers MUST convert conditions to structured JSON responses.

**R4**: Unhandled errors MUST return 500 Internal Server Error.

## Guarantees

**G1**: Error responses match format in error-response-format.md.

**G2**: Common Lisp restarts are available for error recovery.

**G3**: Error details include machine-readable error type strings.
