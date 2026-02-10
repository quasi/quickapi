---
type: contract
name: special-variables
version: 1.0.0
feature: request_context
---

# Request Context Special Variables

## Purpose

Provide access to request data via special variables bound during request handling.

## Interface

```lisp
*body*          ; Parsed JSON request body (hash-table or nil)
*request*       ; Lack request environment (plist)
*current-user*  ; Authenticated user data (when auth enabled)
```

## Requirements

**R1**: `*body*` MUST be bound for POST/PUT/PATCH requests with JSON content-type.

**R2**: `*request*` MUST contain the complete Lack request environment.

**R3**: `*current-user*` MUST be bound only when authentication succeeds.

## Guarantees

**G1**: Variables are dynamically scoped (thread-safe, request-isolated).

**G2**: Variables are `nil` when not applicable (e.g., `*body*` for GET requests).

**G3**: Variables are accessible in handler body without explicit binding.
