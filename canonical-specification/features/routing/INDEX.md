# Routing Feature

**Status**: stable | **Confidence**: 0.95 | **Category**: core

---

## Overview

Define HTTP routes with automatic path parameter extraction. Five route macros (api-get, api-post, api-put, api-patch, api-delete) provide clean syntax for REST endpoints with zero boilerplate.

**Key Innovation**: Path parameters (`:id`) automatically extracted and bound as lexical variables in handler code.

---

## Quick Reference

### Define Routes

```lisp
;; Simple GET
(api-get "/health" ()
  (ok :status "healthy"))

;; Path parameter (id automatically bound)
(api-get "/users/:id" ()
  (ok :user-id id))

;; POST with body
(api-post "/users" ()
  (validate (require-fields *body* "name" "email"))
  (created :id (create-user *body*)))
```

### API Definition

```lisp
(defapi my-api
  :name "My API"
  :version "1.0"
  :description "My awesome API")
```

### Server Lifecycle

```lisp
(start :port 8080)  ; Start server
(stop)              ; Stop server
```

---

## Primary Contracts

| Contract | Purpose |
|----------|---------|
| `api-get` | Define GET route |
| `api-post` | Define POST route (with *body*) |
| `api-put` | Define PUT route (with *body*) |
| `api-patch` | Define PATCH route (with *body*) |
| `api-delete` | Define DELETE route |
| `defapi` | Define API with metadata |
| `start` | Start HTTP server |
| `stop` | Stop HTTP server |

---

## Core Properties

- Routes are unique per (method, URI pattern) combination
- Path parameters automatically bound as lexical variables
- Literal segments are case-insensitive
- Route registry is global (package-level special variable)

---

## Files

| File | Purpose |
|------|---------|
| `feature.yaml` | Feature metadata and specification |
| `.context.yaml` | Agent context bundle (auto-generated) |

---

## Implementation

**Source**: `src/core.lisp:13-274`
**Tests**: `tests/integration-tests.lisp`, `tests/route-registry-tests.lisp`
**Examples**: `examples/todo-api.lisp`

---

## Related Features

- [`json_serialization`](../json_serialization/) - Automatic JSON response handling
- [`request_context`](../request_context/) - Special variables (*request*, *body*)
- [`validation`](../validation/) - Validate request data
- [`error_handling`](../error_handling/) - Standardized error responses

---

## Design Decisions

- [ADR-001: Route Registry Architecture](../../core/decisions/ADR-001-route-registry-architecture.md)
- [ADR-004: Snooze as Routing Library](../../core/decisions/ADR-004-snooze-as-routing-library.md)

---

*Part of Canon v0.2.0 | [Features Index](../INDEX.md) | [Canon Root](../../INDEX.md)*
