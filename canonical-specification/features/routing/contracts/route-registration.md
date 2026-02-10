---
type: contract
name: route-registration
version: 1.0.0
feature: routing
---

# Route Registration Contract

## Purpose

Define HTTP routes using macros that automatically register handlers with the underlying routing library (snooze).

## Interface

```lisp
(api-get uri (&rest options) &body body)
(api-post uri (&rest options) &body body)
(api-put uri (&rest options) &body body)
(api-patch uri (&rest options) &body body)
(api-delete uri (&rest options) &body body)
```

## Requirements

**R1**: Path parameters (e.g., `:id` in `/users/:id`) MUST be automatically extracted and bound as variables in the handler body.

**R2**: Route macros MUST register with the global route registry for dispatch.

**R3**: Multiple routes with the same URI but different HTTP methods MUST be supported.

**R4**: Routes MUST support optional `:auth` parameter for authentication requirements.

## Guarantees

**G1**: Calling a route macro registers the handler immediately at macro expansion time.

**G2**: Path parameter variables are visible in the handler body without explicit declaration.

**G3**: Handler return values are automatically converted to HTTP responses.

## Side Effects

- Modifies `*route-registry*` global hash table
- Defines snooze resources via `snooze:defroute`
