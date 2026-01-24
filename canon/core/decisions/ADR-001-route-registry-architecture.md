# ADR-001: Route Registry Architecture

**Status**: Accepted
**Date**: 2026-01 (extracted from implementation)
**Deciders**: quickAPI core team
**Extraction Confidence**: 0.95

## Context

quickAPI needed a way to support path parameters in URI patterns (e.g., `/users/:id`) while integrating with Snooze's routing system. Path parameters needed to be automatically extracted and bound as lexical variables in route handler code.

## Decision

Implemented a route registry system with:

1. **Central Registry**: Hash table (`*route-registry*`) mapping (method . uri-pattern) to route metadata
2. **URI Pattern Parsing**: `parse-uri-pattern` function that parses URI templates into segment descriptors
3. **Runtime Matching**: `match-uri-to-route` function that matches incoming requests to registered patterns
4. **Automatic Binding**: Path parameters extracted during macro expansion and bound as lexical variables
5. **Snooze Integration**: Custom `quickapi-resource-name` function hooks into Snooze routing

## Alternatives Considered

### Manual Parameter Extraction
**Rejected**: Would require users to manually extract path parameters from URIs
```lisp
;; Rejected approach
(api-get "/users/:id" ()
  (let ((id (extract-param :id *request*)))  ; Manual extraction
    ...))
```
**Why rejected**: Too verbose, defeats the "minimal boilerplate" philosophy

### Compile-Time Only Routing
**Rejected**: Generate all routing logic at compile time without runtime registry
**Why rejected**: Less flexible, harder to introspect routes, complicates debugging

### Direct Snooze defroute Usage
**Rejected**: Skip the registry, use Snooze defroute directly with manual parameter handling
**Why rejected**: Exposes Snooze internals, more complex for users

## Consequences

### Positive
- ✅ **Automatic parameter extraction**: Users write clean, declarative routes
- ✅ **Familiar syntax**: `:id` style matches Express.js, Flask, Rails conventions
- ✅ **Runtime introspection**: Registry enables route listing, debugging, documentation generation
- ✅ **Flexible**: Easy to add features like route guards, middleware in future
- ✅ **Integration**: Works seamlessly with Snooze without fighting the framework

### Negative
- ⚠️ **Slight overhead**: Runtime pattern matching on each request (negligible in practice)
- ⚠️ **Complexity**: Additional layer over Snooze (but isolated in 100 LoC)
- ⚠️ **Lambda-list deprecated**: Route macros still require empty `()` parameter (legacy artifact)

### Neutral
- Registry is global (`*route-registry*`), shared across all API instances
- Pattern matching is case-insensitive for literal segments
- Parameters are interned in `:quickapi` package for consistency

## Implementation

**Location**: `src/core.lisp:13-117`
**Commit**: 6e9351a (2026-01-25)

Key functions:
- `parse-uri-pattern`: Splits URI into `:literal` and `:param` segments
- `register-route`: Stores route pattern in registry
- `match-uri-to-route`: Matches request URI to registered pattern
- `quickapi-resource-name`: Integration point with Snooze

## Evidence

**Code**: Route registry implementation in core.lisp
**Tests**: 20 unit tests in tests/route-registry-tests.lisp (100% pass rate)
**Usage**: All examples use path parameter syntax successfully
**Documentation**: README shows `:id` style parameters in examples

## Related Decisions

- ADR-004: Choice of Snooze as routing library (enables this architecture)
- Future: If middleware is added, will leverage this registry for routing hooks

## Notes

This was the second major architectural iteration. The first version likely used manual parameter extraction (evidence: deprecated lambda-list parameter still required). The current approach represents a maturation toward automatic, declarative routing.

**Extracted via**: Canon initiation multi-source triangulation
**Confidence**: 0.95 (code + tests + examples aligned)
