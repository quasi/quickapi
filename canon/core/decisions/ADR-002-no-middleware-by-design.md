# ADR-002: No Middleware by Design

**Status**: Accepted
**Date**: 2026-01 (extracted from implementation)
**Deciders**: quickAPI core team
**Extraction Confidence**: 0.95

## Context

Modern web frameworks often provide middleware hooks for cross-cutting concerns like authentication, logging, CORS, request transformation, etc. quickAPI needed to decide whether to provide middleware infrastructure.

## Decision

**Explicitly no middleware system.**

quickAPI provides built-in automatic features (JSON serialization, validation, error handling) without middleware hooks for user customization. The framework focuses on convention over configuration.

## Rationale

Aligned with core philosophy: **"Focus on business logic, not infrastructure"**

1. **Simplicity**: No middleware = no complex execution chains to understand
2. **Predictability**: Clear execution flow without magic hooks
3. **Small scope**: Target is small-to-medium projects that don't need extensibility
4. **Thin veneer**: Stay close to underlying Snooze/Hunchentoot behavior
5. **Less abstraction**: Users can drop to Snooze/Hunchentoot when needed

## Alternatives Considered

### Ring-Style Middleware (Clojure model)
**Rejected**: Compose middleware as nested function wrappers
```lisp
;; Rejected approach
(-> app
    (wrap-authentication)
    (wrap-logging)
    (wrap-cors))
```
**Why rejected**: Adds complexity, fights "5 macros not 50 abstractions" philosophy

### Before/After Hooks
**Rejected**: Provide `:before` and `:after` hooks on routes
```lisp
;; Rejected approach
(api-get "/users" ()
  :before (check-auth)
  :after (log-response)
  ...)
```
**Why rejected**: Still adds configuration surface, unclear ordering

### Plugin System
**Rejected**: Extensibility via plugins/modules
**Why rejected**: Over-engineering for target use case

## Consequences

### Positive
- ✅ **Minimal API surface**: Only 5 route macros + helpers to learn
- ✅ **Clear execution flow**: No hidden behavior
- ✅ **Fast to learn**: No middleware ordering, composition, or configuration
- ✅ **Easy to debug**: No middleware stack to trace through
- ✅ **Predictable**: Same behavior every time, no side effects from middleware

### Negative
- ⚠️ **Limited extensibility**: Can't easily add cross-cutting concerns
- ⚠️ **Code duplication**: Authentication/logging repeated across routes
- ⚠️ **Not suitable for complex apps**: Enterprises need middleware for compliance, auditing
- ⚠️ **Escape hatch required**: Users must drop to Snooze/Hunchentoot for advanced needs

### Mitigations

For users who need middleware-like behavior:

1. **Use Snooze hooks directly**: Snooze provides extensibility points
2. **Wrap routes manually**: Common logic can be extracted to helper functions
3. **Hunchentoot acceptor customization**: Customize server behavior at HTTP layer
4. **Move to different framework**: quickAPI explicitly not for complex use cases

## Implementation

**Evidence**:
- No middleware code in codebase
- README explicitly states "No middleware" as feature
- Examples show no middleware usage
- Package exports contain no middleware-related symbols

**Location**: Philosophy documented in README.md:20

## Target Users

This decision aligns with stated target users:
- Small-to-medium JSON APIs
- Prototypes and MVPs
- Personal projects
- Learning Common Lisp web development

**NOT for**:
- Enterprise applications
- Multi-tenant SaaS
- Apps requiring complex auth flows
- Compliance-heavy domains

## Related Decisions

- ADR Philosophy: "Thin veneer over proven libraries"
- ADR Philosophy: "5 macros, not 50 abstractions"
- Canon system scope: Explicitly states "non-goals: Large-scale enterprise applications"

## Future Considerations

If middleware becomes necessary (e.g., 80% of users request it):

1. **Option 1**: Add minimal `:before`/`:after` hooks on route macros
2. **Option 2**: Create separate middleware layer library (keep quickAPI core minimal)
3. **Option 3**: Recommend migration to full-featured framework

**Current stance**: Wait for user feedback before adding complexity.

## Notes

This is a **controversial but intentional** decision. The philosophy is:
- **Better**: Simple and limited, easy to understand
- **Worse**: Complex and flexible, hard to master

For users who need flexibility, Snooze and Hunchentoot provide full control.

**Extracted via**: Canon initiation multi-source triangulation
**Confidence**: 0.95 (explicit documentation + zero middleware code)
