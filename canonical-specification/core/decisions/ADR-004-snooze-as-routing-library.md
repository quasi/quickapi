# ADR-004: Snooze as Routing Library

**Status**: Accepted
**Date**: 2026-01 (extracted from implementation)
**Deciders**: quickAPI core team
**Extraction Confidence**: 0.97

## Context

quickAPI needed a routing library to handle HTTP request matching and dispatch. Common Lisp has several options for web routing, each with different trade-offs in terms of abstraction level, maturity, and integration with HTTP servers.

## Decision

**Use Snooze as the core routing library.**

quickAPI wraps Snooze's `defroute` macro with higher-level `api-get`, `api-post`, etc. macros while delegating actual routing logic to Snooze.

## Rationale

Snooze chosen for these characteristics:

### 1. **Small and Focused** (~850 LoC)
- Easy to understand the entire library
- Low dependency footprint
- Aligns with "thin veneer" philosophy
- Can read source code to debug issues

### 2. **CLOS-Based Design**
- Routes defined as CLOS methods
- Natural extension through method combination
- Familiar to Common Lisp developers
- Enables introspection and metaprogramming

### 3. **HTTP Condition System**
- Elegant error handling via `http-condition`
- Control flow through condition signaling
- Natural integration with CL condition system
- Enables clean error responses

### 4. **Hunchentoot Integration**
- Designed to work with Hunchentoot
- `make-hunchentoot-app` for easy setup
- Battle-tested combination

### 5. **Mature and Stable**
- Active development
- Used in production systems
- Good Common Lisp community support

## Alternatives Considered

### Caveman2
**Rejected**: Full-featured web framework (part of Clack ecosystem)
```lisp
;; Caveman2 approach
(defroute "/users/:id" ()
  ...)
```
**Why rejected**:
- Too heavy (full framework, not just routing)
- Pulls in entire Clack/Lack stack
- quickAPI wants minimal dependencies
- Fighting framework abstractions conflicts with "thin veneer" goal

### Ningle
**Rejected**: Lightweight Clack-based routing microframework
```lisp
;; Ningle approach
(setf (ningle:route *app* "/users/:id")
  (lambda (params)
    ...))
```
**Why rejected**:
- Still requires Clack infrastructure
- Less Common Lisp idiomatic (more Sinatra-like)
- Fewer CLOS benefits

### Hunchentoot Easy-Handlers Alone
**Rejected**: Use only Hunchentoot's built-in routing
```lisp
;; Hunchentoot alone
(hunchentoot:define-easy-handler (users :uri "/users") ()
  ...)
```
**Why rejected**:
- No path parameter support (would need manual parsing)
- Less flexible routing
- More verbose

### Custom Routing from Scratch
**Rejected**: Implement routing logic directly
**Why rejected**:
- Reinventing wheel
- URL parsing, dispatch, method handling is complex
- Violates "use proven libraries" philosophy
- More surface area for bugs

## Consequences

### Positive
- ✅ **Small dependency**: Only ~850 LoC to understand
- ✅ **CLOS integration**: Natural for CL developers
- ✅ **Condition-based errors**: Elegant error handling
- ✅ **Hunchentoot compatibility**: Works with widely-used server
- ✅ **Extensible**: Can hook into Snooze mechanisms (e.g., custom resource naming)
- ✅ **Proven**: Used in production by others

### Negative
- ⚠️ **Less popular**: Smaller community than Caveman/Clack
- ⚠️ **Tied to Hunchentoot**: Can't easily switch servers
- ⚠️ **CLOS learning curve**: New CL devs may find method-based routing unfamiliar

### Neutral
- Snooze methods generated behind the scenes (transparent to users)
- HTTP server choice (Hunchentoot) follows from Snooze choice

## Integration Architecture

```
quickAPI User Code
        ↓
api-get/api-post/etc macros (thin wrapper)
        ↓
Snooze defroute (routing logic)
        ↓
Hunchentoot (HTTP server)
```

### Customization Points Used

1. **`*resource-name-function*`**: Custom function for URI matching (enables path parameter registry)
2. **`defroute` macro wrapping**: Generate Snooze routes from quickAPI macros
3. **`http-condition`**: Signal errors that Snooze converts to HTTP responses
4. **`make-hunchentoot-app`**: Create dispatch table for Hunchentoot

## Implementation

**Dependencies** (from quickapi.asd):
```lisp
:depends-on ("snooze"           ; Routing
             "hunchentoot"      ; HTTP server
             "com.inuoe.jzon"   ; JSON
             "cl-ppcre"         ; Regex
             "sqlite")          ; Database
```

**Core Integration**: `src/core.lisp:119-142` (start/stop functions)

**Route Macro Template**:
```lisp
(defmacro api-get (uri lambda-list &body body)
  `(snooze:defroute ,resource-name (:get ,uri)
     ,@body))
```

## Dependency Philosophy

Aligns with core principle: **"Curated stack of proven libraries"**

| Library | Purpose | Why Chosen |
|---------|---------|------------|
| Snooze | Routing | Small, CLOS-based, elegant |
| Hunchentoot | HTTP Server | Mature, widely used |
| jzon | JSON | Modern, fast |
| cl-ppcre | Regex | De facto standard |
| sqlite | Database | Serverless, simple |

Each chosen for **maturity + simplicity + common lisp idioms**.

## Evidence

**Code**: All route macros expand to `snooze:defroute`
**Tests**: Integration tests verify Snooze integration
**Docs**: README mentions Snooze explicitly, ~850 LoC cited
**Examples**: All examples use quickAPI macros which wrap Snooze

## Related Decisions

- ADR-001: Route registry (extends Snooze via `*resource-name-function*`)
- ADR-002: No middleware (Snooze also minimal, aligns with philosophy)
- ADR-003: HTTP conditions (uses Snooze's `http-condition` system)

## Risks and Mitigations

### Risk: Snooze abandonment
**Likelihood**: Low (active as of 2026)
**Impact**: Medium (would need to swap routing library)
**Mitigation**:
- Snooze is small enough to fork if needed (~850 LoC)
- quickAPI's thin wrapper makes swapping easier
- Could inline critical Snooze code if necessary

### Risk: Hunchentoot lock-in
**Likelihood**: Low (stable for 10+ years)
**Impact**: Low (Hunchentoot is CL standard)
**Mitigation**: Alternative servers possible but not priority

## Future Considerations

- **HTTP/2 support**: Would require Hunchentoot upgrade or alternative server
- **Async I/O**: Snooze is synchronous; async would need different stack
- **WebSocket support**: Not in Snooze, would need additional library

**Current stance**: Current stack sufficient for target use case (JSON REST APIs).

## Notes

Choice of Snooze is **fundamental architectural decision**. Changing routing library would require significant refactoring of quickAPI macros.

**Key insight**: By choosing a small, focused library (Snooze), quickAPI can wrap it thinly without fighting abstractions. Larger frameworks (Caveman) would require more compromise.

**Extracted via**: Canon initiation multi-source triangulation
**Confidence**: 0.97 (explicit dependency, all code uses Snooze)
