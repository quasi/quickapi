# Route Registry Architecture

quickAPI uses a **route registry system** to enable automatic path parameter extraction and flexible URI pattern matching. This document explains how it works internally.

## Overview

When you write:
```lisp
(api-get "/users/:id" ()
  (find-user id))  ; 'id' is automatically bound
```

Behind the scenes, the route registry:
1. Parses the URI pattern `/users/:id` into segments
2. Registers the pattern with metadata
3. Matches incoming requests to patterns at runtime
4. Extracts path parameters and binds them as variables

## Components

### 1. Route Registry (`*route-registry*`)

A global hash table mapping `(method . uri-pattern)` keys to route metadata:

```lisp
(defvar *route-registry* (make-hash-table :test 'equal)
  "Registry of route patterns to resource names for URI matching.")
```

**Example entries**:
```
(:get . "/users")       → #<ROUTE-ENTRY /users>
(:get . "/users/:id")   → #<ROUTE-ENTRY /users/:id>
(:post . "/users")      → #<ROUTE-ENTRY /users>
```

### 2. Route Entry Structure

```lisp
(defstruct route-entry
  pattern        ; Original pattern: "/users/:id"
  resource-name  ; Unique route identifier symbol
  segments       ; Parsed: ((:literal . "users") (:param . ID))
  method)        ; HTTP method: :GET, :POST, etc.
```

### 3. URI Pattern Parser

**Function**: `parse-uri-pattern`
**Location**: `src/core.lisp:49-58`

Converts URI templates into segment descriptors:

```lisp
(parse-uri-pattern "/users/:id/posts/:post-id")
; → ((:literal . "users")
;    (:param . ID)
;    (:literal . "posts")
;    (:param . POST-ID))
```

**Rules**:
- `/` splits into segments
- Segments starting with `:` are parameters
- Parameter symbols are interned in `:quickapi` package (uppercase)
- Literal segments are preserved as strings

### 4. Route Registration

**Function**: `register-route`
**Location**: `src/core.lisp:60-68`

Called during macro expansion to register route patterns:

```lisp
(register-route "/users/:id" 'users-id-resource :get)
```

Stores the parsed route entry in `*route-registry*` for later matching.

### 5. URI Matching

**Function**: `match-uri-to-route`
**Location**: `src/core.lisp:70-99`

Matches incoming request URIs to registered patterns:

```lisp
(match-uri-to-route "/users/123" :get)
; → (values 'users-id-resource '((ID . "123")))
```

**Algorithm**:
1. Parse incoming URI into segments
2. Iterate registered routes for matching HTTP method
3. Compare segment counts (must match)
4. Match literal segments (case-insensitive)
5. Collect parameter values
6. Return resource name + extracted parameters

### 6. Clack/Lack Integration

**Registry**: `*route-handlers*`
**Location**: `src/lack-app.lisp`

Each route registers both a pattern in `*route-registry*` (for URI matching) and a handler closure in `*route-handlers*` (for dispatch):

```lisp
(register-handler :get "/users/:id" handler-fn)
```

The Clack application dispatches incoming requests by calling `match-uri-to-route` to find the matching pattern, then invokes the corresponding handler closure with the extracted parameters.

## Example Flow

### Route Definition

```lisp
(api-get "/users/:id/posts/:post-id" ()
  (find-post user-id post-id))
```

**Macro expansion**:
1. Parse pattern: `/users/:id/posts/:post-id`
2. Extract parameters: `ID`, `POST-ID`
3. Register pattern in `*route-registry*` via `register-route`
4. Register handler closure in `*route-handlers*` via `register-handler`
5. Handler closure binds extracted params as local variables

### Request Handling

**Incoming request**: `GET /users/42/posts/99`

1. Clack app receives request, extracts method and path
2. `match-uri-to-route` searches `*route-registry*`
3. Finds pattern `/users/:id/posts/:post-id`
4. Extracts: `((ID . "42") (POST-ID . "99"))`
5. Looks up handler closure in `*route-handlers*`
6. Handler executes with `user-id=42`, `post-id=99` bound

## Design Decisions

### Why Runtime Registry?

**Pros**:
- Introspection: Can list all routes programmatically
- Debugging: Can inspect registered patterns
- Flexibility: Easy to add route guards, middleware hooks
- Testing: Can verify route registration

**Cons**:
- Slight runtime overhead (negligible in practice)
- Global state (`*route-registry*`)

**Rationale**: Flexibility and debuggability outweigh minimal overhead.

### Why Case-Insensitive Literal Matching?

HTTP URIs are typically case-sensitive, but quickAPI uses case-insensitive matching for literal segments:

```lisp
/users ≡ /USERS ≡ /Users
```

**Rationale**: Follows common web framework conventions (Rails, Express). Can be changed if needed.

### Why Parameters Interned in `:quickapi`?

All parameter symbols are interned in the `:quickapi` package:

```lisp
:id → QUICKAPI::ID
:user-id → QUICKAPI::USER-ID
```

**Rationale**: Consistent symbol home package prevents naming conflicts across packages.

## Performance Considerations

### Registry Size

The registry uses a hash table with `(method . uri-pattern)` keys. Lookup is O(1) for exact matches, O(n) for pattern matching (where n = number of routes with same HTTP method).

**Typical overhead**: ~0.01ms for 100 routes

### Pattern Matching

Each incoming request:
1. Parses URI: ~0.001ms
2. Iterates method-specific routes: O(n) where n = routes for that method
3. Compares segments: O(m) where m = segments per route

**Typical overhead**: ~0.01-0.1ms depending on route count

**Optimization**: Routes are tried in registration order. Common routes should be registered first.

## Route Introspection

### List All Routes

```lisp
(defun list-routes ()
  "List all registered routes."
  (maphash (lambda (key entry)
             (format t "~A ~A → ~A~%"
                     (route-entry-method entry)
                     (route-entry-pattern entry)
                     (route-entry-resource-name entry)))
           *route-registry*))
```

### Check Route Registration

```lisp
(defun route-registered-p (uri method)
  "Check if a route is registered."
  (gethash (cons method uri) *route-registry*))
```

## Testing

See `tests/route-registry-tests.lisp` for comprehensive test coverage:

- **7 tests** for `parse-uri-pattern`
- **11 tests** for `match-uri-to-route`
- **2 tests** for `register-route`

**Coverage**:
- Empty paths, single/multiple parameters
- Literal-only, mixed literal/param routes
- Case sensitivity, query string handling
- Multiple routes, method matching
- Edge cases (segment count mismatch, literal mismatch)

## Future Extensions

The route registry enables future features:

### Route Guards

```lisp
(api-get "/admin/:page" ()
  :guard (require-admin)  ; Not yet implemented
  ...)
```

### Route Metadata

```lisp
(api-get "/users/:id" ()
  :metadata '(:rate-limit 100 :cache-ttl 60)  ; Not yet implemented
  ...)
```

### Documentation Generation

The registry can be used to generate API documentation, OpenAPI specs, etc.

## Related Documentation

- [ADR-001: Route Registry Architecture](../canonical-specification/core/decisions/ADR-001-route-registry-architecture.md) - Design rationale
- [API Reference](../README.md) - User-facing route macros
- [Tutorial](02-tutorial.md) - Using path parameters in practice

## Implementation Files

| File | Purpose | Lines |
|------|---------|-------|
| `src/core.lisp` | Route registry + route macros | ~349 LoC |
| `tests/route-registry-tests.lisp` | Unit tests | ~240 LoC |

---

**Note**: This is internal architecture documentation. Users don't need to understand the registry to use quickAPI - path parameters "just work" automatically.
