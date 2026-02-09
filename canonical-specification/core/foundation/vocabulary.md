# Foundation Vocabulary [DRAFT]

**Status**: DRAFT (extracted via canon-initiate)
**Confidence**: 0.92
**Last Updated**: 2026-01-24

---

## Core Domain Terms

### API
An instance of a REST API server defined via `defapi`. Contains metadata (name, version, description) and manages a collection of routes.

**Synonyms**: Service, endpoint collection
**Related**: defapi, *api*, route

### Route
A mapping from an HTTP method and URI pattern to a handler function. Routes are defined using route macros (`api-get`, `api-post`, etc.).

**Synonyms**: Endpoint, resource, handler
**Components**: method, URI pattern, handler body
**Related**: route macro, URI pattern, handler

### URI Pattern
A string template for matching request URIs, supporting literal segments and parameter placeholders.

**Format**: `/segment/:parameter/segment`
**Example**: `/users/:id`, `/todos/:id/toggle`
**Components**: literal segments, parameter segments
**Related**: path parameter, route registry

### Path Parameter
A variable extracted from a URI pattern, indicated by a `:` prefix in the pattern.

**Format**: `:name`
**Example**: In `/users/:id`, `id` is a path parameter
**Binding**: Automatically bound as a variable in the route handler
**Type**: String (extracted from URI)
**Related**: URI pattern, parameter extraction

### Route Macro
A macro that defines a route for a specific HTTP method with automatic JSON handling.

**Types**: `api-get`, `api-post`, `api-put`, `api-patch`, `api-delete`
**Features**: Automatic JSON serialization, body parsing, path parameter extraction
**Related**: route, HTTP method

### Handler
The body of a route definition, containing the business logic for processing requests.

**Context**: Executes with special variables bound (*request*, *body*, *db* if applicable)
**Parameters**: Path parameters automatically bound as variables
**Return**: Data to be JSON-serialized, or response helper call
**Related**: route, request context

---

## Request/Response Cycle

### Request
An incoming HTTP request to the API.

**Components**: method, URI, headers, body (optional)
**Representation**: Snooze request object (bound to *request*)
**Related**: *request*, request context

### Request Context
The set of special variables and extracted data available during request handling.

**Special Variables**:
- `*request*`: Snooze request object
- `*body*`: Parsed JSON body (POST/PUT/PATCH)
- `*db*`: Database connection (if within with-db)

**Extracted Data**: Path parameters (as variables)
**Related**: request, special variable

### Request Body
The JSON payload of a POST, PUT, or PATCH request.

**Parsing**: Automatic for POST/PUT/PATCH with JSON content-type
**Representation**: Hash table with string keys
**Access**: Via `*body*` special variable or `(body)` function
**Related**: *body*, JSON, hash table

### Response
The HTTP response returned from a route handler.

**Format**: Snooze response `(status headers body)`
**Construction**: Automatic from return value or via response helpers
**Serialization**: Return values automatically JSON-serialized
**Related**: response helper, JSON

### Response Helper
A function that constructs a standardized HTTP response.

**Success Helpers**: `ok` (200), `created` (201), `no-content` (204)
**Error Helpers**: `bad-request` (400), `not-found` (404), `error-response` (custom)
**Related**: response, HTTP status

### Error Response
A structured error response with standardized format.

**Format**:
```json
{
  "error": "error_type",
  "message": "Human-readable message",
  "details": <optional additional data>
}
```

**Types**: `bad_request`, `not_found`, `validation_error`, etc.
**Mechanism**: Signals Snooze `http-condition`
**Related**: response helper, validation error

---

## Validation

### Validation
The process of checking request data against constraints using the `validate` macro.

**Scope**: Typically validates `*body*` (request body)
**Mechanism**: Collects all errors, signals HTTP 422 if any exist
**Behavior**: Non-fail-fast (collects all errors before signaling)
**Related**: validate macro, validator, validation error

### Validator
A function that checks a specific constraint on request data.

**Types**:
- `require-fields`: Presence check
- `require-type`: Type constraint
- `require-length`: String/list/vector length
- `require-range`: Numeric bounds
- `require-pattern`: Regex match

**Behavior**: Adds error to `*validation-errors*` if constraint violated
**Related**: validation, validation error

### Validation Error
An error indicating that request data failed validation.

**Status**: HTTP 422 (Unprocessable Entity)
**Format**:
```json
{
  "error": "validation_error",
  "message": "Validation failed",
  "details": [
    {"field": "name", "message": "required"},
    {"field": "age", "message": "must be at least 0"}
  ]
}
```

**Collection**: All errors collected before response
**Related**: validation, validator

---

## JSON Handling

### JSON
JavaScript Object Notation, the only supported content type for quickAPI.

**Request**: Content-Type `application/json`
**Response**: Content-Type `application/json`
**Library**: `com.inuoe.jzon`
**Related**: serialization, deserialization

### Serialization
Converting Common Lisp data to JSON.

**Automatic**: Route macros automatically serialize return values
**Function**: `com.inuoe.jzon:stringify`
**Supported Types**: Hash tables, lists, primitives
**Related**: JSON, response

### Deserialization
Parsing JSON request body to Common Lisp data.

**Automatic**: For POST/PUT/PATCH with JSON content-type
**Function**: `com.inuoe.jzon:parse`
**Result**: Hash table with string keys
**Binding**: Bound to `*body*`
**Related**: JSON, request body

### Hash Table
The Common Lisp data structure used to represent JSON objects.

**Test**: `:test 'equal` (required for string keys)
**Keys**: Strings (JSON object keys)
**Values**: Any serializable type
**Construction**: `(make-hash-table :test 'equal)`
**Related**: JSON, *body*

---

## Database

### Database Connection
An open SQLite database handle.

**Binding**: Via `with-db` macro to `*db*` special variable
**Lifecycle**: Automatically opened and closed
**Type**: SQLite connection handle
**Related**: *db*, with-db

### Table
A SQLite database table.

**Creation**: Via `ensure-table` (idempotent)
**Schema**: Column definitions with types and constraints
**Related**: ensure-table, database

### Row
A single record from a database query.

**Representation**: List of values
**Conversion**: `row-to-hash` converts to hash table for JSON
**Related**: query, hash table

---

## Server Management

### Server
The running Hunchentoot HTTP server instance.

**State**: Stored in `*server*` special variable
**Lifecycle**: Started with `start`, stopped with `stop`
**Singleton**: Only one server can run at a time
**Related**: start, stop, *server*

### Acceptor
The Hunchentoot acceptor that listens for HTTP connections.

**Type**: `hunchentoot:easy-acceptor`
**Configuration**: Port and bind address
**Dispatcher**: Snooze routing via custom resource name function
**Related**: server, Hunchentoot

---

## Internal Infrastructure

### Route Registry
A global hash table mapping (method, URI-pattern) pairs to route entries.

**Variable**: `*route-registry*`
**Key**: `(cons method uri-pattern)`
**Value**: `route-entry` struct
**Purpose**: Enable flexible URI pattern matching
**Related**: route entry, route matching

### Route Entry
A struct holding information about a registered route.

**Fields**:
- `pattern`: Original URI pattern string
- `resource-name`: Snooze resource name symbol
- `segments`: Parsed segment descriptors
- `method`: HTTP method keyword

**Related**: route registry, segment descriptor

### Segment Descriptor
A parsed component of a URI pattern.

**Types**:
- `(:literal . "string")`: Literal segment that must match exactly
- `(:param . symbol)`: Parameter segment to extract

**Example**: `/todos/:id` → `((:literal . "todos") (:param . ID))`
**Related**: URI pattern, route entry

### Route Matching
The process of matching an incoming request URI to a registered route pattern.

**Algorithm**:
1. Parse request URI into segments
2. Try to match against registered patterns
3. Check literal segments match (case-insensitive)
4. Extract parameter values
5. Return resource name and extracted parameters

**Function**: `match-uri-to-route`
**Related**: route registry, URI pattern

---

## Special Variables (Common Lisp Idiom)

### *api*
The current API instance created by `defapi`.

**Type**: `api` class instance
**Scope**: Global
**Related**: defapi, API

### *server*
The running Hunchentoot acceptor, or `nil` if not running.

**Type**: `hunchentoot:easy-acceptor` or `nil`
**Scope**: Global
**Related**: server, start, stop

### *request*
The current Snooze request object.

**Type**: Snooze request
**Scope**: Dynamically bound during request handling
**Related**: request, request context

### *body*
The parsed JSON request body.

**Type**: Hash table or `nil`
**Scope**: Dynamically bound during POST/PUT/PATCH request handling
**Value**: `nil` for GET/DELETE
**Related**: request body, hash table

### *db*
The current SQLite database connection.

**Type**: SQLite connection handle or `nil`
**Scope**: Dynamically bound within `with-db` block
**Related**: database connection, with-db

### *route-registry*
The global route registry hash table.

**Type**: Hash table with `(:cons method uri-pattern)` keys
**Scope**: Global
**Initialization**: `(make-hash-table :test 'equal)`
**Related**: route registry, route entry

### *validation-errors*
List of accumulated validation errors during a `validate` block.

**Type**: List of hash tables
**Scope**: Dynamically bound during `validate` macro
**Format**: Each entry is `{"field": "...", "message": "..."}`
**Related**: validation, validation error

---

## External Dependencies

### Snooze
CLOS-based HTTP routing library (~850 LoC).

**Usage**: Route definition, request handling, HTTP conditions
**Integration**: Custom resource name function for URI matching
**Related**: defroute, http-condition

### Hunchentoot
Common Lisp HTTP server.

**Usage**: HTTP server/acceptor, dispatch table
**Related**: acceptor, server

### com.inuoe.jzon
Modern JSON parsing and serialization library.

**Usage**: Parse JSON requests, stringify JSON responses
**Functions**: `parse`, `stringify`
**Related**: JSON, serialization

### cl-ppcre
Perl-compatible regular expression library.

**Usage**: URI parsing, pattern validation
**Functions**: `split`, `scan`
**Related**: URI pattern, validation

### cl-sqlite
SQLite database bindings.

**Usage**: Database operations
**Optional**: Only required if using database features
**Related**: database, with-db

---

## Conventions

### Naming
- **Route macros**: `api-<method>` (e.g., `api-get`)
- **Special variables**: `*name*` (e.g., `*body*`, `*db*`)
- **Response helpers**: Lowercase semantic names (e.g., `ok`, `not-found`)
- **Internal functions**: Descriptive names (e.g., `parse-uri-pattern`)

### Package
- **Package name**: `:quickapi`
- **Exports**: All user-facing API in `src/package.lisp`
- **Re-exports**: Some symbols from dependencies (e.g., Snooze, SQLite)

### Code Organization
- **Loading order**: package → response → validation → core → sqlite
- **ABOUTME comments**: Every source file has purpose statement
- **Documentation**: Comprehensive docstrings for all public API

---

## Design Philosophy

quickAPI follows these principles:

1. **Thin veneer over proven libraries** - Minimal abstraction
2. **Automatic everything** - JSON, validation, errors handled automatically
3. **5 macros, not 50** - Small, focused API surface
4. **Focus on business logic** - Infrastructure handled by framework
5. **No configuration files** - Everything in code
6. **No middleware** - Keep it simple

---

*This vocabulary extracted via canon-initiate from working implementation.*
*Confidence scores available in `.canon-initiation/` artifacts.*
