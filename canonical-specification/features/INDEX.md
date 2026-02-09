# Features Index

**Purpose**: Feature specifications document the capabilities of quickAPI. Each feature includes contracts, scenarios, properties, and context bundles.

---

## Quick Navigation

| Need to... | Feature |
|------------|---------|
| Define HTTP routes with path parameters | [`routing`](routing/) |
| Parse/serialize JSON automatically | [`json_serialization`](json_serialization/) |
| Validate request data | [`validation`](validation/) |
| Access request context (*request*, *body*, *db*) | [`request_context`](request_context/) |
| Return HTTP responses (ok, created, not-found) | [`response_helpers`](response_helpers/) |
| Work with SQLite database | [`sqlite_integration`](sqlite_integration/) |
| Handle errors with standard format | [`error_handling`](error_handling/) |

---

## All Features

### routing

**Tagline**: Define HTTP routes with automatic path parameter extraction

**Status**: stable | **Confidence**: 0.95

**Key Capabilities**:
- Five route macros for HTTP methods (GET, POST, PUT, PATCH, DELETE)
- URI patterns with :param placeholders (e.g., /users/:id)
- Automatic path parameter extraction and variable binding
- Route registry for flexible pattern matching
- Integration with Snooze routing infrastructure

**Primary Contracts**: api-get, api-post, api-put, api-patch, api-delete, defapi, start, stop

**Related Features**: json_serialization, request_context, validation

**Files**: [`feature.yaml`](routing/feature.yaml), [`.context.yaml`](routing/.context.yaml)

---

### json_serialization

**Tagline**: Automatic JSON parsing and serialization for requests/responses

**Status**: stable | **Confidence**: 0.95

**Key Capabilities**:
- Automatic JSON serialization of return values
- Automatic JSON deserialization of request bodies
- Content-Type: application/json handling
- Hash tables (:test 'equal) for JSON objects
- Support for all JSON types (string, number, boolean, null, array, object)

**Primary Contracts**: Automatic (handled by route macros)

**Related Features**: routing, request_context, error_handling

**Files**: [`feature.yaml`](json_serialization/feature.yaml), [`.context.yaml`](json_serialization/.context.yaml)

---

### validation

**Tagline**: Validate request data with automatic error collection and 422 responses

**Status**: stable | **Confidence**: 0.97

**Key Capabilities**:
- validate macro for declarative validation
- Five validators: require-fields, require-type, require-length, require-range, require-pattern
- Error collection (non-fail-fast)
- Standardized 422 error responses
- Automatic validation error formatting

**Primary Contracts**: validate, require-fields, require-type, require-length, require-range, require-pattern, validation-error

**Related Features**: request_context, error_handling, json_serialization

**Files**: [`feature.yaml`](validation/feature.yaml), [`.context.yaml`](validation/.context.yaml)

---

### request_context

**Tagline**: Special variables and extracted data available during request handling

**Status**: stable | **Confidence**: 0.93

**Key Capabilities**:
- *request* special variable (Snooze request object)
- *body* special variable (parsed JSON body)
- *db* special variable (database connection)
- Automatic path parameter binding as lexical variables
- Integration with routing, JSON parsing, and database

**Primary Contracts**: *request*, *body*, *db*, path parameter binding

**Related Features**: routing, json_serialization, validation, sqlite_integration

**Files**: [`feature.yaml`](request_context/feature.yaml), [`.context.yaml`](request_context/.context.yaml)

---

### response_helpers

**Tagline**: Standardized HTTP response constructors

**Status**: stable | **Confidence**: 0.95

**Key Capabilities**:
- Success helpers: ok (200), created (201), no-content (204)
- Error helpers: bad-request (400), not-found (404), error-response (custom)
- Automatic JSON serialization
- Keyword arguments → JSON object fields
- Standardized error format

**Primary Contracts**: ok, created, no-content, bad-request, not-found, error-response

**Related Features**: json_serialization, error_handling

**Files**: [`feature.yaml`](response_helpers/feature.yaml), [`.context.yaml`](response_helpers/.context.yaml)

---

### sqlite_integration

**Tagline**: SQLite database helpers for Common Lisp APIs

**Status**: stable | **Confidence**: 0.93

**Key Capabilities**:
- with-db macro for connection management
- ensure-table for idempotent table creation
- query/query-all for SELECT operations
- execute for INSERT/UPDATE/DELETE
- last-insert-rowid for getting generated IDs
- Automatic connection cleanup

**Primary Contracts**: with-db, ensure-table, query, query-all, execute, last-insert-rowid

**Related Features**: request_context, json_serialization, validation

**Files**: [`feature.yaml`](sqlite_integration/feature.yaml), [`.context.yaml`](sqlite_integration/.context.yaml)

---

### error_handling

**Tagline**: Standardized JSON error responses with consistent format

**Status**: stable | **Confidence**: 0.95

**Key Capabilities**:
- Standardized {error, message, details} format
- HTTP status codes (400, 404, 422, 500)
- Validation errors return HTTP 422 with details array
- Error messages are human-readable
- Error codes use lowercase_underscore format

**Primary Contracts**: error-response, bad-request, not-found, validation-error condition

**Related Features**: validation, response_helpers, json_serialization

**Files**: [`feature.yaml`](error_handling/feature.yaml), [`.context.yaml`](error_handling/.context.yaml)

---

## Feature Dependency Graph

```
routing
  ├─→ json_serialization (response handling)
  ├─→ request_context (*request*, *body*)
  ├─→ validation (request validation)
  └─→ error_handling (error responses)

json_serialization
  ├─→ routing (route macros)
  └─→ request_context (*body* parsing)

validation
  ├─→ request_context (*body* validation)
  ├─→ error_handling (422 responses)
  └─→ json_serialization (error serialization)

request_context
  ├─→ routing (path parameters)
  ├─→ json_serialization (*body* parsing)
  └─→ sqlite_integration (*db* binding)

response_helpers
  ├─→ json_serialization (response serialization)
  └─→ error_handling (error responses)

sqlite_integration
  ├─→ request_context (*db* convention)
  ├─→ json_serialization (hash table results)
  └─→ validation (data validation)

error_handling
  ├─→ json_serialization (error serialization)
  ├─→ validation (422 errors)
  └─→ response_helpers (error constructors)
```

---

## Feature Summary Statistics

| Metric | Value |
|--------|-------|
| Total Features | 7 |
| Average Confidence | 0.95 |
| Status | All stable |
| Context Bundles | 7/7 (100%) |
| Dependencies (avg) | 2.3 per feature |

---

*Generated on 2026-01-25 from Canon v0.2.0*
