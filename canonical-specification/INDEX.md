# Canon Index: quickAPI

**Project**: quickAPI - Curated JSON API stack for Common Lisp
**Canon Version**: 0.2.0
**Status**: Stable (extracted via canon-initiate, validated 2026-01-25)
**Last Updated**: 2026-01-25

---

## Purpose

This Canon documents the formal specification for quickAPI, a minimal JSON API framework for Common Lisp. Use this index to navigate specifications, architecture decisions, and feature contracts.

---

## Quick Navigation

| I want to... | Start here |
|--------------|------------|
| Understand core terminology | [`core/foundation/vocabulary.md`](core/foundation/vocabulary.md) |
| Learn why design decisions were made | [`core/decisions/`](core/decisions/) |
| See all features | [`features/`](features/) or Features section below |
| Implement routing logic | [`features/routing/`](features/routing/) |
| Add validation to routes | [`features/validation/`](features/validation/) |
| Work with database | [`features/sqlite_integration/`](features/sqlite_integration/) |
| Understand error responses | [`features/error_handling/`](features/error_handling/) |
| Use request context | [`features/request_context/`](features/request_context/) |
| Return HTTP responses | [`features/response_helpers/`](features/response_helpers/) |
| Handle JSON serialization | [`features/json_serialization/`](features/json_serialization/) |
| Understand the route registry | [`core/decisions/ADR-001-route-registry-architecture.md`](core/decisions/ADR-001-route-registry-architecture.md) |
| Understand middleware philosophy | [`core/decisions/ADR-002-no-middleware-by-design.md`](core/decisions/ADR-002-no-middleware-by-design.md) |
| Understand validation error codes | [`core/decisions/ADR-003-http-422-for-validation-errors.md`](core/decisions/ADR-003-http-422-for-validation-errors.md) |
| Understand routing library choice | [`core/decisions/ADR-004-snooze-as-routing-library.md`](core/decisions/ADR-004-snooze-as-routing-library.md) |

---

## Canon Structure

```
canon/
├── INDEX.md                          # This file
├── canon.yaml                        # Canon manifest
├── README.md                         # Canon overview
├── core/                             # Core foundation
│   ├── INDEX.md                     # Core navigation
│   ├── foundation/
│   │   └── vocabulary.md            # Domain terminology (~423 lines)
│   └── decisions/
│       ├── ADR-001-route-registry-architecture.md
│       ├── ADR-002-no-middleware-by-design.md
│       ├── ADR-003-http-422-for-validation-errors.md
│       └── ADR-004-snooze-as-routing-library.md
└── features/                         # Feature specifications
    ├── INDEX.md                     # Features navigation
    ├── routing/
    │   ├── feature.yaml             # Feature metadata
    │   └── .context.yaml            # Agent context bundle
    ├── json_serialization/
    │   ├── feature.yaml
    │   └── .context.yaml
    ├── validation/
    │   ├── feature.yaml
    │   └── .context.yaml
    ├── request_context/
    │   ├── feature.yaml
    │   └── .context.yaml
    ├── response_helpers/
    │   ├── feature.yaml
    │   └── .context.yaml
    ├── sqlite_integration/
    │   ├── feature.yaml
    │   └── .context.yaml
    └── error_handling/
        ├── feature.yaml
        └── .context.yaml
```

---

## Contents

### Core Foundation

| File | Purpose | Lines | Confidence |
|------|---------|-------|------------|
| [`core/foundation/vocabulary.md`](core/foundation/vocabulary.md) | Canonical terminology for routes, validation, JSON, database, routing | ~423 | 0.92 |

**Key concepts**: API, Route, URI Pattern, Path Parameter, Request/Response Cycle, Validation, JSON Handling, Route Registry, Special Variables

### Architecture Decisions

| File | Decision | Status | Lines | Confidence |
|------|----------|--------|-------|------------|
| [`core/decisions/ADR-001-route-registry-architecture.md`](core/decisions/ADR-001-route-registry-architecture.md) | Route registry with automatic path parameter extraction | Accepted | ~90 | 0.95 |
| [`core/decisions/ADR-002-no-middleware-by-design.md`](core/decisions/ADR-002-no-middleware-by-design.md) | Explicitly no middleware hooks | Accepted | ~130 | 0.95 |
| [`core/decisions/ADR-003-http-422-for-validation-errors.md`](core/decisions/ADR-003-http-422-for-validation-errors.md) | HTTP 422 for validation failures | Accepted | ~160 | 0.97 |
| [`core/decisions/ADR-004-snooze-as-routing-library.md`](core/decisions/ADR-004-snooze-as-routing-library.md) | Snooze chosen as routing library | Accepted | ~214 | 0.97 |

**Critical decisions**:
- **Route Registry**: Enables automatic path parameter extraction (`:id` → `id` variable binding)
- **No Middleware**: Intentional simplification; use Snooze/Hunchentoot hooks for advanced needs
- **HTTP 422**: Semantic accuracy for validation errors vs malformed requests (400)
- **Snooze**: Small (~850 LoC), CLOS-based, integrates with Hunchentoot

---

## Features

**Status**: All 7 core features specified with high confidence (avg 0.95).

| Feature | Description | Status | Confidence |
|---------|-------------|--------|------------|
| [`routing`](features/routing/) | HTTP route macros with automatic path parameter extraction | stable | 0.95 |
| [`json_serialization`](features/json_serialization/) | Automatic JSON parsing and serialization | stable | 0.95 |
| [`validation`](features/validation/) | Request validation with error collection (HTTP 422) | stable | 0.97 |
| [`request_context`](features/request_context/) | Special variables (*request*, *body*, *db*) and path parameters | stable | 0.93 |
| [`response_helpers`](features/response_helpers/) | HTTP response constructors (ok, created, not-found, etc.) | stable | 0.95 |
| [`sqlite_integration`](features/sqlite_integration/) | SQLite database helpers (with-db, ensure-table, query) | stable | 0.93 |
| [`error_handling`](features/error_handling/) | Standardized JSON error responses | stable | 0.95 |

For detailed feature navigation, see [`features/INDEX.md`](features/INDEX.md).

### Feature Capabilities Summary
- Route definition (`api-get`, `api-post`, `api-put`, `api-delete`, `api-patch`)
- Request validation
- JSON serialization/deserialization
- SQLite integration
- Response helpers
- Error handling

**Next step**: Use `canon-specify` or `canon-genesis` to formalize features with contracts, scenarios, and properties.

---

## Dependencies

This Canon references these external technologies:

| Technology | Version | Purpose | Canon Coverage |
|------------|---------|---------|----------------|
| Snooze | Latest | HTTP routing | ADR-004 |
| Hunchentoot | Latest | HTTP server | ADR-004 |
| com.inuoe.jzon | Latest | JSON parsing | Vocabulary |
| cl-ppcre | Latest | Regex | Vocabulary |
| inquisitio | Latest | SQLite bindings | Vocabulary |
| Common Lisp | SBCL | Language | All |

---

## Architectural Principles

From vocabulary and ADRs:

1. **Thin veneer over proven libraries** - Minimal abstraction over Snooze, Hunchentoot, jzon
2. **Automatic everything** - JSON serialization, request body parsing, error responses
3. **5 macros, not 50** - Small, focused API surface
4. **No middleware** - Intentional simplification (ADR-002)
5. **Semantic HTTP status codes** - 422 for validation, 404 for not found, etc. (ADR-003)
6. **Automatic path parameters** - `:id` in URI → `id` variable in handler (ADR-001)

---

## Reading Paths

### For new contributors
1. Start: `core/foundation/vocabulary.md` - Learn the terminology
2. Then: `core/decisions/` - Understand key design decisions
3. Finally: Implementation (`../src/`) - See how it's built

### For users integrating quickAPI
1. Start: `../README.md` - User-facing documentation
2. Then: `core/decisions/ADR-004-*.md` - Understand Snooze dependency
3. Reference: `core/foundation/vocabulary.md` - Look up terms

### For debugging routing issues
1. Read: `core/decisions/ADR-001-route-registry-architecture.md`
2. Check: `src/core.lisp:13-117` (implementation)
3. Tests: `tests/route-registry-tests.lisp`

### For understanding validation
1. Read: `core/decisions/ADR-003-http-422-for-validation-errors.md`
2. Reference: `core/foundation/vocabulary.md` (Validation section)
3. Implementation: `src/validation.lisp`

---

## Maintenance

### Confidence Scores

Canon artifacts extracted via `canon-initiate` include confidence scores indicating reliability of extracted information:

- **0.92-0.97**: High confidence (code + tests + docs aligned)
- **0.80-0.91**: Medium confidence (some ambiguity)
- **< 0.80**: Low confidence (requires verification)

All current artifacts have confidence ≥ 0.92.

### Updating This Canon

When quickAPI evolves:

1. **New features**: Use `canon-specify` to add feature specifications
2. **Architecture changes**: Add new ADR in `core/decisions/`
3. **Terminology updates**: Update `core/foundation/vocabulary.md`
4. **This index**: Keep in sync with structure changes

### Status Markers

- **DRAFT**: Extracted but not yet reviewed/finalized
- **Accepted**: Reviewed and approved
- **Superseded**: Replaced by newer decision

---

## Related Artifacts

| Artifact | Location | Purpose |
|----------|----------|---------|
| **AGENT.md** | `../AGENT.md` | Instructions for contributing agents |
| **README.md** | `../README.md` | User-facing documentation |
| **Source code** | `../src/` | Implementation |
| **Tests** | `../tests/` | Test suite |
| **Examples** | `../examples/` | Usage examples |

---

## Notes

This Canon was initialized via `canon-initiate` with multi-source triangulation from:
- Source code (`src/`)
- Tests (`tests/`)
- Documentation (`README.md`, `docs/`)
- Examples (`examples/todo-api.lisp`)

**Extraction date**: 2026-01-24
**Extraction confidence**: 0.92-0.97 across all artifacts

For feature specifications (contracts, scenarios, properties), use `canon-specify` to evolve this Canon.
