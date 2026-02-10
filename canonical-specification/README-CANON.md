# Canon for quickAPI

This directory contains the **canonical specification** for quickAPI — the authoritative source of truth for what the system is, how it works, and why design decisions were made.

## What is Canon?

Canon is a specification-first development system that maintains a structured, machine-readable specification alongside your code. It ensures your documentation stays synchronized with reality and captures the **intent** behind implementation choices.

## Structure

```
canonical-specification/
├── canon.yaml              # Manifest: system metadata, features, dependencies
├── README-CANON.md         # This file
│
├── foundation/
│   ├── vocabulary.yaml     # Domain terminology and definitions
│   └── ontology.yaml       # Conceptual relationships and hierarchy
│
├── core/
│   ├── contracts/          # Feature contracts (requirements, guarantees)
│   ├── scenarios/          # Usage scenarios and examples
│   └── decisions/          # Architecture Decision Records (ADRs)
│
└── verification/
    └── verification-strategy.yaml  # How to verify implementation correctness
```

## Features

quickAPI's canonical specification covers these features:

- **routing** — Path-based route registration with automatic parameter extraction
- **json_serialization** — Automatic JSON encoding/decoding for requests and responses
- **validation** — Declarative request validation with structured error responses
- **request_context** — Special variables (*body*, *request*) bound during request handling
- **response_helpers** — Semantic functions (ok, created, not-found) for HTTP responses
- **sqlite_integration** — Thin wrapper over sqlite for database operations
- **error_handling** — Structured condition system mapping to HTTP error responses

## Philosophy

quickAPI follows these design principles:

1. **Thin veneer over proven libraries** — Build on snooze, hunchentoot, jzon rather than reinventing
2. **5 route macros, not 50 abstractions** — Minimal API surface, maximum clarity
3. **Automatic JSON, automatic validation, automatic errors** — Handle infrastructure concerns transparently
4. **Focus on business logic, not infrastructure** — Let developers concentrate on their domain

## Using This Specification

### For Developers

- Read `foundation/vocabulary.yaml` to understand domain terminology
- Check `core/decisions/ADR-*.md` to understand why things work the way they do
- Review feature contracts in `core/contracts/` to see requirements and guarantees

### For Contributors

- Add new features by creating contracts and scenarios
- Document design decisions in `core/decisions/`
- Keep vocabulary and ontology updated as concepts evolve

### For AI Agents

This specification is optimized for LLM consumption:
- Structured YAML for machine parsing
- Explicit contracts with preconditions and postconditions
- Rich context in ADRs explaining rationale

## Verification

The specification includes a verification strategy (`verification/verification-strategy.yaml`) that defines:
- What tests are required for each feature
- Which integration points need smoke tests
- How to validate that code matches specification

## Maintenance

The Canon is kept synchronized with code via:
- **CCC (Canon Completion Contract)** — Automated checks that specification matches reality
- **Drift detection** — Identifies when code changes without corresponding spec updates
- **Regeneration** — Extracts updated specifications from implementation when needed

## Questions?

See the [main README](../README.md) for user-facing documentation, or explore `foundation/vocabulary.yaml` for a glossary of terms used throughout this specification.
