# quickAPI Canon Specification

**Status**: DRAFT (extracted via canon-initiate, high-priority items resolved)
**Version**: 0.1.0 (tracks quickAPI version)
**Canon Version**: 0.2.0
**Initiated**: 2026-01-24
**Last Updated**: 2026-01-25
**Overall Confidence**: 0.92 (High)
**Feature Coverage**: 100% (7/7 features specified)

---

## What is This?

This Canon specification was **extracted from the working quickAPI implementation** using multi-source triangulation. It represents the **actual behavior, contracts, and design decisions** present in the code, verified against documentation, tests, and examples.

Unlike traditional documentation that can drift, this Canon was mechanically extracted and cross-validated across:
- Source code implementation
- Documentation claims
- Test assertions
- Working examples
- Git commit history

---

## Extraction Method

**Seven-Pass Triangulation** (canon-initiate v0.2):

1. **Pass 0: Documentation Survey** - Establish doc claims before examining code
2. **Pass 1: Structural Discovery** - Identify features from CODE, compare with docs
3. **Pass 2: Contract Extraction** - Extract interface specs from implementation
4. **Pass 3: Behavioral Capture** - Extract scenarios from tests and examples
5. **Pass 4: Property Inference** - Infer invariants from code and assertions
6. **Pass 5: Rationale Recovery** - Recover design decisions from history/comments
7. **Pass 6: Reconciliation** - Final triangulation and confidence scoring

**Result**: 7 convergent features, 0 conflicts, 92% confidence

See [.canon-initiation/triangulation-report.md](../.canon-initiation/triangulation-report.md) for full details.

---

## Structure

```
canon/
├── canon.yaml                          # Manifest
├── core/
│   ├── foundation/
│   │   └── vocabulary.md               # Domain vocabulary [DRAFT]
│   └── decisions/                      # Architecture Decision Records
└── features/
    ├── routing/feature.yaml            # Routing specification ✓
    ├── validation/feature.yaml         # Validation specification ✓
    ├── json_serialization/feature.yaml # JSON handling specification ✓
    ├── request_context/feature.yaml    # Request context ✓ (added 2026-01-25)
    ├── response_helpers/feature.yaml   # Response helpers ✓ (added 2026-01-25)
    ├── sqlite_integration/feature.yaml # SQLite integration ✓ (added 2026-01-25)
    └── error_handling/feature.yaml     # Error handling ✓ (added 2026-01-25)
```

---

## Core Features

All features extracted with **high confidence** from working implementation:

### 1. Routing (0.95)
Five HTTP route macros with automatic path parameter extraction.
- `api-get`, `api-post`, `api-put`, `api-patch`, `api-delete`
- URI patterns: `/users/:id` style
- Automatic variable binding for path parameters

### 2. Validation (0.97)
Declarative validation framework with error collection.
- `validate` macro
- 5 validators: `require-fields`, `require-type`, `require-length`, `require-range`, `require-pattern`
- HTTP 422 responses with all errors

### 3. JSON Serialization (0.92)
Automatic JSON for requests and responses.
- Response serialization (hash tables → JSON)
- Request parsing (JSON → hash tables for POST/PUT/PATCH)
- Content-Type: application/json

### 4. Request Context (0.90)
Special variables for request handling.
- `*request*`, `*body*`, `*db*`
- Automatic body parsing
- Path parameters as variables

### 5. Response Helpers (0.95)
Semantic HTTP response functions.
- Success: `ok`, `created`, `no-content`
- Errors: `bad-request`, `not-found`, `error-response`

### 6. SQLite Integration (0.93)
Database helpers for CRUD operations.
- `with-db`, `ensure-table`, `last-insert-id`
- `row-to-hash`, `rows-to-json`, `with-transaction`

### 7. Error Handling (0.90)
Standardized error responses.
- Format: `{error, message, details}`
- HTTP conditions for control flow

---

## Design Philosophy

Extracted from code, docs, and commit history:

1. **"Thin veneer over proven libraries"** (0.97 confidence)
   - Minimal abstraction over Snooze, jzon, cl-sqlite
   - Easy to debug, less magic

2. **"5 macros, not 50 abstractions"** (0.95 confidence)
   - Small, focused API surface
   - Learn once, use everywhere

3. **"Automatic everything"** (0.95 confidence)
   - JSON serialization
   - Body parsing
   - Error responses

4. **"Focus on business logic, not infrastructure"** (0.93 confidence)
   - No configuration files
   - No middleware complexity

5. **"Small-to-medium projects"** (0.93 confidence)
   - Not for enterprise scale
   - Perfect for REST APIs in CL

---

## Triangulation Results

### Convergence Summary
```
Convergent Features:      7  (87.5%)  ████████████████
Code-Only Features:       3  (12.5%)  ██
Docs-Only Features:       1  (12.5%)  ██
Conflicts:                0  (0%)
Coverage Gaps:            4  (noted)
```

### Confidence Distribution
| Artifact Type | High (≥0.9) | Medium (0.7-0.9) | Low (<0.7) |
|---------------|-------------|------------------|------------|
| Contracts | 28 | 2 | 0 |
| Behaviors | 18 | 3 | 0 |
| Properties | 42 | 8 | 0 |
| Features | 6 | 2 | 0 |

**Overall: 0.92** (High Confidence)

---

## Observations

### Convergent (High Confidence)
- All 5 route macros fully implemented and documented
- Validation framework complete and well-tested (20+ tests)
- Path parameter auto-extraction working as documented
- SQLite integration complete with examples

### Code-Only (Implementation Details)
- Route registry infrastructure (internal, not user-facing)
- Custom Snooze resource name function (integration glue)

### Docs-Only (Needs Verification)
- ✅ ~~`path-param`, `query-param`, `header` functions exported but not implemented~~ **RESOLVED**
  - Resolution: Removed stale exports (commit c2c8e31, 2026-01-25)
  - Rationale: Superseded by automatic path parameter binding

### Coverage Gaps (Testing)
- ✅ ~~Route matching edge cases need unit tests~~ **RESOLVED**
  - Resolution: Added 20 unit tests (tests/route-registry-tests.lisp, commit c2c8e31, 2026-01-25)
  - Coverage: 168 tests passing (100% pass rate)
- JSON parsing error handling (medium priority) - PENDING
- Server lifecycle tests (medium priority) - PENDING
- Concurrency behavior (low priority) - PENDING

---

## Next Steps

### Completed ✅
1. ✅ Verified and removed stale `path-param`/`query-param`/`header` exports (2026-01-25)
2. ✅ Added 20 unit tests for route registry functions (2026-01-25)
3. ✅ Completed all 7 feature specifications (2026-01-25)

### Immediate (High Priority)
4. Add Architecture Decision Records (ADRs) for key design decisions

### Short-Term (Medium Priority)
5. Document error response format schema
6. Add JSON parsing edge case tests
7. Update docs to reflect route registry architecture

### Long-Term (Low Priority)
8. Consider making lambda-list optional in route macros
9. Document concurrency model
10. Expand examples

---

## How to Use This Canon

### For Developers
- **Foundation Vocabulary**: Start with `core/foundation/vocabulary.md` for domain terms
- **Feature Specs**: Read `features/*/feature.yaml` for detailed specifications
- **Contracts**: See `.canon-initiation/contracts.yaml` for all API contracts
- **Properties**: See `.canon-initiation/properties.yaml` for invariants

### For Documentation Writers
- **High-confidence specs** (≥0.9) are safe to document
- **Medium-confidence specs** (0.7-0.9) should be verified
- Check `observations.yaml` for known gaps and issues

### For Testers
- **Coverage gaps** listed in `.canon-initiation/observations.yaml`
- **Properties** in `properties.yaml` suggest test cases
- **Behaviors** in `behaviors.yaml` show existing test coverage

### For Code Reviewers
- **Design philosophy** guides architectural decisions
- **Observations** highlight areas needing attention
- **Rationale** explains why things are designed this way

---

## Confidence Interpretation

| Score | Meaning | Action |
|-------|---------|--------|
| ≥ 0.9 | Very High | Accept as specification |
| 0.7-0.9 | Good | Light verification recommended |
| 0.5-0.7 | Moderate | Verify before relying on |
| < 0.5 | Low | Needs attention |

---

## Initiation Artifacts

Detailed extraction data available in `.canon-initiation/`:

- **triangulation-report.md** - Executive summary and findings
- **extraction-log.md** - Detailed pass-by-pass log
- **observations.yaml** - All 24 observations with priority/action
- **state.yaml** - Extraction state and statistics
- **docs-survey.yaml** - Documentation claims (Pass 0)
- **structural-discovery.yaml** - Code structure analysis (Pass 1)
- **contracts.yaml** - 30+ API contracts (Pass 2)
- **behaviors.yaml** - 30+ scenarios from tests/examples (Pass 3)
- **properties.yaml** - 50+ invariants and properties (Pass 4)
- **rationale.yaml** - Design decisions and philosophy (Pass 5)

---

## Status

**Canon Initiation**: ✅ Complete (2026-01-24)

**Ready for**:
- Feature specification completion
- Property-based testing
- Documentation generation
- Implementation verification

**Not Ready for**:
- External implementation (this is the source of truth)
- Formal verification (properties identified but not formally proven)

---

*This Canon extracted via canon-initiate v0.2 multi-source triangulation.*
*Confidence: 0.92 (High) - Ready to proceed with specification development.*
