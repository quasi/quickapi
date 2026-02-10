---
type: contract
name: declarative-validation
version: 1.0.0
feature: validation
---

# Declarative Validation

## Purpose

Validate request data using declarative checks that automatically return structured error responses on failure.

## Interface

```lisp
(validate data
  (require-fields "field1" "field2" ...)
  (require-type "field" 'type)
  (require-length "field" :min N :max M)
  (require-range "field" :min N :max M)
  (require-pattern "field" "regex"))
```

## Requirements

**R1**: Validation errors MUST return HTTP 422 with structured error details.

**R2**: All validation checks MUST execute (fail-late, not fail-fast).

**R3**: Error details MUST include field name and error type for each failure.

## Guarantees

**G1**: If validation passes, execution continues normally.

**G2**: If validation fails, signals `validation-error` condition.

**G3**: Error response format matches specification in error-response-format.md.
