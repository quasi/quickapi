---
type: contract
name: automatic-json
version: 1.0.0
feature: json_serialization
---

# Automatic JSON Serialization

## Purpose

Automatically parse incoming JSON requests and serialize outgoing responses without explicit user code.

## Requirements

**R1**: POST/PUT/PATCH requests with `Content-Type: application/json` MUST be automatically parsed.

**R2**: Parsed JSON MUST be available in `*body*` special variable as a hash-table.

**R3**: Handler return values (hash-tables, plists, lists) MUST be automatically serialized to JSON.

**R4**: Malformed JSON MUST return 400 Bad Request with error details.

## Guarantees

**G1**: `*body*` contains parsed JSON or `nil` if no body present.

**G2**: All responses have `Content-Type: application/json; charset=utf-8`.

**G3**: Hash-table keys are preserved as strings (not symbols).
