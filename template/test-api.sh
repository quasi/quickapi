#!/bin/bash
# ABOUTME: Curl-based smoke tests for my-api

BASE_URL="${1:-http://127.0.0.1:8000}"
PASS=0
FAIL=0

check() {
    local desc="$1"
    local expected_status="$2"
    local url="$3"
    shift 3

    local status
    local body
    body=$(curl -s -w "\n%{http_code}" "$@" "$url")
    status=$(echo "$body" | tail -1)
    body=$(echo "$body" | sed '$d')

    if [ "$status" = "$expected_status" ]; then
        echo "PASS: $desc (HTTP $status)"
        PASS=$((PASS + 1))
    else
        echo "FAIL: $desc (expected $expected_status, got $status)"
        echo "  Body: $body"
        FAIL=$((FAIL + 1))
    fi
}

echo "=== Testing my-api at $BASE_URL ==="
echo

# Root endpoint
check "GET /" 200 "$BASE_URL/"

# Health endpoint
check "GET /health" 200 "$BASE_URL/health"

# Echo endpoint - with query params
check "GET /echo?name=world&lang=en" 200 "$BASE_URL/echo?name=world&lang=en"

# Echo endpoint - no params
check "GET /echo" 200 "$BASE_URL/echo"

# Not found
check "GET /nonexistent" 404 "$BASE_URL/nonexistent"

echo
echo "=== Results: $PASS passed, $FAIL failed ==="
[ "$FAIL" -eq 0 ] && exit 0 || exit 1
