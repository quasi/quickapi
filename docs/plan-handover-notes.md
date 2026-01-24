# QuickAPI 2.0 - Handover Notes

**Purpose:** Track progress across context resets. Update this file after each work session.

---

## Quick Context for New Session

**Read these files first:**
1. `docs/quickapi-2.0-plan.md` - Full implementation plan
2. `docs/http-stack-research.md` - Research on HTTP libraries
3. `docs/snooze-native-architecture.md` - Why we're replacing Snooze
4. This file - Current status

**Project:** quickapi - Batteries-included JSON API framework for Common Lisp beginners

**Current Phase:** Phase 1 - defmodel macro

---

## Session Log

### Session: [DATE PENDING - Initial Planning]

**What was done:**
- Analyzed issues from real-world usage (step1.md, step2.md, step3.md)
- Researched Snooze architecture - found we're fighting its design
- Researched CL HTTP ecosystem (Clack, Lack, Ningle, easy-routes, Woo)
- Researched authentication options (jose, sessions, cl-bcrypt)
- Created comprehensive plan document
- Decided on 3-phase approach: defmodel → Clack/Lack → Auth

**Key decisions made:**
1. **Keep route registry** - It works, it's ours, no need to change
2. **Replace Snooze with Clack/Lack** - Better middleware, server flexibility
3. **Add defmodel** - Beginner convenience, optional (can still use raw SQL)
4. **Use jose for JWT** - fukamachi's library, well-maintained
5. **Phase order** - defmodel first (self-contained), then Clack/Lack, then auth

**Files created this session:**
- `docs/snooze-native-architecture.md`
- `docs/http-stack-research.md`
- `docs/quickapi-2.0-plan.md`
- `docs/plan-handover-notes.md` (this file)

**Next session should:**
1. Start Phase 2: Clack/Lack integration
2. Begin with `src/lack-app.lisp`
3. Follow tasks in plan document section 2.4

---

### Session: 2026-01-25 - Phase 1 Complete

**Starting point:** Phase 1 - defmodel implementation

**What was done:**
- Created `src/models.lisp` with defmodel macro
- Implemented type mapping (CL ↔ SQLite ↔ JSON)
- Generated table DDL with auto timestamps (created_at, updated_at)
- Generated all CRUD functions:
  - `create-<model>` - Insert with validation
  - `find-<model>` - Find by id
  - `find-<model>-by` - Find by any field
  - `list-<model>s` - List with pagination (limit, offset, order-by)
  - `update-<model>` - Update fields
  - `delete-<model>` - Delete by id
  - `count-<model>s` - Count records
- Added validation integration (required, max-length, min-length, pattern)
- Added `migrate-models` function for table creation
- Created comprehensive test suite in `tests/model-tests.lisp`

**Files created:**
- `src/models.lisp` - defmodel macro and CRUD generators
- `tests/model-tests.lisp` - 15 tests for model functionality

**Files modified:**
- `quickapi.asd` - Added models.lisp and model-tests.lisp
- `src/package.lisp` - Added exports for defmodel, migrate-models, *models*

**Tests status:** All 232 tests passing

**Example usage:**
```lisp
(defmodel todo
  ((title :type string :required t :max-length 200)
   (completed :type boolean :default nil)))

(with-db ("app.db")
  (migrate-models)
  (create-todo (hash "title" "Buy milk"))
  (list-todos :limit 10)
  (find-todo 1)
  (update-todo 1 (hash "completed" t))
  (delete-todo 1))
```

**Next session should:**
1. Start Phase 3: Authentication helpers
2. Add `jose` to dependencies
3. Create `src/auth.lisp`

---

### Session: 2026-01-25 - Phase 2 Complete

**Starting point:** Phase 2 - Clack/Lack integration

**What was done:**
- Replaced Snooze with Clack/Lack for HTTP handling
- Created `src/lack-app.lisp` with:
  - `http-error` condition (replaces snooze:http-condition)
  - `make-quickapi-app` - Lack application factory
  - Route dispatch from Lack env to route registry
  - Request body parsing from Lack env
  - Error handling for http-error, validation-error, and unexpected errors
  - Middleware builder with on-demand loading
- Updated `src/core.lisp`:
  - `start` uses `clack:clackup` with server selection (:hunchentoot/:woo)
  - `stop` uses `clack:stop`
  - Route handlers stored as closures in handler registry
  - `defapi` accepts `:middlewares` option
- Updated `src/response.lisp` to use new http-condition
- Updated `src/validation.lisp` to signal validation-error condition
- Updated all tests to use new condition types
- Fixed query string stripping in route matching

**Files created:**
- `src/lack-app.lisp` - Lack application and middleware

**Files modified:**
- `quickapi.asd` - Replaced snooze with clack/lack dependencies
- `src/core.lisp` - Clack-based start/stop, closure handlers
- `src/response.lisp` - Use http-condition function
- `src/validation.lisp` - Signal validation-error condition
- `src/package.lisp` - Export http-error, http-condition
- `tests/response-tests.lisp` - Use http-error condition
- `tests/validation-tests.lisp` - Use validation-error condition
- `tests/integration-tests.lisp` - Use new condition types

**Tests status:** All 232 tests passing

**Architecture change:**
```
Before:                          After:
quickapi route macros            quickapi route macros
        ↓                                ↓
    Snooze                       quickapi route registry
        ↓                                ↓
    Hunchentoot                  Lack middleware stack
                                         ↓
                                     Clack
                                         ↓
                                 Hunchentoot / Woo
```

---

### Session: 2026-01-25 - Phase 3 Complete

**Starting point:** Phase 3 - Authentication helpers

**What was done:**
- Created `src/auth.lisp` with complete auth implementation:
  - JWT functions: `generate-jwt`, `verify-jwt` using jose library
  - Password hashing: `hash-password`, `verify-password` using PBKDF2-SHA256 (ironclad)
  - Session helpers: `session-get`, `(setf session-get)` for Lack sessions
  - Auth checkers: `check-jwt-auth`, `check-session-auth`, `check-api-key-auth`
  - `perform-auth-check` for unified auth dispatch
- Updated `src/core.lisp`:
  - Added `extract-auth-option` to parse `:auth` from route lambda-list
  - Route macros now accept `:auth :jwt`, `:auth :session`, `:auth :api-key`
  - Handler registration includes auth-type
- Updated `src/lack-app.lisp`:
  - `handler-entry` struct stores function + auth-type
  - `dispatch-to-handler` checks auth before calling handler
  - `*current-user*` bound from auth result
- Added dependencies: jose, ironclad, cl-base64
- Created comprehensive test suite in `tests/auth-tests.lisp`

**Key design decisions:**
1. **Per-route auth** - Auth specified via `:auth` option in route macros
2. **PBKDF2-SHA256** - 100k iterations per OWASP 2023 recommendations
3. **JWT claims as hash-table** - `*current-user*` contains decoded claims
4. **Session user loader** - User provides `*session-user-loader*` function
5. **API key from header or query** - Checks X-API-Key header and api_key param

**Files created:**
- `src/auth.lisp` - All authentication utilities

**Files modified:**
- `quickapi.asd` - Added jose, ironclad, cl-base64 dependencies
- `src/package.lisp` - Exported auth symbols
- `src/core.lisp` - Auth option parsing in route macros
- `src/lack-app.lisp` - Auth checking in dispatch
- `tests/auth-tests.lisp` - 35 new auth tests

**Tests status:** All 284 tests passing

**Example usage:**
```lisp
;; Configure
(setf *jwt-secret* "your-secret-key-at-least-32-chars")

;; Password hashing
(hash-password "secret")  ; => "pbkdf2:sha256:100000:..."
(verify-password "secret" hash)  ; => T

;; JWT auth
(api-post "/login" ()
  (let ((user (find-user-by-email (gethash "email" *body*))))
    (when (verify-password (gethash "password" *body*)
                          (gethash "password_hash" user))
      (ok (list :token (generate-jwt (list :sub (gethash "id" user))))))))

(api-get "/profile" (:auth :jwt)
  (ok *current-user*))  ; *current-user* is JWT claims

;; Session auth
(setf *session-user-loader* #'find-user)
(api-get "/dashboard" (:auth :session)
  (ok *current-user*))  ; *current-user* loaded via *session-user-loader*

;; API key auth
(setf *api-key-validator* #'find-api-key-owner)
(api-get "/data" (:auth :api-key)
  (ok *current-user*))  ; *current-user* from validator result
```

**QuickAPI 2.0 is now feature-complete!**

**Next session should:**
1. Update README.md with new features (defmodel, middleware, auth)
2. Create comprehensive examples
3. Update AGENT.md with new architecture

---

## Current Status

| Phase | Status | Notes |
|-------|--------|-------|
| Phase 1: defmodel | **COMPLETE** | All CRUD + validation working |
| Phase 2: Clack/Lack | **COMPLETE** | Snooze replaced, 232 tests pass |
| Phase 3: Auth | **COMPLETE** | JWT, session, API key auth working, 284 tests pass |

---

## Phase 1: defmodel - Task Checklist ✅ COMPLETE

```
- [x] Define `defmodel` macro syntax
- [x] Implement type mapping (CL ↔ SQLite ↔ JSON)
- [x] Generate `ensure-table` DDL from model spec
- [x] Generate `create-<model>` function
- [x] Generate `find-<model>` function
- [x] Generate `find-<model>-by` function
- [x] Generate `list-<model>` function with pagination
- [x] Generate `update-<model>` function
- [x] Generate `delete-<model>` function
- [x] Generate `count-<model>` function
- [x] Add validation integration
- [x] Add `migrate-models` function
- [x] Write tests for all generated functions
- [ ] Document defmodel in README (deferred to end)
```

---

## Phase 2: Clack/Lack - Task Checklist ✅ COMPLETE

```
- [x] Add Clack, Lack to dependencies
- [x] Create `src/lack-app.lisp`
- [x] Modify `start` to use `clack:clackup`
- [x] Modify `stop` to use `clack:stop`
- [x] Update request context for Lack
- [x] Add `:middlewares` to `defapi`
- [x] Create `quickapi-handler` Lack application
- [x] Route dispatch from Lack to registry
- [x] Remove Snooze dependency
- [x] Test all existing functionality
- [x] Add server selection (:hunchentoot or :woo)
- [ ] Document middleware usage (deferred to end)
```

---

## Phase 3: Auth - Task Checklist ✅ COMPLETE

```
- [x] Add `jose` to dependencies
- [x] Create `src/auth.lisp`
- [x] Implement JWT functions
- [x] Implement session helpers
- [x] Implement API key auth
- [x] Add `:auth` to route macros
- [x] Bind `*current-user*`
- [x] Add password hashing helpers
- [x] Write auth tests
- [ ] Document auth patterns (deferred to end)
```

---

## Important Code Locations

| What | Where |
|------|-------|
| Route macros | `src/core.lisp` |
| Route registry | `src/core.lisp:42-99` |
| Validation | `src/validation.lisp` |
| SQLite helpers | `src/sqlite.lisp` |
| Response helpers | `src/response.lisp` |
| Package exports | `src/package.lisp` |
| Existing tests | `tests/` |

---

## Design Decisions Record

### DD-001: Keep Route Registry
**Decision:** Keep our custom route registry instead of using Snooze's CLOS dispatch
**Rationale:**
- It works and supports nested resources (`/users/:id/posts/:id`)
- Snooze's model doesn't handle this well
- We control it fully

### DD-002: Replace Snooze with Clack/Lack
**Decision:** Use Clack for HTTP abstraction, Lack for middleware
**Rationale:**
- Snooze has no middleware story
- Lack has CORS, sessions, auth, logging built-in
- Clack allows server swap (Hunchentoot → Woo)
- We only used Snooze for HTTP plumbing anyway

### DD-003: defmodel is Optional
**Decision:** defmodel is additive - raw SQL path always available
**Rationale:**
- Beginners get convenience
- Power users can drop to SQL
- No forced ORM

### DD-004: JWT via jose library
**Decision:** Use fukamachi/jose for JWT
**Rationale:**
- Same author as Clack, Lack, Woo, Mito - ecosystem consistency
- Full JOSE support (JWS, JWE, JWK)
- Active maintenance

---

## Blockers / Questions

_None currently. Ready to start Phase 1._

---

## Template for New Session Notes

Copy this to start a new session entry:

```markdown
### Session: [DATE]

**Starting point:** [Which phase/task]

**What was done:**
-

**Decisions made:**
-

**Problems encountered:**
-

**Files modified:**
-

**Tests status:** [All passing / X failing / Not run]

**Next session should:**
1.
```
