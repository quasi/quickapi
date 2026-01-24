;;;; ABOUTME: Tests for route registry and URI pattern matching

(in-package :quickapi/tests)

(5am:in-suite quickapi-tests)

;;; PARSE-URI-PATTERN Tests

(5am:test parse-uri-pattern-empty
  "Test parse-uri-pattern with root path"
  (let ((result (qa::parse-uri-pattern "/")))
    (5am:is (null result))))

(5am:test parse-uri-pattern-simple-literal
  "Test parse-uri-pattern with simple literal path"
  (let ((result (qa::parse-uri-pattern "/users")))
    (5am:is (= 1 (length result)))
    (5am:is (eq :literal (car (first result))))
    (5am:is (string= "users" (cdr (first result))))))

(5am:test parse-uri-pattern-multiple-literals
  "Test parse-uri-pattern with multiple literal segments"
  (let ((result (qa::parse-uri-pattern "/api/v1/users")))
    (5am:is (= 3 (length result)))
    (5am:is (eq :literal (car (first result))))
    (5am:is (string= "api" (cdr (first result))))
    (5am:is (eq :literal (car (second result))))
    (5am:is (string= "v1" (cdr (second result))))
    (5am:is (eq :literal (car (third result))))
    (5am:is (string= "users" (cdr (third result))))))

(5am:test parse-uri-pattern-single-param
  "Test parse-uri-pattern with single path parameter"
  (let ((result (qa::parse-uri-pattern "/users/:id")))
    (5am:is (= 2 (length result)))
    (5am:is (eq :literal (car (first result))))
    (5am:is (string= "users" (cdr (first result))))
    (5am:is (eq :param (car (second result))))
    (5am:is (eq 'qa::id (cdr (second result))))))

(5am:test parse-uri-pattern-multiple-params
  "Test parse-uri-pattern with multiple path parameters"
  (let ((result (qa::parse-uri-pattern "/users/:user-id/posts/:post-id")))
    (5am:is (= 4 (length result)))
    (5am:is (eq :literal (car (first result))))
    (5am:is (string= "users" (cdr (first result))))
    (5am:is (eq :param (car (second result))))
    (5am:is (eq 'qa::user-id (cdr (second result))))
    (5am:is (eq :literal (car (third result))))
    (5am:is (string= "posts" (cdr (third result))))
    (5am:is (eq :param (car (fourth result))))
    (5am:is (eq 'qa::post-id (cdr (fourth result))))))

(5am:test parse-uri-pattern-param-symbol-case
  "Test parse-uri-pattern interns param symbols in QUICKAPI package"
  (let ((result (qa::parse-uri-pattern "/items/:ItemId")))
    (5am:is (= 2 (length result)))
    (5am:is (eq :param (car (second result))))
    ;; Should be uppercased and interned in QUICKAPI
    (5am:is (eq 'qa::itemid (cdr (second result))))
    (5am:is (eq (symbol-package (cdr (second result)))
                (find-package :quickapi)))))

(5am:test parse-uri-pattern-trailing-slash
  "Test parse-uri-pattern handles trailing slash"
  (let ((result1 (qa::parse-uri-pattern "/users"))
        (result2 (qa::parse-uri-pattern "/users/")))
    ;; Both should produce same result (trailing slash creates empty segment)
    (5am:is (= (length result1) (length result2)))))

;;; MATCH-URI-TO-ROUTE Tests

(5am:test match-uri-exact-literal
  "Test match-uri-to-route with exact literal match"
  ;; Setup: register a route
  (clrhash qa::*route-registry*)
  (qa::register-route "/users" 'test-users-resource :get)
  ;; Test: match should succeed
  (multiple-value-bind (resource-name args)
      (qa::match-uri-to-route "/users" :get)
    (5am:is (eq 'test-users-resource resource-name))
    (5am:is (null args))))

(5am:test match-uri-no-match
  "Test match-uri-to-route returns nil when no match"
  (clrhash qa::*route-registry*)
  (qa::register-route "/users" 'test-users-resource :get)
  ;; Test: different path should not match
  (multiple-value-bind (resource-name args)
      (qa::match-uri-to-route "/posts" :get)
    (5am:is (null resource-name))
    (5am:is (null args))))

(5am:test match-uri-wrong-method
  "Test match-uri-to-route requires method to match"
  (clrhash qa::*route-registry*)
  (qa::register-route "/users" 'test-users-resource :get)
  ;; Test: same path but different method should not match
  (multiple-value-bind (resource-name args)
      (qa::match-uri-to-route "/users" :post)
    (5am:is (null resource-name))
    (5am:is (null args))))

(5am:test match-uri-single-param
  "Test match-uri-to-route extracts single path parameter"
  (clrhash qa::*route-registry*)
  (qa::register-route "/users/:id" 'test-user-resource :get)
  ;; Test: should match and extract id
  (multiple-value-bind (resource-name args)
      (qa::match-uri-to-route "/users/123" :get)
    (5am:is (eq 'test-user-resource resource-name))
    (5am:is (= 1 (length args)))
    (5am:is (eq 'qa::id (car (first args))))
    (5am:is (string= "123" (cdr (first args))))))

(5am:test match-uri-multiple-params
  "Test match-uri-to-route extracts multiple path parameters"
  (clrhash qa::*route-registry*)
  (qa::register-route "/users/:user-id/posts/:post-id" 'test-post-resource :get)
  ;; Test: should match and extract both params
  (multiple-value-bind (resource-name args)
      (qa::match-uri-to-route "/users/42/posts/99" :get)
    (5am:is (eq 'test-post-resource resource-name))
    (5am:is (= 2 (length args)))
    (5am:is (eq 'qa::user-id (car (first args))))
    (5am:is (string= "42" (cdr (first args))))
    (5am:is (eq 'qa::post-id (car (second args))))
    (5am:is (string= "99" (cdr (second args))))))

(5am:test match-uri-mixed-literal-param
  "Test match-uri-to-route with mixed literal and param segments"
  (clrhash qa::*route-registry*)
  (qa::register-route "/api/users/:id/profile" 'test-profile-resource :get)
  ;; Test: should match with param in middle
  (multiple-value-bind (resource-name args)
      (qa::match-uri-to-route "/api/users/alice/profile" :get)
    (5am:is (eq 'test-profile-resource resource-name))
    (5am:is (= 1 (length args)))
    (5am:is (eq 'qa::id (car (first args))))
    (5am:is (string= "alice" (cdr (first args))))))

(5am:test match-uri-segment-count-mismatch
  "Test match-uri-to-route fails when segment count differs"
  (clrhash qa::*route-registry*)
  (qa::register-route "/users/:id" 'test-user-resource :get)
  ;; Test: too many segments should not match
  (multiple-value-bind (resource-name args)
      (qa::match-uri-to-route "/users/123/extra" :get)
    (5am:is (null resource-name))
    (5am:is (null args)))
  ;; Test: too few segments should not match
  (multiple-value-bind (resource-name args)
      (qa::match-uri-to-route "/users" :get)
    (5am:is (null resource-name))
    (5am:is (null args))))

(5am:test match-uri-literal-mismatch
  "Test match-uri-to-route fails when literal segment doesn't match"
  (clrhash qa::*route-registry*)
  (qa::register-route "/users/:id" 'test-user-resource :get)
  ;; Test: different literal should not match
  (multiple-value-bind (resource-name args)
      (qa::match-uri-to-route "/posts/123" :get)
    (5am:is (null resource-name))
    (5am:is (null args))))

(5am:test match-uri-case-insensitive-literal
  "Test match-uri-to-route is case-insensitive for literal segments"
  (clrhash qa::*route-registry*)
  (qa::register-route "/users" 'test-users-resource :get)
  ;; Test: different case should still match
  (multiple-value-bind (resource-name args)
      (qa::match-uri-to-route "/USERS" :get)
    (5am:is (eq 'test-users-resource resource-name))
    (5am:is (null args))))

(5am:test match-uri-with-query-string
  "Test match-uri-to-route ignores query string in matching"
  (clrhash qa::*route-registry*)
  (qa::register-route "/users/:id" 'test-user-resource :get)
  ;; Test: query string should be ignored for route matching
  (multiple-value-bind (resource-name args)
      (qa::match-uri-to-route "/users/123?page=1&limit=10" :get)
    (5am:is (eq 'test-user-resource resource-name))
    (5am:is (= 1 (length args)))
    (5am:is (eq 'qa::id (car (first args))))
    (5am:is (string= "123" (cdr (first args))))))

(5am:test match-uri-multiple-routes
  "Test match-uri-to-route with multiple registered routes"
  (clrhash qa::*route-registry*)
  (qa::register-route "/users" 'list-users :get)
  (qa::register-route "/users/:id" 'get-user :get)
  (qa::register-route "/users" 'create-user :post)
  (qa::register-route "/posts/:id" 'get-post :get)

  ;; Test: each route should match correctly
  (multiple-value-bind (resource-name args)
      (qa::match-uri-to-route "/users" :get)
    (5am:is (eq 'list-users resource-name)))

  (multiple-value-bind (resource-name args)
      (qa::match-uri-to-route "/users/99" :get)
    (5am:is (eq 'get-user resource-name))
    (5am:is (string= "99" (cdr (first args)))))

  (multiple-value-bind (resource-name args)
      (qa::match-uri-to-route "/users" :post)
    (5am:is (eq 'create-user resource-name)))

  (multiple-value-bind (resource-name args)
      (qa::match-uri-to-route "/posts/42" :get)
    (5am:is (eq 'get-post resource-name))
    (5am:is (string= "42" (cdr (first args))))))

;;; REGISTER-ROUTE Tests

(5am:test register-route-stores-entry
  "Test register-route stores entry in registry"
  (clrhash qa::*route-registry*)
  (qa::register-route "/users/:id" 'test-resource :get)
  ;; Verify entry was stored
  (let ((entry (gethash (cons :get "/users/:id") qa::*route-registry*)))
    (5am:is (not (null entry)))
    (5am:is (string= "/users/:id" (qa::route-entry-pattern entry)))
    (5am:is (eq 'test-resource (qa::route-entry-resource-name entry)))
    (5am:is (eq :get (qa::route-entry-method entry)))
    (5am:is (= 2 (length (qa::route-entry-segments entry))))))

(5am:test register-route-overwrites-duplicate
  "Test register-route overwrites duplicate route"
  (clrhash qa::*route-registry*)
  (qa::register-route "/users" 'old-resource :get)
  (qa::register-route "/users" 'new-resource :get)
  ;; Should have the new resource
  (multiple-value-bind (resource-name args)
      (qa::match-uri-to-route "/users" :get)
    (5am:is (eq 'new-resource resource-name))))
