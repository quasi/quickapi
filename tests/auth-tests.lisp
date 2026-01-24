;;;; ABOUTME: Tests for authentication functions - JWT, passwords, sessions, API keys

(in-package :quickapi/tests)

(5am:in-suite quickapi-tests)

;;; ============================================================================
;;; Password Hashing Tests
;;; ============================================================================

(5am:test hash-password-format
  "Test hash-password produces correct format"
  (let ((hash (qa:hash-password "secret123")))
    (5am:is (stringp hash))
    (5am:is (cl-ppcre:scan "^pbkdf2:sha256:\\d+:[A-Za-z0-9+/=]+:[A-Za-z0-9+/=]+$" hash))))

(5am:test hash-password-unique-salts
  "Test hash-password generates unique salts"
  (let ((hash1 (qa:hash-password "secret123"))
        (hash2 (qa:hash-password "secret123")))
    (5am:is (not (string= hash1 hash2)))))

(5am:test verify-password-correct
  "Test verify-password returns t for correct password"
  (let ((hash (qa:hash-password "mypassword")))
    (5am:is (eq t (qa:verify-password "mypassword" hash)))))

(5am:test verify-password-incorrect
  "Test verify-password returns nil for wrong password"
  (let ((hash (qa:hash-password "mypassword")))
    (5am:is (null (qa:verify-password "wrongpassword" hash)))))

(5am:test verify-password-invalid-hash
  "Test verify-password returns nil for invalid hash format"
  (5am:is (null (qa:verify-password "password" "invalid-hash")))
  (5am:is (null (qa:verify-password "password" "")))
  (5am:is (null (qa:verify-password "password" nil))))

;;; ============================================================================
;;; JWT Tests
;;; ============================================================================

(5am:test generate-jwt-requires-secret
  "Test generate-jwt errors when secret not set"
  (let ((qa:*jwt-secret* nil))
    (5am:signals error
      (qa:generate-jwt (make-hash-table :test 'equal)))))

(5am:test generate-jwt-produces-token
  "Test generate-jwt produces a valid token string"
  (let ((qa:*jwt-secret* "test-secret-key-at-least-32-chars"))
    (let ((claims (make-hash-table :test 'equal)))
      (setf (gethash "sub" claims) "user123")
      (let ((token (qa:generate-jwt claims)))
        (5am:is (stringp token))
        ;; JWT format: header.payload.signature
        (5am:is (= 2 (count #\. token)))))))

(5am:test verify-jwt-valid-token
  "Test verify-jwt decodes valid token"
  (let ((qa:*jwt-secret* "test-secret-key-at-least-32-chars"))
    (let ((claims (make-hash-table :test 'equal)))
      (setf (gethash "sub" claims) "user123")
      (setf (gethash "name" claims) "Test User")
      (let* ((token (qa:generate-jwt claims))
             (decoded (qa:verify-jwt token)))
        (5am:is (hash-table-p decoded))
        (5am:is (string= "user123" (gethash "sub" decoded)))
        (5am:is (string= "Test User" (gethash "name" decoded)))))))

(5am:test verify-jwt-returns-nil-no-secret
  "Test verify-jwt returns nil when secret not set"
  (let ((qa:*jwt-secret* nil))
    (5am:is (null (qa:verify-jwt "some.token.here")))))

(5am:test verify-jwt-returns-nil-invalid-token
  "Test verify-jwt returns nil for invalid token"
  (let ((qa:*jwt-secret* "test-secret-key-at-least-32-chars"))
    (5am:is (null (qa:verify-jwt "invalid-token")))
    (5am:is (null (qa:verify-jwt "")))))

(5am:test verify-jwt-returns-nil-wrong-secret
  "Test verify-jwt returns nil when verified with wrong secret"
  (let ((qa:*jwt-secret* "original-secret-key-32-chars-long"))
    (let ((claims (make-hash-table :test 'equal)))
      (setf (gethash "sub" claims) "user123")
      (let ((token (qa:generate-jwt claims)))
        ;; Try to verify with different secret
        (let ((qa:*jwt-secret* "different-secret-key-32-chars!!"))
          (5am:is (null (qa:verify-jwt token))))))))

(5am:test verify-jwt-returns-nil-expired
  "Test verify-jwt returns nil for expired token"
  (let ((qa:*jwt-secret* "test-secret-key-at-least-32-chars"))
    (let ((claims (make-hash-table :test 'equal)))
      (setf (gethash "sub" claims) "user123")
      ;; Create token that expired 1 hour ago
      (setf (gethash "exp" claims) (- (get-universal-time) 3600))
      (let ((token (qa:generate-jwt claims :expires-in 0)))
        ;; Manually create an expired token by setting exp in the past
        (5am:is (null (qa:verify-jwt token)))))))

(5am:test generate-jwt-with-plist
  "Test generate-jwt accepts plist as well as hash-table"
  (let ((qa:*jwt-secret* "test-secret-key-at-least-32-chars"))
    (let* ((token (qa:generate-jwt '(:sub "user123" :role "admin")))
           (decoded (qa:verify-jwt token)))
      (5am:is (hash-table-p decoded))
      (5am:is (string= "user123" (gethash "sub" decoded)))
      (5am:is (string= "admin" (gethash "role" decoded))))))

;;; ============================================================================
;;; Session Helper Tests
;;; ============================================================================

(5am:test session-get-returns-nil-no-request
  "Test session-get returns nil when no request"
  (let ((qa:*request* nil))
    (5am:is (null (qa:session-get :user-id)))))

(5am:test session-get-returns-nil-no-session
  "Test session-get returns nil when no session in request"
  (let ((qa:*request* '(:path-info "/")))
    (5am:is (null (qa:session-get :user-id)))))

(5am:test session-get-and-set
  "Test session-get and setf work with session"
  (let* ((session (make-hash-table :test 'equal))
         (qa:*request* (list :lack.session session)))
    ;; Set a value
    (setf (qa:session-get :user-id) 123)
    ;; Get it back
    (5am:is (= 123 (qa:session-get :user-id)))))

;;; ============================================================================
;;; Auth Checker Tests
;;; ============================================================================

(5am:test check-jwt-auth-extracts-bearer
  "Test check-jwt-auth extracts bearer token"
  (let ((qa:*jwt-secret* "test-secret-key-at-least-32-chars"))
    (let ((claims (make-hash-table :test 'equal)))
      (setf (gethash "sub" claims) "user123")
      (let* ((token (qa:generate-jwt claims))
             (env (list :http-authorization (format nil "Bearer ~a" token)))
             (result (qa::check-jwt-auth env)))
        (5am:is (hash-table-p result))
        (5am:is (string= "user123" (gethash "sub" result)))))))

(5am:test check-jwt-auth-returns-nil-no-header
  "Test check-jwt-auth returns nil when no auth header"
  (let ((qa:*jwt-secret* "test-secret-key-at-least-32-chars")
        (env '(:path-info "/")))
    (5am:is (null (qa::check-jwt-auth env)))))

(5am:test check-jwt-auth-returns-nil-wrong-scheme
  "Test check-jwt-auth returns nil for non-Bearer scheme"
  (let ((qa:*jwt-secret* "test-secret-key-at-least-32-chars")
        (env '(:http-authorization "Basic dXNlcjpwYXNz")))
    (5am:is (null (qa::check-jwt-auth env)))))

(5am:test check-session-auth-loads-user
  "Test check-session-auth uses *session-user-loader*"
  (let* ((session (make-hash-table :test 'equal))
         (env (list :lack.session session)))
    (setf (gethash :user-id session) 42)
    (let ((qa:*session-user-loader*
            (lambda (id)
              (when (= id 42)
                (let ((h (make-hash-table :test 'equal)))
                  (setf (gethash "id" h) 42)
                  (setf (gethash "name" h) "Test User")
                  h)))))
      (let ((result (qa::check-session-auth env)))
        (5am:is (hash-table-p result))
        (5am:is (= 42 (gethash "id" result)))
        (5am:is (string= "Test User" (gethash "name" result)))))))

(5am:test check-session-auth-returns-nil-no-loader
  "Test check-session-auth returns nil when no loader set"
  (let* ((session (make-hash-table :test 'equal))
         (env (list :lack.session session))
         (qa:*session-user-loader* nil))
    (setf (gethash :user-id session) 42)
    (5am:is (null (qa::check-session-auth env)))))

(5am:test check-api-key-auth-from-header
  "Test check-api-key-auth extracts key from X-API-Key header"
  (let ((qa:*api-key-validator*
          (lambda (key)
            (when (string= key "valid-key-123")
              (let ((h (make-hash-table :test 'equal)))
                (setf (gethash "id" h) 1)
                h)))))
    (let ((env '(:http-x-api-key "valid-key-123")))
      (let ((result (qa::check-api-key-auth env)))
        (5am:is (hash-table-p result))
        (5am:is (= 1 (gethash "id" result)))))))

(5am:test check-api-key-auth-from-query
  "Test check-api-key-auth extracts key from query param"
  (let ((qa:*api-key-validator*
          (lambda (key)
            (when (string= key "query-key-456")
              (let ((h (make-hash-table :test 'equal)))
                (setf (gethash "id" h) 2)
                h)))))
    (let ((env '(:query-string "api_key=query-key-456")))
      (let ((result (qa::check-api-key-auth env)))
        (5am:is (hash-table-p result))
        (5am:is (= 2 (gethash "id" result)))))))

(5am:test check-api-key-auth-returns-nil-invalid
  "Test check-api-key-auth returns nil for invalid key"
  (let ((qa:*api-key-validator*
          (lambda (key)
            (declare (ignore key))
            nil)))
    (let ((env '(:http-x-api-key "invalid-key")))
      (5am:is (null (qa::check-api-key-auth env))))))

;;; ============================================================================
;;; perform-auth-check Tests
;;; ============================================================================

(5am:test perform-auth-check-jwt-success
  "Test perform-auth-check with valid JWT"
  (let ((qa:*jwt-secret* "test-secret-key-at-least-32-chars"))
    (let ((claims (make-hash-table :test 'equal)))
      (setf (gethash "sub" claims) "user123")
      (let* ((token (qa:generate-jwt claims))
             (env (list :http-authorization (format nil "Bearer ~a" token))))
        (multiple-value-bind (success user error)
            (qa::perform-auth-check :jwt env)
          (5am:is (eq t success))
          (5am:is (hash-table-p user))
          (5am:is (null error)))))))

(5am:test perform-auth-check-jwt-failure
  "Test perform-auth-check with invalid JWT"
  (let ((qa:*jwt-secret* "test-secret-key-at-least-32-chars")
        (env '(:http-authorization "Bearer invalid.token.here")))
    (multiple-value-bind (success user error)
        (qa::perform-auth-check :jwt env)
      (5am:is (null success))
      (5am:is (null user))
      (5am:is (stringp error)))))

(5am:test perform-auth-check-unknown-type
  "Test perform-auth-check with unknown auth type"
  (multiple-value-bind (success user error)
      (qa::perform-auth-check :unknown '())
    (5am:is (null success))
    (5am:is (null user))
    (5am:is (stringp error))))

;;; ============================================================================
;;; Route Auth Integration Tests
;;; ============================================================================

(5am:test extract-auth-option-present
  "Test extract-auth-option extracts :auth value"
  (5am:is (eq :jwt (qa::extract-auth-option '(:auth :jwt))))
  (5am:is (eq :session (qa::extract-auth-option '(:auth :session))))
  (5am:is (eq :api-key (qa::extract-auth-option '(:auth :api-key)))))

(5am:test extract-auth-option-absent
  "Test extract-auth-option returns nil when not present"
  (5am:is (null (qa::extract-auth-option '())))
  (5am:is (null (qa::extract-auth-option nil))))
