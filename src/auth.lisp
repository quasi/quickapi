;;;; ABOUTME: Authentication utilities for quickapi - JWT, sessions, API keys, password hashing

(in-package :quickapi)

;;; ============================================================================
;;; Special Variables
;;; ============================================================================

(defvar *current-user* nil
  "The authenticated user for the current request.
   For JWT: contains the decoded claims hash-table.
   For session: contains the user loaded via *session-user-loader*.
   For API key: contains the result from *api-key-validator*.")

(defvar *jwt-secret* nil
  "Secret key for JWT signing. Must be set before using JWT auth.
   Should be at least 32 characters for HS256.")

(defvar *jwt-algorithm* :hs256
  "Algorithm for JWT signing. Currently only :hs256 is supported.")

(defvar *session-user-loader* nil
  "Function to load user from session user-id.
   Takes a user-id argument, returns user data or nil.
   Example: (setf *session-user-loader* #'find-user)")

(defvar *api-key-validator* nil
  "Function to validate an API key.
   Takes the API key string, returns user data or nil.
   Example: (setf *api-key-validator* (lambda (key) (find-api-key-owner key)))")

;;; ============================================================================
;;; Password Hashing (PBKDF2-SHA256 via Ironclad)
;;; ============================================================================

(defvar *pbkdf2-iterations* 100000
  "Number of PBKDF2 iterations.
   Production: 100000 (OWASP 2023 recommendation for SHA256)
   Development: 1000 (fast enough for testing)
   Automatically reduced in debug mode.")

(defconstant +pbkdf2-iterations-production+ 100000
  "Production iteration count. OWASP 2023 recommends 100000 for SHA256.")

(defconstant +pbkdf2-iterations-debug+ 1000
  "Debug iteration count. Fast for testing, NOT secure for production.")

(defconstant +salt-length+ 16
  "Length of random salt in bytes.")

(defconstant +hash-length+ 32
  "Length of derived key in bytes.")

(defun get-pbkdf2-iterations ()
  "Get the current PBKDF2 iteration count.
   Uses debug iterations if *debug-mode* is true."
  (if (and (boundp '*debug-mode*) *debug-mode*)
      +pbkdf2-iterations-debug+
      *pbkdf2-iterations*))

(defun hash-password (password)
  "Hash a password using PBKDF2-SHA256 via Ironclad's convenience function.
   Returns a string in format: pbkdf2:sha256:iterations:salt-base64:hash-base64
   Note: Uses fewer iterations in debug mode for faster testing."
  (let* ((iterations (get-pbkdf2-iterations))
         (password-bytes (ironclad:ascii-string-to-byte-array password)))
    (multiple-value-bind (hash salt)
        (ironclad:pbkdf2-hash-password password-bytes
                                        :digest :sha256
                                        :iterations iterations)
      (format nil "pbkdf2:sha256:~a:~a:~a"
              iterations
              (cl-base64:usb8-array-to-base64-string salt)
              (cl-base64:usb8-array-to-base64-string hash)))))

(defun verify-password (password stored-hash)
  "Verify a password against a stored hash.
   Returns T if the password matches, NIL otherwise."
  (handler-case
      (let* ((parts (cl-ppcre:split ":" stored-hash))
             (iterations (parse-integer (third parts)))
             (salt (cl-base64:base64-string-to-usb8-array (fourth parts)))
             (expected-hash (cl-base64:base64-string-to-usb8-array (fifth parts)))
             (password-bytes (ironclad:ascii-string-to-byte-array password)))
        (multiple-value-bind (computed-hash _salt)
            (ironclad:pbkdf2-hash-password password-bytes
                                            :salt salt
                                            :digest :sha256
                                            :iterations iterations)
          (declare (ignore _salt))
          (ironclad:constant-time-equal computed-hash expected-hash)))
    (error () nil)))

;;; ============================================================================
;;; JWT Functions (via jose library)
;;; ============================================================================

(defun hash-to-alist (hash)
  "Convert a hash-table to an alist with string keys."
  (let ((result nil))
    (maphash (lambda (k v) (push (cons k v) result)) hash)
    (nreverse result)))

(defun alist-to-hash (alist)
  "Convert an alist to a hash-table with string keys."
  (let ((hash (make-hash-table :test 'equal)))
    (dolist (pair alist)
      (setf (gethash (car pair) hash) (cdr pair)))
    hash))

(defun generate-jwt (claims &key (expires-in 3600))
  "Generate a JWT token from CLAIMS hash-table or plist.
   EXPIRES-IN is seconds until expiration (default 1 hour).
   Returns the token string."
  (unless *jwt-secret*
    (error "JWT secret not configured. Set *jwt-secret* before generating tokens."))
  (let ((claims-hash (etypecase claims
                       (hash-table claims)
                       (list (plist-to-hash claims)))))
    ;; Add standard claims if not present
    (let ((now (get-universal-time)))
      (unless (gethash "iat" claims-hash)
        (setf (gethash "iat" claims-hash) now))
      (unless (gethash "exp" claims-hash)
        (setf (gethash "exp" claims-hash) (+ now expires-in))))
    ;; Convert to alist for jose (which uses cl-json internally)
    (jose:encode *jwt-algorithm*
                 (babel:string-to-octets *jwt-secret* :encoding :utf-8)
                 (hash-to-alist claims-hash))))

(defun verify-jwt (token)
  "Verify and decode a JWT token.
   Returns the claims hash-table if valid, NIL otherwise.
   Checks signature and expiration."
  (unless *jwt-secret*
    (return-from verify-jwt nil))
  (handler-case
      (let* ((claims-alist (jose:decode *jwt-algorithm*
                                        (babel:string-to-octets *jwt-secret* :encoding :utf-8)
                                        token))
             (claims (alist-to-hash claims-alist)))
        ;; Check expiration
        (let ((exp (gethash "exp" claims)))
          (when (and exp (> (get-universal-time) exp))
            (return-from verify-jwt nil)))
        claims)
    (error () nil)))

(defun plist-to-hash (plist)
  "Convert a plist to a hash-table with string keys."
  (let ((hash (make-hash-table :test 'equal)))
    (loop for (key value) on plist by #'cddr
          do (setf (gethash (string-downcase (string key)) hash) value))
    hash))

;;; ============================================================================
;;; Session Helpers
;;; ============================================================================

(defun session-get (key)
  "Get a value from the current session.
   Requires :session middleware to be active."
  (when *request*
    (let ((session (getf *request* :lack.session)))
      (when session
        (gethash key session)))))

(defun (setf session-get) (value key)
  "Set a value in the current session.
   Requires :session middleware to be active."
  (when *request*
    (let ((session (getf *request* :lack.session)))
      (when session
        (setf (gethash key session) value))))
  value)

;;; ============================================================================
;;; Auth Checkers (used by route dispatch)
;;; ============================================================================

(defun extract-bearer-token (env)
  "Extract Bearer token from Authorization header.
   Handles both Lack/Clack style (headers in :headers hash-table)
   and direct :http-authorization style."
  (let* ((headers (getf env :headers))
         (auth-header (or (when (hash-table-p headers)
                            (gethash "authorization" headers))
                          (getf env :http-authorization))))
    (when (and auth-header
               (> (length auth-header) 7)
               (string-equal "Bearer " (subseq auth-header 0 7)))
      (subseq auth-header 7))))

(defun check-jwt-auth (env)
  "Check JWT authentication from Authorization header.
   Returns claims hash-table or NIL."
  (let ((token (extract-bearer-token env)))
    (when token
      (verify-jwt token))))

(defun check-session-auth (env)
  "Check session authentication.
   Returns user loaded via *session-user-loader* or NIL."
  (let ((session (getf env :lack.session)))
    (when session
      (let ((user-id (gethash :user-id session)))
        (when (and user-id *session-user-loader*)
          (funcall *session-user-loader* user-id))))))

(defun extract-api-key (env)
  "Extract API key from X-API-Key header or api_key query param."
  (or (getf env :http-x-api-key)
      (let ((query (getf env :query-string)))
        (when query
          (let ((match (cl-ppcre:scan-to-strings "(?:^|&)api_key=([^&]*)" query)))
            (when match
              (cl-ppcre:register-groups-bind (key)
                  ("(?:^|&)api_key=([^&]*)" query)
                key)))))))

(defun check-api-key-auth (env)
  "Check API key authentication.
   Returns result from *api-key-validator* or NIL."
  (let ((api-key (extract-api-key env)))
    (when (and api-key *api-key-validator*)
      (funcall *api-key-validator* api-key))))

(defun perform-auth-check (auth-type env)
  "Perform authentication check based on AUTH-TYPE.
   Returns (values success-p user-or-nil error-message-or-nil)."
  (case auth-type
    (:jwt
     (let ((claims (check-jwt-auth env)))
       (if claims
           (values t claims nil)
           (values nil nil "Invalid or expired token"))))
    (:session
     (let ((user (check-session-auth env)))
       (if user
           (values t user nil)
           (values nil nil "Session required"))))
    (:api-key
     (let ((user (check-api-key-auth env)))
       (if user
           (values t user nil)
           (values nil nil "Invalid API key"))))
    (t
     (values nil nil (format nil "Unknown auth type: ~a" auth-type)))))
