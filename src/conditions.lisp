;;;; ABOUTME: Condition system for quickapi - proper Common Lisp error handling

(in-package :quickapi)

;;; =============================================================================
;;; Condition Hierarchy
;;; =============================================================================
;;;
;;; quickapi-error (base)
;;;   ├── http-error
;;;   │   ├── client-error (4xx)
;;;   │   │   ├── bad-request-error (400)
;;;   │   │   ├── unauthorized-error (401)
;;;   │   │   ├── forbidden-error (403)
;;;   │   │   ├── not-found-error (404)
;;;   │   │   ├── conflict-error (409)
;;;   │   │   └── validation-error (422)
;;;   │   └── server-error (5xx)
;;;   │       └── internal-server-error (500)
;;;   ├── database-error
;;;   │   ├── record-not-found (also inherits not-found-error)
;;;   │   ├── duplicate-record (also inherits conflict-error)
;;;   │   └── connection-error
;;;   └── authentication-error
;;;       └── invalid-credentials

;;; Base Condition

(define-condition quickapi-error (error)
  ((message :initarg :message
            :reader error-message
            :initform "An error occurred"
            :documentation "Human-readable error message"))
  (:documentation "Base condition for all quickapi errors."))

;;; HTTP Errors

(define-condition http-error (quickapi-error)
  ((status :initarg :status
           :reader http-error-status
           :documentation "HTTP status code (e.g., 404, 500)")
   (details :initarg :details
            :reader http-error-details
            :initform nil
            :documentation "Additional structured error details"))
  (:report (lambda (c stream)
             (format stream "HTTP ~a: ~a"
                     (http-error-status c)
                     (error-message c))))
  (:documentation "Base condition for HTTP-related errors."))

(define-condition client-error (http-error)
  ()
  (:documentation "Base for client errors (4xx status codes)."))

(define-condition server-error (http-error)
  ()
  (:documentation "Base for server errors (5xx status codes)."))

;;; Specific Client Errors

(define-condition bad-request-error (client-error)
  ()
  (:default-initargs :status 400)
  (:documentation "Bad request - client sent invalid data (400)."))

(define-condition unauthorized-error (client-error)
  ()
  (:default-initargs :status 401)
  (:documentation "Authentication required or failed (401)."))

(define-condition forbidden-error (client-error)
  ()
  (:default-initargs :status 403)
  (:documentation "Authenticated but not authorized (403)."))

(define-condition not-found-error (client-error)
  ()
  (:default-initargs
   :status 404
   :message "Resource not found")
  (:documentation "Resource not found (404)."))

(define-condition conflict-error (client-error)
  ()
  (:default-initargs :status 409)
  (:documentation "Resource conflict, e.g., duplicate entry (409)."))

(define-condition validation-error (client-error)
  ((field-errors :initarg :errors
                 :reader validation-errors
                 :documentation "List of field-specific validation errors"))
  (:default-initargs
   :status 422
   :message "Validation failed")
  (:report (lambda (c stream)
             (format stream "Validation failed: ~a error(s)"
                     (length (validation-errors c)))))
  (:documentation "Request validation failed (422)."))

;;; Server Errors

(define-condition internal-server-error (server-error)
  ((cause :initarg :cause
          :reader error-cause
          :initform nil
          :documentation "Underlying error that caused this"))
  (:default-initargs
   :status 500
   :message "Internal server error")
  (:documentation "Internal server error (500)."))

;;; Database Errors

(define-condition database-error (quickapi-error)
  ((operation :initarg :operation
              :reader db-error-operation
              :documentation "Database operation being performed (e.g., INSERT, SELECT)")
   (table :initarg :table
          :reader db-error-table
          :initform nil
          :documentation "Table name if applicable"))
  (:report (lambda (c stream)
             (format stream "Database error during ~a~@[ on table ~a~]: ~a"
                     (db-error-operation c)
                     (db-error-table c)
                     (error-message c))))
  (:documentation "Base condition for database operations."))

(define-condition record-not-found (database-error not-found-error)
  ()
  (:documentation "Database record not found. Inherits from both database-error and not-found-error."))

(define-condition duplicate-record (database-error conflict-error)
  ((field :initarg :field
          :reader duplicate-field
          :initform nil
          :documentation "Field that caused the duplicate constraint violation"))
  (:documentation "Duplicate record constraint violation."))

(define-condition connection-error (database-error)
  ()
  (:documentation "Database connection failed."))

;;; Authentication Errors

(define-condition authentication-error (unauthorized-error)
  ()
  (:documentation "Authentication failed - credentials invalid or missing."))

(define-condition invalid-credentials (authentication-error)
  ()
  (:default-initargs :message "Invalid username or password")
  (:documentation "Login credentials are invalid."))

;;; =============================================================================
;;; Condition Signaling Helpers
;;; =============================================================================

(defun bad-request (message &optional details)
  "Signal a bad request error (400).
   MESSAGE is a human-readable description.
   DETAILS is optional structured data (e.g., hash-table, alist)."
  (error 'bad-request-error
         :message message
         :details details))

(defun unauthorized (message)
  "Signal an unauthorized error (401).
   MESSAGE describes why authentication failed."
  (error 'unauthorized-error
         :message message))

(defun forbidden (message)
  "Signal a forbidden error (403).
   MESSAGE describes why access was denied."
  (error 'forbidden-error
         :message message))

(defun not-found (&optional (message "Resource not found") details)
  "Signal a not-found error (404).
   MESSAGE defaults to 'Resource not found'.
   DETAILS is optional structured data."
  (error 'not-found-error
         :message message
         :details details))

(defun conflict (message &optional details)
  "Signal a conflict error (409).
   MESSAGE describes the conflict.
   DETAILS is optional structured data."
  (error 'conflict-error
         :message message
         :details details))

(defun internal-error (message &optional cause)
  "Signal an internal server error (500).
   MESSAGE describes what went wrong.
   CAUSE is the underlying error if any."
  (error 'internal-server-error
         :message message
         :cause cause))

;;; =============================================================================
;;; Response Conversion
;;; =============================================================================

(defun http-error-to-response (condition)
  "Convert an HTTP error condition to a Lack response.
   Returns (status headers body) list."
  (let* ((status (http-error-status condition))
         (message (error-message condition))
         (details (when (slot-boundp condition 'details)
                    (http-error-details condition)))
         (body (format-error-response status message details)))
    (list status
          '(:content-type "application/json; charset=utf-8")
          (list (com.inuoe.jzon:stringify body)))))

(defun format-error-response (status message &optional details)
  "Format an error response as a hash-table for JSON serialization."
  (let ((response (make-hash-table :test 'equal)))
    (setf (gethash "error" response) (status-to-error-type status))
    (setf (gethash "message" response) message)
    (when details
      (setf (gethash "details" response) details))
    response))

(defun status-to-error-type (status)
  "Convert HTTP status code to an error type string."
  (case status
    (400 "bad_request")
    (401 "unauthorized")
    (403 "forbidden")
    (404 "not_found")
    (405 "method_not_allowed")
    (409 "conflict")
    (422 "validation_error")
    (429 "rate_limited")
    (500 "internal_error")
    (502 "bad_gateway")
    (503 "service_unavailable")
    (t (format nil "error_~a" status))))
