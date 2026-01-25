;;;; ABOUTME: Response helpers for quickapi - semantic core for HTTP responses

(in-package :quickapi)

;;; JSON Response Construction

(defun json-response (data &key (status 200))
  "Create a JSON response with the given data and HTTP status code.
   DATA can be a hash-table, plist, alist, list, or any jzon-serializable value."
  (list status
        '(:content-type "application/json; charset=utf-8")
        (list (com.inuoe.jzon:stringify data))))

;;; Success Response Helpers

(defun make-response-data (args)
  "Convert response arguments to JSON-serializable data.
   If ARGS is a single value, return it as-is.
   If ARGS is keyword pairs, build a hash-table from them."
  (cond
    ;; No args - empty object
    ((null args)
     (make-hash-table :test 'equal))
    ;; Single non-keyword argument - return as-is
    ((and (= 1 (length args))
          (not (keywordp (first args))))
     (first args))
    ;; Keyword pairs - build hash-table
    ((keywordp (first args))
     (let ((ht (make-hash-table :test 'equal)))
       (loop for (key val) on args by #'cddr
             do (setf (gethash (string-downcase (symbol-name key)) ht) val))
       ht))
    ;; Single argument
    (t (first args))))

(defun ok (&rest args)
  "Return 200 OK with JSON data.
   Can be called as:
     (ok hash-table)           ; Pass existing data
     (ok :key1 val1 :key2 val2) ; Build object from keyword pairs"
  (json-response (make-response-data args) :status 200))

(defun created (&rest args)
  "Return 201 Created with JSON data.
   Can be called as:
     (created hash-table)           ; Pass existing data
     (created :key1 val1 :key2 val2) ; Build object from keyword pairs"
  (json-response (make-response-data args) :status 201))

(defun no-content ()
  "Return 204 No Content with empty body."
  (list 204 '() '()))

;;; Error Response Helpers
;;; These signal HTTP error conditions for proper error handling

(defun format-error (error-type message &optional details)
  "Format an error response body as a hash-table."
  (let ((response (make-hash-table :test 'equal)))
    (setf (gethash "error" response) error-type)
    (setf (gethash "message" response) message)
    (when details
      (setf (gethash "details" response) details))
    response))

(defun bad-request (message &optional details)
  "Signal 400 Bad Request. MESSAGE is a human-readable error description.
   DETAILS is optional additional structured data about the error."
  (http-condition 400
    (com.inuoe.jzon:stringify (format-error "bad_request" message details))))

(defun not-found (&optional (message "Resource not found"))
  "Signal 404 Not Found. MESSAGE defaults to 'Resource not found'."
  (http-condition 404
    (com.inuoe.jzon:stringify (format-error "not_found" message))))

(defun error-response (status message &optional details)
  "Signal an HTTP error condition with the given status code and message.
   Use this for custom error status codes (401, 403, 409, 500, etc.)."
  (http-condition status
    (com.inuoe.jzon:stringify (format-error (status-to-error-type status) message details))))

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
