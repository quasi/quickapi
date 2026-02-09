;;;; ABOUTME: Lack application for quickapi - HTTP handling via Clack/Lack

(in-package :quickapi)

;;; HTTP Error Condition
;;; Replaces snooze:http-condition with a proper CL condition + restart pattern

(define-condition http-error (error)
  ((status :initarg :status :reader http-error-status)
   (body :initarg :body :reader http-error-body))
  (:report (lambda (c stream)
             (format stream "HTTP ~a: ~a"
                     (http-error-status c)
                     (http-error-body c))))
  (:documentation "Condition for HTTP error responses."))

(defun http-condition (status body)
  "Signal an HTTP error condition. BODY should be a JSON string.
   This replaces snooze:http-condition."
  (error 'http-error :status status :body body))

;;; Route Handler Registry
;;; Each route stores a handler function (closure) that takes (env params)
;;; along with optional auth requirements

(defstruct handler-entry
  "Entry in the handler registry."
  (function nil :type function)
  (auth-type nil :type (or null keyword)))

(defvar *route-handlers* (make-hash-table :test 'equal)
  "Maps (method . uri-pattern) to handler-entry structs.")

(defun register-handler (method uri-pattern handler &optional auth-type)
  "Register a handler function for a route with optional AUTH-TYPE."
  (let ((key (cons method uri-pattern)))
    (setf (gethash key *route-handlers*)
          (make-handler-entry :function handler :auth-type auth-type))))

(defun find-handler (method uri-pattern)
  "Find the handler entry for a route."
  (gethash (cons method uri-pattern) *route-handlers*))

;;; Request Parsing

(defun env-to-method (env)
  "Extract HTTP method from Lack env as keyword."
  (let ((method (getf env :request-method)))
    (if (keywordp method)
        method
        (intern (string-upcase method) :keyword))))

(defun env-to-path (env)
  "Extract path from Lack env."
  (getf env :path-info "/"))

(defun env-to-body-string (env)
  "Read request body as string from Lack env."
  (let ((raw-body (getf env :raw-body)))
    (when raw-body
      (let ((content-length (getf env :content-length)))
        (when (and content-length (plusp content-length))
          (let ((buffer (make-array content-length :element-type '(unsigned-byte 8))))
            (read-sequence buffer raw-body)
            (babel:octets-to-string buffer :encoding :utf-8)))))))

(defun parse-json-body-from-env (env)
  "Parse JSON body from Lack env."
  (let ((body-string (env-to-body-string env)))
    (when (and body-string (plusp (length body-string)))
      (handler-case
          (com.inuoe.jzon:parse body-string)
        (error () nil)))))

;;; Lack Application

(defun make-quickapi-app ()
  "Create a Lack application for quickapi.
   Returns a function (lambda (env) ...) suitable for Clack."
  (lambda (env)
    (let* ((method (env-to-method env))
           (path (env-to-path env))
           (*request* env)
           (*body* nil)
           (*current-user* nil))
      ;; Match route and dispatch
      (multiple-value-bind (resource-name params)
          (match-uri-to-route path method)
        (if resource-name
            ;; Found a matching route
            (let* ((pattern (find-pattern-for-resource resource-name method))
                   (handler-entry (when pattern (find-handler method pattern))))
              (if handler-entry
                  (dispatch-to-handler handler-entry env params method)
                  (make-error-response 404 "not_found" "Route not found")))
            ;; No matching route
            (make-error-response 404 "not_found" "Route not found"))))))

(defun find-pattern-for-resource (resource-name method)
  "Find the URI pattern for a resource name by searching *route-registry*.
   Returns pattern string or NIL if not found."
  (maphash (lambda (key entry)
             (declare (ignore key))
             (when (and (eq (route-entry-method entry) method)
                        (eq (route-entry-resource-name entry) resource-name))
               (return-from find-pattern-for-resource
                 (route-entry-pattern entry))))
           *route-registry*)
  nil)

(defun dispatch-to-handler (handler-entry env params method)
  "Dispatch to a route handler, handling auth and errors gracefully."
  (let ((handler (handler-entry-function handler-entry))
        (auth-type (handler-entry-auth-type handler-entry)))
    ;; Check authentication if required
    (when auth-type
      (multiple-value-bind (success user error-message)
          (perform-auth-check auth-type env)
        (if success
            (setf *current-user* user)
            (return-from dispatch-to-handler
              (make-error-response 401 "unauthorized"
                                   (or error-message "Authentication required"))))))
    ;; Auth passed (or not required) - dispatch to handler
    (handler-case
        (let ((*body* (when (member method '(:post :put :patch))
                        (parse-json-body-from-env env))))
          (funcall handler env params))
      ;; Handle HTTP error conditions
      (http-error (e)
        (list (http-error-status e)
              '(:content-type "application/json; charset=utf-8")
              (list (http-error-body e))))
      ;; Handle validation errors
      (validation-error (e)
        (list 422
              '(:content-type "application/json; charset=utf-8")
              (list (com.inuoe.jzon:stringify
                     (format-error "validation_error" "Validation failed"
                                   (validation-errors e))))))
      ;; Handle unexpected errors
      (error (e)
        (if *debug-mode*
            (error e)  ; Re-signal in debug mode for better debugging
            (make-error-response 500 "internal_error"
                                 (format nil "~a" e)))))))

(defun make-error-response (status error-type message)
  "Create a JSON error response."
  (list status
        '(:content-type "application/json; charset=utf-8")
        (list (com.inuoe.jzon:stringify (format-error error-type message)))))

;;; Middleware Builder

(defvar *middlewares* nil
  "List of middleware to apply to the app.")

(defun build-app-with-middleware (app middlewares)
  "Wrap APP with the given MIDDLEWARES (applied in order)."
  (reduce (lambda (inner-app mw)
            (apply-middleware mw inner-app))
          (reverse middlewares)
          :initial-value app))

(defun apply-middleware (middleware-spec app)
  "Apply a single middleware spec to APP.
   MIDDLEWARE-SPEC can be:
   - A keyword like :accesslog
   - A list like (:cors :origins (\"*\"))

   Middleware packages are loaded on demand."
  (let ((name (if (consp middleware-spec)
                  (car middleware-spec)
                  middleware-spec))
        (args (if (consp middleware-spec)
                  (cdr middleware-spec)
                  nil)))
    (case name
      (:accesslog
       (asdf:load-system "lack-middleware-accesslog" :verbose nil)
       (funcall (find-symbol "LACK-MIDDLEWARE-ACCESSLOG" "LACK.MIDDLEWARE.ACCESSLOG") app))
      (:session
       (asdf:load-system "lack-middleware-session" :verbose nil)
       (apply (find-symbol "LACK-MIDDLEWARE-SESSION" "LACK.MIDDLEWARE.SESSION") app args))
      (:backtrace
       (asdf:load-system "lack-middleware-backtrace" :verbose nil)
       (funcall (find-symbol "LACK-MIDDLEWARE-BACKTRACE" "LACK.MIDDLEWARE.BACKTRACE") app))
      (t (warn "Unknown middleware: ~a" name)
         app))))
