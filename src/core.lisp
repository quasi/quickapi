;;;; ABOUTME: Core route macros for quickapi - thin DSL layer over Clack/Lack

(in-package :quickapi)

;;; API Definition and Server Management

(defvar *api* nil
  "The current API instance. Set by DEFAPI.")

(defvar *server* nil
  "The running Clack handler.")

(defvar *route-registry* (make-hash-table :test 'equal)
  "Registry of route patterns to resource names for URI matching.")

(defclass api ()
  ((name :initarg :name :accessor api-name)
   (version :initarg :version :accessor api-version :initform "1.0")
   (description :initarg :description :accessor api-description :initform nil)
   (middlewares :initarg :middlewares :accessor api-middlewares :initform nil))
  (:documentation "An API definition containing metadata."))

(defmacro defapi (symbol-name &key name (version "1.0") description middlewares)
  "Define an API with the given SYMBOL-NAME, optional :NAME, VERSION, DESCRIPTION,
   and MIDDLEWARES.

   MIDDLEWARES is a list of middleware specifications:
   - :accesslog - Request logging
   - :session - Session management
   - :backtrace - Error backtraces in development
   - (:cors :origins '(\"*\")) - CORS with options

   Example:
     (defapi my-api
       :name \"My API\"
       :version \"1.0\"
       :description \"A sample API\"
       :middlewares (:accesslog :session))"
  `(progn
     (setf *api* (make-instance 'api
                                :name ,(or name (string symbol-name))
                                :version ,version
                                :description ,description
                                :middlewares ',middlewares))
     *api*))

;;; Route Registry and URI Matching

(defstruct route-entry
  "Entry in the route registry."
  (pattern nil :type string)           ; Original URI pattern like "/todos/:id"
  (resource-name nil :type symbol)     ; Unique route identifier
  (segments nil :type list)            ; Parsed segments: (:literal . "todos") or (:param . id)
  (method nil :type keyword))          ; HTTP method

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parse-uri-pattern (uri)
    "Parse a URI pattern into a list of segment descriptors.
Returns list of (:literal . string) or (:param . symbol).
Param symbols are interned in the QUICKAPI package for consistency."
    (loop for part in (cl-ppcre:split "/" uri)
          when (plusp (length part))
            collect (if (char= (char part 0) #\:)
                        (cons :param (intern (string-upcase (subseq part 1)) :quickapi))
                        (cons :literal part)))))

(defun register-route (uri-pattern resource-name method)
  "Register a route pattern with its resource name."
  (let* ((key (cons method uri-pattern))
         (segments (parse-uri-pattern uri-pattern))
         (entry (make-route-entry :pattern uri-pattern
                                  :resource-name resource-name
                                  :segments segments
                                  :method method)))
    (setf (gethash key *route-registry*) entry)))

(defun match-uri-to-route (uri method)
  "Match a request URI to a registered route.
Returns (values resource-name extracted-args) or NIL."
  ;; Strip query string if present
  (let* ((path (let ((qpos (position #\? uri)))
                 (if qpos (subseq uri 0 qpos) uri)))
         (uri-segments (remove-if (lambda (s) (zerop (length s)))
                                  (cl-ppcre:split "/" path))))
    ;; Try to find a matching route
    (maphash (lambda (key entry)
               (declare (ignore key))
               (when (eq (route-entry-method entry) method)
                 (let ((pattern-segments (route-entry-segments entry)))
                   (when (= (length uri-segments) (length pattern-segments))
                     (let ((args nil)
                           (match t))
                       (loop for uri-seg in uri-segments
                             for pat-seg in pattern-segments
                             do (case (car pat-seg)
                                  (:literal
                                   (unless (string-equal uri-seg (cdr pat-seg))
                                     (setf match nil)
                                     (return)))
                                  (:param
                                   (push (cons (cdr pat-seg) uri-seg) args))))
                       (when match
                         (return-from match-uri-to-route
                           (values (route-entry-resource-name entry)
                                   (nreverse args)))))))))
             *route-registry*)
    nil))

;;; Server Start/Stop

(defun start (&key (port 8000) (address "0.0.0.0") (server :hunchentoot))
  "Start the API server on PORT (default 8000).
   SERVER can be :hunchentoot (default) or :woo (faster, async)."
  (when *server*
    (stop))
  (let* ((base-app (make-quickapi-app))
         (middlewares (when *api* (api-middlewares *api*)))
         (app (if middlewares
                  (build-app-with-middleware base-app middlewares)
                  base-app)))
    (setf *server* (clack:clackup app
                                   :port port
                                   :address address
                                   :server server
                                   :use-thread t
                                   :silent t))
    (format t "~&Server started on http://~a:~a/~%" address port)
    *server*))

(defun stop ()
  "Stop the running API server."
  (when *server*
    (clack:stop *server*)
    (setf *server* nil)
    (format t "~&Server stopped~%")))

;;; Request Context Helpers

(defun body ()
  "Get the parsed JSON body of the current request.
   Returns a hash-table or nil if no JSON body."
  *body*)

;;; Route Definition Macros
;;; These define routes as closures stored in the handler registry

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun generate-resource-name (uri method)
    "Generate a unique resource name from URI pattern and method.
E.g., /todos/:id with GET -> TODOS/:ID/GET"
    (let* ((clean-uri (substitute #\/ #\- (string-left-trim "/" uri)))
           (name-str (format nil "~a/~a"
                             (string-upcase clean-uri)
                             method)))
      (intern name-str :quickapi))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun extract-auth-option (lambda-list)
    "Extract :auth option from route lambda-list.
     Returns (values auth-type remaining-options).
     E.g., (:auth :jwt) -> :jwt"
    (let ((auth-pos (position :auth lambda-list)))
      (if (and auth-pos (< (1+ auth-pos) (length lambda-list)))
          (elt lambda-list (1+ auth-pos))
          nil))))

(defmacro define-json-route (method uri lambda-list &body body)
  "Internal macro to define a JSON route.

   Path parameters are extracted from URI (e.g., :id from /users/:id).
   The route handler is stored as a closure that binds path params as local variables.

   LAMBDA-LIST can contain :auth option:
     (:auth :jwt)     - Require JWT authentication
     (:auth :session) - Require session authentication
     (:auth :api-key) - Require API key authentication"
  (let* ((auth-type (extract-auth-option lambda-list))
         (resource-name (generate-resource-name uri method))
         ;; Parse URI segments
         (parsed-segments (parse-uri-pattern uri))
         ;; Extract path parameter names in order
         (path-params (loop for seg in parsed-segments
                            when (eq (car seg) :param)
                              collect (cdr seg))))
    `(progn
       ;; Register the route pattern for URI matching
       (register-route ,uri ',resource-name ,method)

       ;; Register the handler as a closure with auth info
       (register-handler ,method ,uri
         (lambda (env params)
           (declare (ignorable env))
           ;; Bind path parameters from the matched params alist
           (let (,@(loop for param in path-params
                         collect `(,param (cdr (assoc ',param params)))))
             (let ((result (progn ,@body)))
               ;; Convert result to Lack response format
               (response-to-lack result))))
         ,auth-type))))

(defun response-to-lack (result)
  "Convert a route handler result to Lack response format (status headers body)."
  (typecase result
    ;; Already a proper response (list of status, headers, body)
    (cons (if (and (integerp (first result))
                   (listp (second result)))
              ;; It's a full response - use as-is
              result
              ;; It's data - serialize to JSON
              (list 200
                    '(:content-type "application/json; charset=utf-8")
                    (list (com.inuoe.jzon:stringify result)))))
    ;; Hash-table or other data - serialize to JSON
    (t (list 200
             '(:content-type "application/json; charset=utf-8")
             (list (com.inuoe.jzon:stringify result))))))

(defmacro api-get (uri (&rest lambda-list) &body body)
  "Define a GET route. Response body is automatically JSON-encoded.
   Path parameters (e.g., :id in /users/:id) are auto-extracted and
   bound as variables in BODY.

   Options in lambda-list:
     :auth :jwt     - Require JWT authentication
     :auth :session - Require session authentication
     :auth :api-key - Require API key authentication

   Example:
     (api-get \"/users\" ()
       (list-all-users))

     (api-get \"/users/:id\" ()
       ;; id is automatically bound from the path
       (find-user id))

     (api-get \"/profile\" (:auth :jwt)
       ;; *current-user* bound to JWT claims
       (ok *current-user*))"
  `(define-json-route :get ,uri ,lambda-list ,@body))

(defmacro api-post (uri (&rest lambda-list) &body body)
  "Define a POST route. *body* is bound to parsed JSON request body.
   Path parameters are auto-extracted. Response is automatically JSON-encoded.

   Options in lambda-list:
     :auth :jwt     - Require JWT authentication
     :auth :session - Require session authentication
     :auth :api-key - Require API key authentication

   Example:
     (api-post \"/users\" ()
       (validate *body*
         (require-fields \"name\" \"email\"))
       (create-user *body*))

     (api-post \"/todos\" (:auth :jwt)
       ;; Protected route - *current-user* available
       (create-todo *body*))"
  `(define-json-route :post ,uri ,lambda-list ,@body))

(defmacro api-put (uri (&rest lambda-list) &body body)
  "Define a PUT route. *body* is bound to parsed JSON request body.
   Path parameters are auto-extracted. Response is automatically JSON-encoded.

   Options in lambda-list:
     :auth :jwt     - Require JWT authentication
     :auth :session - Require session authentication
     :auth :api-key - Require API key authentication

   Example:
     (api-put \"/users/:id\" (:auth :jwt)
       ;; id is automatically bound from the path
       (update-user id *body*))"
  `(define-json-route :put ,uri ,lambda-list ,@body))

(defmacro api-patch (uri (&rest lambda-list) &body body)
  "Define a PATCH route. *body* is bound to parsed JSON request body.
   Path parameters are auto-extracted. Response is automatically JSON-encoded.

   Options in lambda-list:
     :auth :jwt     - Require JWT authentication
     :auth :session - Require session authentication
     :auth :api-key - Require API key authentication

   Example:
     (api-patch \"/users/:id\" (:auth :session)
       ;; id is automatically bound from the path
       (partial-update-user id *body*))"
  `(define-json-route :patch ,uri ,lambda-list ,@body))

(defmacro api-delete (uri (&rest lambda-list) &body body)
  "Define a DELETE route. Path parameters are auto-extracted.
   Response is automatically JSON-encoded.

   Options in lambda-list:
     :auth :jwt     - Require JWT authentication
     :auth :session - Require session authentication
     :auth :api-key - Require API key authentication

   Example:
     (api-delete \"/users/:id\" (:auth :jwt)
       ;; id is automatically bound from the path
       (delete-user id)
       (no-content))"
  `(define-json-route :delete ,uri ,lambda-list ,@body))
