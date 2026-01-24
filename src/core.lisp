;;;; ABOUTME: Core route macros for quickapi - thin DSL layer over Snooze

(in-package :quickapi)

;;; API Definition and Server Management

(defvar *api* nil
  "The current API instance. Set by DEFAPI.")

(defvar *server* nil
  "The running Hunchentoot acceptor.")

(defvar *route-registry* (make-hash-table :test 'equal)
  "Registry of route patterns to resource names for URI matching.")

(defclass api ()
  ((name :initarg :name :accessor api-name)
   (version :initarg :version :accessor api-version :initform "1.0")
   (description :initarg :description :accessor api-description :initform nil))
  (:documentation "An API definition containing metadata."))

(defmacro defapi (symbol-name &key name (version "1.0") description)
  "Define an API with the given SYMBOL-NAME, optional :NAME, VERSION, and DESCRIPTION.
   SYMBOL-NAME is used as the API identifier.
   :NAME is an optional display name (defaults to SYMBOL-NAME as string).
   This sets up the API metadata and prepares for route definitions.

   Example:
     (defapi my-api
       :name \"My API\"
       :version \"1.0\"
       :description \"A sample API\")"
  `(progn
     (setf *api* (make-instance 'api
                                :name ,(or name (string symbol-name))
                                :version ,version
                                :description ,description))
     *api*))

;;; Route Registry and URI Matching

(defstruct route-entry
  "Entry in the route registry."
  (pattern nil :type string)           ; Original URI pattern like "/todos/:id"
  (resource-name nil :type symbol)     ; Snooze resource name
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
  (let* ((uri-obj (quri:uri uri))
         (path (quri:uri-path uri-obj))
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

(defun quickapi-resource-name (uri)
  "Custom resource name function for quickapi.
Matches URI against registered route patterns."
  (let* ((uri-obj (quri:uri uri))
         (path (quri:uri-path uri-obj))
         (query (quri:uri-query uri-obj)))
    ;; Try each HTTP method (we'll refine based on actual request later)
    (dolist (method '(:get :post :put :patch :delete))
      (multiple-value-bind (resource-name args)
          (match-uri-to-route uri method)
        (when resource-name
          ;; Return resource name and remaining URI (query string only)
          (return-from quickapi-resource-name
            (values (string-downcase (symbol-name resource-name))
                    (if query (format nil "?~a" query) ""))))))
    ;; Fallback to Snooze's default
    (snooze::default-resource-name uri)))

(defun start (&key (port 8000) (address "0.0.0.0"))
  "Start the API server on PORT (default 8000).
   Uses Hunchentoot as the backend."
  (when *server*
    (stop))
  ;; Create and start hunchentoot acceptor
  (let ((acceptor (make-instance 'hunchentoot:easy-acceptor
                                 :port port
                                 :address address)))
    ;; Install snooze as the dispatcher with our custom resource name function
    (setf hunchentoot:*dispatch-table*
          (list (snooze:make-hunchentoot-app
                 `((snooze:*resource-name-function* . ,#'quickapi-resource-name)))))
    (hunchentoot:start acceptor)
    (setf *server* acceptor)
    (format t "~&Server started on http://~a:~a/~%" address port)
    *server*))

(defun stop ()
  "Stop the running API server."
  (when *server*
    (hunchentoot:stop *server*)
    (setf *server* nil)
    (format t "~&Server stopped~%")))

;;; Request Context Helpers

(defun parse-json-body ()
  "Parse the JSON body from the current request."
  (let ((body-string (snooze:payload-as-string)))
    (when (and body-string (plusp (length body-string)))
      (com.inuoe.jzon:parse body-string))))

(defun body ()
  "Get the parsed JSON body of the current request.
   Returns a hash-table or nil if no JSON body."
  *body*)

;;; Route Definition Macros
;;; These are thin wrappers around snooze:defroute that:
;;; 1. Set up JSON content type
;;; 2. Parse JSON body for POST/PUT/PATCH
;;; 3. Bind *body*
;;; 4. Auto-serialize response to JSON

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun generate-resource-name (uri method)
    "Generate a unique resource name from URI pattern and method.
E.g., /todos/:id with GET -> TODOS/:ID/GET"
    (let* ((clean-uri (substitute #\/ #\- (string-left-trim "/" uri)))
           (name-str (format nil "~a/~a"
                             (string-upcase clean-uri)
                             method)))
      (intern name-str :quickapi))))

(defmacro define-json-route (method uri lambda-list &body body)
  "Internal macro to define a JSON route. Used by GET, POST, etc.

   Path parameters are extracted from URI (e.g., :id from /users/:id).
   The LAMBDA-LIST is deprecated - path params are auto-extracted.
   If both are provided, LAMBDA-LIST is ignored to prevent duplicates."
  (declare (ignore lambda-list))
  (let* ((resource-name (generate-resource-name uri method))
         (has-body-p (member method '(:post :put :patch)))
         ;; Parse URI segments
         (parsed-segments (parse-uri-pattern uri))
         ;; Extract path parameter names in order
         (path-params (loop for seg in parsed-segments
                            when (eq (car seg) :param)
                              collect (cdr seg))))
    `(progn
       ;; Register the route pattern for URI matching
       (register-route ,uri ',resource-name ,method)

       ;; Define the Snooze route
       (snooze:defroute ,resource-name
           (,method "application/json" ,@path-params)
         (let ((*body* ,(when has-body-p '(parse-json-body))))
           (let ((result (progn ,@body)))
             (typecase result
               ;; Already a proper response (list of status, headers, body)
               (cons (if (and (integerp (first result))
                              (listp (second result)))
                         ;; It's a full response - extract just the body
                         (values (third (first (third result)))
                                 (first result)
                                 "application/json")
                         ;; It's data - serialize to JSON
                         (values (com.inuoe.jzon:stringify result) 200 "application/json")))
               ;; Hash-table or other data - serialize to JSON
               (t (values (com.inuoe.jzon:stringify result) 200 "application/json")))))))))

(defmacro api-get (uri (&rest lambda-list) &body body)
  "Define a GET route. Response body is automatically JSON-encoded.
   Path parameters (e.g., :id in /users/:id) are auto-extracted and
   bound as variables in BODY.

   Example:
     (api-get \"/users\" ()
       (list-all-users))

     (api-get \"/users/:id\" ()
       ;; id is automatically bound from the path
       (find-user id))"
  (declare (ignore lambda-list))
  `(define-json-route :get ,uri () ,@body))

(defmacro api-post (uri (&rest lambda-list) &body body)
  "Define a POST route. *body* is bound to parsed JSON request body.
   Path parameters are auto-extracted. Response is automatically JSON-encoded.

   Example:
     (api-post \"/users\" ()
       (validate *body*
         (require-fields \"name\" \"email\"))
       (create-user *body*))

     (api-post \"/users/:id/activate\" ()
       ;; id is automatically bound from the path
       (activate-user id))"
  (declare (ignore lambda-list))
  `(define-json-route :post ,uri () ,@body))

(defmacro api-put (uri (&rest lambda-list) &body body)
  "Define a PUT route. *body* is bound to parsed JSON request body.
   Path parameters are auto-extracted. Response is automatically JSON-encoded.

   Example:
     (api-put \"/users/:id\" ()
       ;; id is automatically bound from the path
       (update-user id *body*))"
  (declare (ignore lambda-list))
  `(define-json-route :put ,uri () ,@body))

(defmacro api-patch (uri (&rest lambda-list) &body body)
  "Define a PATCH route. *body* is bound to parsed JSON request body.
   Path parameters are auto-extracted. Response is automatically JSON-encoded.

   Example:
     (api-patch \"/users/:id\" ()
       ;; id is automatically bound from the path
       (partial-update-user id *body*))"
  (declare (ignore lambda-list))
  `(define-json-route :patch ,uri () ,@body))

(defmacro api-delete (uri (&rest lambda-list) &body body)
  "Define a DELETE route. Path parameters are auto-extracted.
   Response is automatically JSON-encoded.

   Example:
     (api-delete \"/users/:id\" ()
       ;; id is automatically bound from the path
       (delete-user id)
       (no-content))"
  (declare (ignore lambda-list))
  `(define-json-route :delete ,uri () ,@body))
