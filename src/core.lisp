;;;; ABOUTME: Core route macros for quickapi - thin DSL layer over Snooze

(in-package :quickapi)

;;; API Definition and Server Management

(defvar *api* nil
  "The current API instance. Set by DEFAPI.")

(defvar *server* nil
  "The running Hunchentoot acceptor.")

(defclass api ()
  ((name :initarg :name :accessor api-name)
   (version :initarg :version :accessor api-version :initform "1.0")
   (description :initarg :description :accessor api-description :initform nil))
  (:documentation "An API definition containing metadata."))

(defmacro defapi (name &key (version "1.0") description)
  "Define an API with the given NAME, VERSION, and DESCRIPTION.
   This sets up the API metadata and prepares for route definitions."
  `(progn
     (setf *api* (make-instance 'api
                                :name ,(string name)
                                :version ,version
                                :description ,description))
     *api*))

(defun start (&key (port 8000) (address "0.0.0.0"))
  "Start the API server on PORT (default 8000).
   Uses Hunchentoot as the backend."
  (when *server*
    (stop))
  ;; Create and start hunchentoot acceptor
  (let ((acceptor (make-instance 'hunchentoot:easy-acceptor
                                 :port port
                                 :address address)))
    ;; Install snooze as the dispatcher
    (setf hunchentoot:*dispatch-table*
          (list (snooze:make-hunchentoot-app)))
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

(defmacro define-json-route (method uri lambda-list &body body)
  "Internal macro to define a JSON route. Used by GET, POST, etc."
  (let* ((resource-name (intern (string-upcase (subseq uri 1 (or (position #\/ uri :start 1)
                                                                  (position #\: uri :start 1)
                                                                  (length uri))))))
         (has-body-p (member method '(:post :put :patch)))
         ;; Parse URI to extract path params
         (uri-parts (cl-ppcre:split "/" uri))
         (path-params (loop for part in uri-parts
                            when (and (plusp (length part))
                                      (char= (char part 0) #\:))
                              collect (intern (string-upcase (subseq part 1))))))
    `(snooze:defroute ,resource-name
         (,method "application/json" ,@path-params ,@lambda-list)
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
             (t (values (com.inuoe.jzon:stringify result) 200 "application/json"))))))))

(defmacro api-get (uri lambda-list &body body)
  "Define a GET route. Response body is automatically JSON-encoded.

   Example:
     (GET \"/users\" ()
       (list-all-users))

     (GET \"/users/:id\" (id)
       (find-user id))"
  `(define-json-route :get ,uri ,lambda-list ,@body))

(defmacro api-post (uri lambda-list &body body)
  "Define a POST route. *body* is bound to parsed JSON request body.
   Response is automatically JSON-encoded.

   Example:
     (POST \"/users\" ()
       (validate *body*
         (require-fields \"name\" \"email\"))
       (create-user *body*))"
  `(define-json-route :post ,uri ,lambda-list ,@body))

(defmacro api-put (uri lambda-list &body body)
  "Define a PUT route. *body* is bound to parsed JSON request body.
   Response is automatically JSON-encoded.

   Example:
     (PUT \"/users/:id\" (id)
       (update-user id *body*))"
  `(define-json-route :put ,uri ,lambda-list ,@body))

(defmacro api-patch (uri lambda-list &body body)
  "Define a PATCH route. *body* is bound to parsed JSON request body.
   Response is automatically JSON-encoded.

   Example:
     (PATCH \"/users/:id\" (id)
       (partial-update-user id *body*))"
  `(define-json-route :patch ,uri ,lambda-list ,@body))

(defmacro api-delete (uri lambda-list &body body)
  "Define a DELETE route. Response is automatically JSON-encoded.

   Example:
     (DELETE \"/users/:id\" (id)
       (delete-user id)
       (no-content))"
  `(define-json-route :delete ,uri ,lambda-list ,@body))
