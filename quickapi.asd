;;;; ABOUTME: System definition for quickapi - curated JSON API stack for Common Lisp

(asdf:defsystem "quickapi"
  :version "0.1.0"
  :author "Quasilabs"
  :license "MIT"
  :description "Curated stack for JSON APIs in Common Lisp - like FastAPI but simpler"
  :long-description "Quickapi makes building JSON APIs in Common Lisp as easy as FastAPI
makes it in Python. It's not a framework - it's a carefully curated combination of
proven libraries (Snooze, jzon, cl-sqlite) with thin glue code and excellent documentation."

  :depends-on ("clack"            ; HTTP server abstraction
               "lack"             ; Middleware framework
               "lack-request"     ; Request parsing
               "lack-response"    ; Response utilities
               "com.inuoe.jzon"  ; Modern JSON library
               "cl-ppcre"        ; Regex for validation
               "sqlite"          ; cl-sqlite for database
               "jose"            ; JWT/JOSE implementation
               "ironclad"        ; Cryptography for password hashing
               "cl-base64")      ; Base64 encoding for password storage

  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "response")
                             (:file "validation")
                             (:file "auth")
                             (:file "lack-app")
                             (:file "core")
                             (:file "sqlite")
                             (:file "models"))))

  :in-order-to ((test-op (test-op "quickapi/tests"))))

(asdf:defsystem "quickapi/tests"
  :depends-on ("quickapi" "fiveam")
  :components ((:module "tests"
                :serial t
                :components ((:file "package")
                             (:file "response-tests")
                             (:file "validation-tests")
                             (:file "route-registry-tests")
                             (:file "json-parsing-tests")
                             (:file "sqlite-tests")
                             (:file "model-tests")
                             (:file "auth-tests")
                             (:file "integration-tests")
                             (:file "tests"))))
  :perform (test-op (o c)
             (symbol-call :fiveam :run! :quickapi-tests)))
