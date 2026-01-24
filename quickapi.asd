;;;; ABOUTME: System definition for quickapi - curated JSON API stack for Common Lisp

(asdf:defsystem "quickapi"
  :version "0.1.0"
  :author "Quasilabs"
  :license "MIT"
  :description "Curated stack for JSON APIs in Common Lisp - like FastAPI but simpler"
  :long-description "Quickapi makes building JSON APIs in Common Lisp as easy as FastAPI
makes it in Python. It's not a framework - it's a carefully curated combination of
proven libraries (Snooze, jzon, cl-sqlite) with thin glue code and excellent documentation."

  :depends-on ("snooze"           ; CLOS-based routing, ~850 LoC
               "hunchentoot"     ; HTTP server
               "com.inuoe.jzon"  ; Modern JSON library
               "cl-ppcre"        ; Regex for validation
               "sqlite")         ; cl-sqlite for database

  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "response")
                             (:file "validation")
                             (:file "core")
                             (:file "sqlite"))))

  :in-order-to ((test-op (test-op "quickapi/tests"))))

(asdf:defsystem "quickapi/tests"
  :depends-on ("quickapi" "fiveam")
  :components ((:module "tests"
                :serial t
                :components ((:file "package")
                             (:file "response-tests")
                             (:file "validation-tests")
                             (:file "route-registry-tests")
                             (:file "sqlite-tests")
                             (:file "integration-tests")
                             (:file "tests"))))
  :perform (test-op (o c)
             (symbol-call :fiveam :run! :quickapi-tests)))
