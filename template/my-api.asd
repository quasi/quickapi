;;;; ABOUTME: Template ASDF system definition - copy and rename for your project

(asdf:defsystem "my-api"
  :version "0.1.0"
  :author "Your Name"
  :license "MIT"
  :description "My API built with quickapi"

  :depends-on ("quickapi")

  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "app")))))
