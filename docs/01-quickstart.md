# Quickstart

Build a JSON API in Common Lisp in 5 minutes.

## Prerequisites

- SBCL or another Common Lisp implementation
- Quicklisp installed
- inquisitio configured (see [Database Guide](03-database.md))

## Install

Add quickapi to your ASDF load path:

```bash
# Clone to your local-projects
cd ~/quicklisp/local-projects
git clone <quickapi-repo-url> quickapi
```

Load in your REPL:

```lisp
(ql:quickload :quickapi)
```

## Create Your First API

Create a file `hello.lisp`:

```lisp
(defpackage :hello
  (:use :cl :quickapi))

(in-package :hello)

;; Define your API
(defapi my-api :name "My First API" :version "1.0")

;; Add a route
(api-get "/" ()
  (let ((response (make-hash-table :test 'equal)))
    (setf (gethash "message" response) "Hello, World!")
    response))

;; Start the server
(start :port 8000)
```

## Run

Load and run in your REPL:

```lisp
(load "hello.lisp")
```

Expected output:

```
Server started on http://0.0.0.0:8000/
```

## Verify

In another terminal:

```bash
curl http://localhost:8000/
```

Expected output:

```json
{"message":"Hello, World!"}
```

You now have a working JSON API in Common Lisp.

## Next Steps

- [Tutorial](02-tutorial.md) - Build a complete todo API with database
- [Database Guide](03-database.md) - Add SQLite persistence
- [Deployment](04-deployment.md) - Deploy to production
