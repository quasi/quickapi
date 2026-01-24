# My API

A JSON API built with [quickapi](https://github.com/your-org/quickapi).

## Quick Start

```bash
# Load and run
sbcl --eval '(ql:quickload :my-api)' --eval '(my-api:main)'

# Test
curl http://localhost:8000/
curl http://localhost:8000/health
```

## Development

Edit `src/app.lisp` to add routes and customize your API.

```lisp
;; Add a new route
(api-get "/hello/:name" (name)
  (let ((h (make-hash-table :test 'equal)))
    (setf (gethash "message" h) (format nil "Hello, ~a!" name))
    h))
```

Reload in REPL after changes.

## Building

```bash
# Build executable
make build

# Run executable
./my-api
```

## Docker

```bash
# Build image
make docker-build

# Run container
make docker-run
```

## Configuration

Environment variables:

| Variable | Default | Description |
|----------|---------|-------------|
| `PORT` | 8000 | Server port |
| `DB_PATH` | app.db | SQLite database path |

## Project Structure

```
my-api/
├── my-api.asd      # System definition
├── src/
│   └── app.lisp    # Main application
├── Dockerfile
├── Makefile
└── README.md
```

## License

MIT
