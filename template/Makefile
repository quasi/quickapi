# ABOUTME: Makefile for quickapi applications

.PHONY: build run clean docker-build docker-run test

# Application name
APP = my-api

# Build standalone executable
build:
	sbcl --non-interactive \
		--eval '(ql:quickload :$(APP))' \
		--eval '(sb-ext:save-lisp-and-die "$(APP)" :toplevel (quote $(APP):main) :executable t :compression t)'

# Run in development
run:
	sbcl --eval '(ql:quickload :$(APP))' --eval '($(APP):main)'

# Clean build artifacts
clean:
	rm -f $(APP) *.fasl
	rm -rf data/

# Build Docker image
docker-build:
	docker build -t $(APP) .

# Run Docker container
docker-run:
	docker run -p 8000:8000 -v $(PWD)/data:/app/data $(APP)

# Run tests (if you have them)
test:
	sbcl --non-interactive \
		--eval '(ql:quickload :$(APP)/tests)' \
		--eval '(asdf:test-system :$(APP))'
