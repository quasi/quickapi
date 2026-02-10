# ABOUTME: Makefile for quickapi applications

.PHONY: build run clean docker-build docker-run test check-quicklisp

# Application name
APP = my-api

# Quicklisp check — verify ql:quickload is available before proceeding
check-quicklisp:
	@sbcl --non-interactive \
		--eval '(unless (find-package :ql) (format *error-output* "~&ERROR: Quicklisp is not installed.~%Install it from https://www.quicklisp.org/beta/~%") (sb-ext:exit :code 1))' \
		--eval '(sb-ext:exit :code 0)'

# Register project .asd with ASDF, then load via Quicklisp
LOAD_APP = --eval '(push (truename ".") asdf:*central-registry*)' \
           --eval '(ql:quickload :$(APP))'

# Build standalone executable
build: check-quicklisp
	sbcl --non-interactive \
		$(LOAD_APP) \
		--eval '(sb-ext:save-lisp-and-die "$(APP)" :toplevel (quote $(APP):main) :executable t :compression t)'

# Run in development
run: check-quicklisp
	sbcl $(LOAD_APP) --eval '($(APP):main)'

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

# Run tests
test: check-quicklisp
	sbcl --non-interactive \
		--eval '(push (truename ".") asdf:*central-registry*)' \
		--eval '(ql:quickload :$(APP)/tests)' \
		--eval '(asdf:test-system :$(APP))'
