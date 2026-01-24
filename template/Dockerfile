# ABOUTME: Dockerfile for quickapi applications

FROM clfoundation/sbcl:latest

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libsqlite3-dev \
    curl \
    && rm -rf /var/lib/apt/lists/*

# Set up Quicklisp (if not already in base image)
RUN curl -O https://beta.quicklisp.org/quicklisp.lisp && \
    sbcl --non-interactive \
         --load quicklisp.lisp \
         --eval '(quicklisp-quickstart:install)' \
         --eval '(ql:add-to-init-file)' \
         --eval '(quit)' && \
    rm quicklisp.lisp

# Pre-load quickapi dependencies
RUN sbcl --non-interactive \
    --eval '(ql:quickload :quickapi)' \
    --eval '(quit)'

# Copy application
WORKDIR /app
COPY . .

# Build executable
RUN sbcl --non-interactive \
    --eval '(ql:quickload :my-api)' \
    --eval '(sb-ext:save-lisp-and-die "my-api" :toplevel #'"'"'my-api:main :executable t :compression t)'

# Create data directory
RUN mkdir -p /app/data

# Expose port
EXPOSE 8000

# Health check
HEALTHCHECK --interval=30s --timeout=3s \
  CMD curl -f http://localhost:8000/health || exit 1

# Environment
ENV PORT=8000
ENV DB_PATH=/app/data/app.db

# Run
CMD ["./my-api"]
