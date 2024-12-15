# Use a lightweight Haskell image as the base
FROM haskell:9.6

# Install system dependencies
RUN apt-get update && apt-get install -y build-essential g++ libtinfo6 zlib1g-dev curl libpq-dev postgresql-client

# Set working directory
WORKDIR /app

# Copy only stack configuration files first (for caching dependencies)
COPY stack.yaml stack.yaml.lock package.yaml /app/

# Set up GHC and dependencies
RUN stack setup
RUN stack build --only-dependencies

# Copy the rest of the application files
COPY . /app

# Build the application
RUN stack build

# Expose the app's port
EXPOSE 8080

# Run the application
CMD ["stack", "exec", "kolaboro-be-exe"]
