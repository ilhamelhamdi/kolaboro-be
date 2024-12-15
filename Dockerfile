FROM ubuntu:22.04

# Install system dependencies
RUN apt-get update && apt-get install -y build-essential g++ libtinfo6 zlib1g-dev curl

# Install Haskell Stack
RUN curl -sSL https://get.haskellstack.org/ | sh

# Set working directory
WORKDIR /app

# Copy project files
COPY . .

# Set up GHC and dependencies
RUN stack setup
RUN stack build

# Expose the app's port
EXPOSE 8080

# Run the application
CMD ["stack", "exec", "kolaboro-be-exe"]
