# Using haskell:8 instead of latest due to deprecation warning
# on cabal
FROM haskell:8

# Install codelab dependencies
RUN cabal update && cabal install random
WORKDIR /codelab

# Copy all source files into container
COPY . .

# Build all binaries
RUN make
