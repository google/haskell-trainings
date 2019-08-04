# Using haskell:8 instead of latest due to deprecation warnings
# on cabal
FROM haskell:8
WORKDIR /codelab

# Copy all source files into container
COPY . .

# Build codelab binary
RUN make codelab
