# syntax = docker/dockerfile:1.2

# Using haskell:8 instead of latest due to deprecation warnings
# on cabal
FROM alpine AS dependencies

# https://stackoverflow.com/questions/49939960/docker-copy-files-using-glob-pattern/66137816#66137816
WORKDIR /google/trainings/haskell/cabal
RUN --mount=type=bind,target=/docker-context \
    cd /docker-context/; \
    #find . -name "package.json" -mindepth 0 -maxdepth 4 -exec cp --parents "{}" /app/ \;
    # https://unix.stackexchange.com/questions/15308/how-to-use-find-command-to-search-for-multiple-extensions/15309#15309
    find . -type f \( -iname \*.cabal -o -iname \*.yaml\* \) -mindepth 0 -maxdepth 2 \
         -exec cp --parents "{}" /google/trainings/haskell/cabal/ \;

FROM haskell:8.10.7

# Add vim to enable users to edit files locally while running within a container
RUN apt-get update && \
     apt-get install -y vim

WORKDIR /google/trainings/haskell-101

# Copy all cabal dependencies files only, fetched from docker-context
COPY --from=dependencies /google/trainings/haskell/cabal .

# Copy also the Makefile with the dependency task
COPY Makefile .

RUN ls -d */ | awk '{ print $1 }' | xargs -I {} cp $(pwd)/Makefile $(pwd)/{}
RUN ls -d */ | awk '{ print $1 }' | xargs -I {} make -C {} deps

COPY . .
