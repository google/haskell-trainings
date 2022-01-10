# Using haskell:8 instead of latest due to deprecation warnings
# on cabal
FROM registry.gitlab.com/supercash/core/haskell-stack-base:stack-2.7.3_haskell-8.10.7_13057207-master

WORKDIR /google/trainings/haskell

# Copy all source files into container
COPY . .

RUN ls -d */ | awk '{ print $1 }' | xargs -I {} cp $(pwd)/Makefile $(pwd)/{}
RUN ls -d */ | awk '{ print $1 }' | xargs -I {} make -C {} deps
