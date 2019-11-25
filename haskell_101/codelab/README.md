# Haskell 101 Codelab

## Setup

To setup your machine please consult [the official haskell website](https://www.haskell.org/platform/). We have also provided a docker container that can be used to run the test suite. This method for running the tests requires [Docker](https://www.docker.com/get-started) and [Docker compose](https://docs.docker.com/compose/install/) but avoids the need to set up haskell on your machine.

## Running tests

To run the tests first build the binary `make codelab` and then run the binary `./codelab` and you should see output similar to below as you progress through the lab.
```
### Section 1
add       1 2                             [OK] got: 3
subtract  7 2                             [OK] got: 5
```

If you are using the docker container, build and run the tests run with `docker-compose up --build` and you should see output similar to below:

```
Building haskell101
Step 1/4 : FROM haskell:latest
 ---> 447ebe786704
Step 2/4 : WORKDIR /codelab
 ---> Using cache
 ---> 68fdd5afea08
Step 3/4 : COPY . .
 ---> Using cache
 ---> 139ffbe127f2
Step 4/4 : RUN make codelab
 ---> Using cache
 ---> 56df8c4a8ab5
Successfully built 56df8c4a8ab5
Successfully tagged haskell101:latest
Recreating codelab_haskell101_1 ... done
Attaching to codelab_haskell101_1
haskell101_1  | #### Section 1
haskell101_1  | add       1 2                             [OK] got: 3
haskell101_1  | subtract  7 2                             [OK] got: 5
```
