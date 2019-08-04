# Haskell 102 Codelab

## Setup

To setup your machine please consult [the official haskell website](https://www.haskell.org/platform/). We have also provided a docker container that can be used to run the test suite and game. This method for running the tests/game requires [Docker](https://www.docker.com/get-started) and [Docker compose](https://docs.docker.com/compose/install/) but avoids the need to set up haskell on your machine.

## Running tests

To run the tests first build the binaries with `make` (or `docker-compose build`) and then run the binary `./test_codelab` (or `docker-compose run codelab_test`) and you should see output similar to below as you progress through the lab.
```
#### Section 1
[1.1] allColors contains Red              [OK] got: True
[1.1] allColors contains Yellow           [OK] got: True
```

## Playing the game

Similar to above, to play the game you must first build the binaries with `make` (or `docker-compose build`), and then run the binary `./codelab <play/solve>` (or `docker-compose run codelab_<play/solve>`) and you should see output similar to below.

```
Valid colors: [R,Y,G,C,B,M]
Size of the answer: 4
Number of tries: 8
Good luck!
Turn 1:
```
