# Haskell 102 Codelab

## Setup

To setup your machine please consult [the official haskell website](https://www.haskell.org/platform/). We have also provided a docker container that can be used to run the test suite and game. This method for running the tests/game requires [Docker](https://www.docker.com/get-started) and [Docker compose](https://docs.docker.com/compose/install/) but avoids the need to set up haskell on your machine.

## Running tests

To run the tests just run `make` in each of the directories. The special `00_setup` directory is only used for testing the environment, but `01_mastermind` contains the full game (with multiple exercises).

There are 5 separate exercises identified by a section number and a corresponding source file. For each of the exercises, running `make ARGS="check <number>"` (eg., `make ARGS="check 1") would display the file that needs to change:
```
Checking code from src/Color.hs
...
```

In the file, replace `codelab` with your implementation. When complete, running `make ARGS="check 1"` again would result in an output similar to below:
```
Checking code from src/Color.hs
allColors contains Red                    OK got: True
allColors contains Yellow                 OK got: True
allColors contains Green                  OK got: True
allColors contains Cyan                   OK got: True
allColors contains Blue                   OK got: True
allColors contains Magenta                OK got: True
allColors size is 6                       OK got: 6
show Red                                  OK got: "R"
concatMap show allColors                  OK got: "RYGCBM"
allColors starts with Red                 OK got: R
allColors ends with Magenta               OK got: M
```

If you are using the docker container, build and run the tests run with `docker-compose run`. To test the setup, run `docker-compose run setup`. To test each section, run `docker-compose run section_<number>`. For example, for `docker-compose run section_1`, you should see output similar to below:
```
Creating network "codelab_default" with the default driver
Building section_1
Step 1/3 : FROM haskell:8
8: Pulling from library/haskell
9b99af5931b3: Pull complete
580a548160a1: Pull complete
5d8e6deeb485: Pull complete
70b0645032d3: Pull complete
03b69c8eaa80: Pull complete
Digest: sha256:d26c7a6853190096137361cfe65f5b7fc6e69ec4660ffb9583ec323e85b524e6
Status: Downloaded newer image for haskell:8
 ---> 3baac1927856
Step 2/3 : WORKDIR .
 ---> Running in 7b9c08684455
Removing intermediate container 7b9c08684455
 ---> bd1bd24a236d
Step 3/3 : COPY . .
 ---> 5012253dbb4f

Successfully built 5012253dbb4f
Successfully tagged haskell101:latest
Creating codelab_section_1_run ... done
make: Entering directory '/01_mastermind'
...
Progress 7/8: codelab
Checking code from src/Color.hs
allColors contains Red                    OK got: True
allColors contains Yellow                 OK got: True
allColors contains Green                  OK got: True
allColors contains Cyan                   OK got: True
allColors contains Blue                   OK got: True
allColors contains Magenta                OK got: True
allColors size is 6                       OK got: 6
show Red                                  OK got: "R"
concatMap show allColors                  OK got: "RYGCBM"
allColors starts with Red                 OK got: R
allColors ends with Magenta               OK got: M
make: Leaving directory '/01_mastermind'
```

Running the first time would result in downloading and compiling needed dependencies (omitted from the above screen output).

When done, you can cleanup Docker services with `docker-compose down`.

## Playing the game

Once all the five sections have been completed, you can play the game using either `make ARGS=play` or `docker-compose run play`. You can also let an AI solve the game with either `make ARGS=solve` or `docker-compose run solve`. In this case, you should see an output similar to below:
```
Valid colors: [R,Y,G,C,B,M]
Size of the answer: 4
Number of tries: 8
Good luck!
[R,R,R,R] => black: 3, white: 0
[R,R,R,Y] => black: 3, white: 0
[R,R,R,G] => black: 3, white: 0
[R,R,R,C] => well done!
```
