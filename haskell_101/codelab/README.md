# Haskell 101 Codelab

## Setup

To setup your machine please consult [the official haskell website](https://www.haskell.org/platform/). We have also provided a docker container that can be used to run the test suite. This method for running the tests requires [Docker](https://www.docker.com/get-started) and [Docker compose](https://docs.docker.com/compose/install/) but avoids the need to set up haskell on your machine.

## Running tests

To run the tests just run `make` in each of the directories. The special `00_setup` directory is only used for testing the environment, but all others contain exercises to resolve.

### From local haskell

> **NOTE**: You must make sure your setup is working locally.

For each of the exercises, replace `codelab` with your implementation. When complete, running `make` would result in an output similar to below:

```console
add       1 2                             OK got: 3
subtract  7 2                             OK got: 5
double    3                               OK got: 6
multiply  3 11                            OK got: 33
divide    9 2                             OK got: 4.5
divide    8 4                             OK got: 2.0
factorial 30                              OK got: 265252859812191058636308480000000
gcd       12 4                            OK got: 4
gcd       17 7                            OK got: 1
```

## Using a docker container

If you are using the docker, build and run the tests run with `docker-compose up _EXERCISE_NAME_`. For example, for `docker-compose run 01-functions` runs the exercise `01-functions`, defined as a docker-compose service `01-functions` you should see output similar to below:

> **NOTE**: the following output is longer the first time of the execution as the docker image is first created before docker-compose spawns a new docker container to execute the tests.

```console
$ docker-compose up 01_functions
Starting codelab_01_functions_1 ... done
Attaching to codelab_01_functions_1
01_functions_1     | make: Entering directory '/google/trainings/haskell-101/01_functions'
01_functions_1     | Completed 3 action(s).
01_functions_1     | add       1 2                             OK got: 3
01_functions_1     | subtract  7 2                             OK got: 5
01_functions_1     | double    3                               OK got: 6
01_functions_1     | multiply  3 11                            OK got: 33
01_functions_1     | divide    9 2                             OK got: 4.5
01_functions_1     | divide    8 4                             OK got: 2.0
01_functions_1     | factorial 30                              OK got: 265252859812191058636308480000000
01_functions_1     | gcd       12 4                            OK got: 4
01_functions_1     | gcd       17 7                            OK got: 1
01_functions_1     | make: Leaving directory '/google/trainings/haskell-101/01_functions'
codelab_01_functions_1 exited with code 0
```

## From within a Docker Container

* Running within a docker container means you have full access to haskell, stack, cabal, etc.

> **NOTE**: this requires an update of docker-compose.yaml for the current version.

```diff
   03_lists:
     <<: [ *generic-haskell-runtime ]
-    #<<: [ *docker-cli-mode ]
-    command: make -C 03_lists
+    <<: [ *docker-cli-mode ]
+    #command: make -C 03_lists
```

* Create a new container attached to the terminal

> **NOTE**: The built docker image has `vim`, which will allows you to edit files 
> from within the docker container and reflect when the container is terminated.

```console
$ docker-compose run 03_lists
Creating codelab_03_lists_run ... done
root@9bc17a3fe02a:/google/trainings/haskell-101# ghci --version
The Glorious Glasgow Haskell Compilation System, version 8.10.7
root@9bc17a3fe02a:/google/trainings/haskell-101# cd 03_lists/
root@9bc17a3fe02a:/google/trainings/haskell-101/03_lists# vim src/
Codelab.hs   Internal.hs  Main.hs      Solution.hs
root@9bc17a3fe02a:/google/trainings/haskell-101/03_lists# vim src/Codelab.hs
root@9bc17a3fe02a:/google/trainings/haskell-101/03_lists# stack run
codelab> configure (exe)
Configuring codelab-0.1.0.0...
...
...
```
