<!-- -*- fill-column: 75; -*- -->

# Haskell training: 101 and 102.

This repository contains the source for the slides and the exercises used
in the Haskell trainings at Google. Haskell is not one of the internally
"blessed" languages, but a dedicated team of volunteers is making use of
20% time to try to make Haskell at Google possible! This set of lessons was
created in 2016, to allow newcomers to the language to get involved with
the team.

### Slides

The slides use LaTeX. Simply run `make` to create the PDFs, assuming you have
all the required dependencies.

You need `xelatex` and the [Yanone
Kaffeesatz](https://yanone.de/fonts/kaffeesatz/) fonts installed on your
machine.

On an Ubuntu system, you can get all of these by running

```
sudo apt-get install texlive-xetex texlive-fonts-extra fonts-yanone-kaffeesatz
```

### Exercises

The codelabs only require ghc to be installed and in the path as well as one of
Stack or Cabal. Step-by-step instructions will be provided to guide you through
the installation process, at the point where these tools will be necessary. 
In case you already have them installed, running `make` in each codelab
directory will identify which of the tool is installed and use that to build
the exercises as well as run the test.

There is also a possiblity to run the codelabs using Docker. See the instructions
in the codelab directories.

To solve an exercise, you need to replace `codelab` with the actual
implementation. For the 101 course, each different codelab directory (except
`00_setup` which is only used to test installation) has a `src/Codelab.hs` where
you would need to do the exercises. For the 102 course, all the exercises are in
the same directory in multiple files, given that we are building a full project.
Consult the slides for instructions on how to run each codelab.

We also provide solutions, in the same directory as the corresponding exercise.

### Recordings

Public recordings of the training (in an old format) are available: [Haskell
101](http://youtu.be/cTN1Qar4HSw), [Haskell
102](http://youtu.be/Ug9yJnOYR4U).

### Release

See the [releases](https://github.com/google/haskell-trainings/releases) to
download the generated PDFs and the codelabs.

# Warning

This code is presented as-is, without the speaker notes. This is not an
officially supported Google product.
