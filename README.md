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

The codelabs only require ghc to be installed and in the path. Please read
Codelab.hs and follow the instructions!

### Recordings

Public recordings of the training are available: [Haskell
101](http://youtu.be/cTN1Qar4HSw), [Haskell
102](http://youtu.be/Ug9yJnOYR4U).

### Release

See the [releases](https://github.com/google/haskell-trainings/releases) to
download the generated PDFs and the codelabs.

# Warning

This code is presented as-is, without the speaker notes. This is not an
officially supported Google product.
