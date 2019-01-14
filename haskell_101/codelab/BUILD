load("//tools/build_defs/haskell:def.bzl", "haskell_test")

haskell_test(
    name = "codelab",
    srcs = [
        "Codelab.hs",
        "Main.hs",
    ],
    ghcopts = [
        "-DCODELAB",
        "-Wwarn",
    ],
)

haskell_test(
    name = "solution",
    srcs = [
        "Main.hs",
        "Solution.hs",
    ],
    ghcopts = [
        "-DSOLUTION",
        "-Wwarn",
    ],
)
