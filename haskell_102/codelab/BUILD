load("//tools/build_defs/haskell:def.bzl", "haskell_binary", "haskell_test")

haskell_binary(
    name = "codelab",
    srcs = [
        "Codelab.hs",
        "Game.hs",
        "Main.hs",
    ],
    ghcopts = [
        "-DCODELAB",
        "-Wwarn",
    ],
    deps = [
        "//third_party/haskell/random",
    ],
)

haskell_test(
    name = "test_codelab",
    srcs = [
        "Codelab.hs",
        "Game.hs",
        "Tests.hs",
    ],
    ghcopts = [
        "-DCODELAB",
        "-Wwarn",
    ],
    deps = [
        "//third_party/haskell/random",
    ],
)

haskell_binary(
    name = "solution",
    srcs = [
        "Game.hs",
        "Main.hs",
        "Solution.hs",
    ],
    ghcopts = [
        "-DSOLUTION",
        "-Wwarn",
    ],
    deps = [
        "//third_party/haskell/random",
    ],
)

haskell_test(
    name = "test_solution",
    srcs = [
        "Game.hs",
        "Solution.hs",
        "Tests.hs",
    ],
    ghcopts = [
        "-DSOLUTION",
        "-Wwarn",
    ],
    deps = [
        "//third_party/haskell/random",
    ],
)
