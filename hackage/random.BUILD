load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "random",
  visibility = ["//visibility:public"],
  deps = [
  ],
  prebuilt_dependencies = [
    "base",
    "time",
  ],
  compiler_flags = ["-cpp"],
  srcs = [
    "System/Random.hs",
  ],
)
