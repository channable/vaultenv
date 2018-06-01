load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "network-uri",
  visibility = ["//visibility:public"],
  deps = [
    "@hackage_parsec//:parsec",
  ],
  prebuilt_dependencies = [
    "base",
    "deepseq",
  ],
  compiler_flags = [
    "-cpp",
    "-XDeriveDataTypeable",
    "-XDeriveGeneric",
  ],
  srcs = [
    "Network/URI.hs",
  ],
)
