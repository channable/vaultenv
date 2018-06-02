load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "reflection",
  visibility = ["//visibility:public"],
  prebuilt_dependencies = [
    "base",
    "template-haskell",
  ],
  srcs = ["fast/Data/Reflection.hs"],
  src_strip_prefix = "fast",
)
