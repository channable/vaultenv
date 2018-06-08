load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "case-insensitive",
  visibility = ["//visibility:public"],
  deps = [
    "@hackage_text//:lazy",
    "@hackage_text//:text",
    "@hackage_hashable//:hashable",
  ],
  prebuilt_dependencies = [
    "bytestring",
    "base",
    "deepseq",
  ],
  srcs = [
    "Data/CaseInsensitive.hs",
    "Data/CaseInsensitive/Internal.hs",
    "Data/CaseInsensitive/Unsafe.hs",
  ],
)
