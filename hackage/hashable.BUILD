load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

cc_library(
  name = "cbits",
  srcs = ["cbits/fnv.c"],
)

haskell_library(
  name = "hashable",
  visibility = ["//visibility:public"],
  deps = [
    ":cbits",
    "@hackage_text//:lazy",
    "@hackage_text//:text",
  ],
  prebuilt_dependencies = [
    "bytestring",
    "base",
    "integer-gmp",
    "ghc-prim",
    "deepseq",
  ],
  compiler_flags = [
    "-DGENERICS",
    "-DHAVE_MMAP",
  ],
  srcs = [
    "Data/Hashable.hs",
    "Data/Hashable/Generic.hs",
    "Data/Hashable/Class.hs",
    "Data/Hashable/Lifted.hs",
    "Data/Hashable/SipHash.hs",
  ],
)
