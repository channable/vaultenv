load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "scientific",
  visibility = ["//visibility:public"],
  deps = [
    "@hackage_integer_logarithms//:integer-logarithms",
    "@hackage_text//:text",
    "@hackage_hashable//:hashable",
    "@hackage_primitive//:primitive",
  ],
  prebuilt_dependencies = [
    "bytestring",
    "base",
    "integer-gmp",
    "containers",
    "binary",
    "deepseq",
  ],
  compiler_flags = [
    "-XDeriveDataTypeable",
    "-XBangPatterns"
  ],
  srcs = [
    "src/Data/ByteString/Builder/Scientific.hs",
    "src/Data/Scientific.hs",
    "src/Data/Text/Lazy/Builder/Scientific.hs",
    "src/GHC/Integer/Compat.hs",
    "src/Utils.hs",
  ],
  src_strip_prefix = "src",
)
