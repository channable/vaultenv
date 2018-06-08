load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

cc_library(
  name = "cbits",
  includes = ["include"],
  srcs = ["cbits/cbits.c"],
  hdrs = ["include/text_cbits.h"],
)

haskell_library(
  name = "text",
  visibility = ["//visibility:public"],
  deps = [
    ":cbits",
  ],
  prebuilt_dependencies = [
    "bytestring",
    "base",
    "array",
    "integer-gmp",
    "binary",
    "ghc-prim",
    "deepseq",
  ],
  compiler_flags = [
    "-DHAVE_DEEPSEQ",
    "-DINTEGER_GMP",
  ],
  srcs = [
    "Data/Text.hs",
    "Data/Text/Array.hs",
    "Data/Text/Encoding.hs",
    "Data/Text/Encoding/Error.hs",
    "Data/Text/Internal.hs",
    "Data/Text/Internal/Encoding/Fusion.hs",
    "Data/Text/Internal/Encoding/Fusion/Common.hs",
    "Data/Text/Internal/Encoding/Utf16.hs",
    "Data/Text/Internal/Encoding/Utf32.hs",
    "Data/Text/Internal/Encoding/Utf8.hs",
    "Data/Text/Internal/Functions.hs",
    "Data/Text/Internal/Fusion.hs",
    "Data/Text/Internal/Fusion/CaseMapping.hs",
    "Data/Text/Internal/Fusion/Common.hs",
    "Data/Text/Internal/Fusion/Size.hs",
    "Data/Text/Internal/Fusion/Types.hs",
    "Data/Text/Internal/Private.hs",
    "Data/Text/Internal/Search.hs",
    "Data/Text/Internal/Unsafe.hs",
    "Data/Text/Internal/Unsafe/Char.hs",
    "Data/Text/Internal/Unsafe/Shift.hs",
    "Data/Text/Show.hs",
    "Data/Text/Unsafe.hs",
  ],
)

haskell_library(
  name = "lazy",
  visibility = ["//visibility:public"],
  deps = [
    ":text",
    ":cbits",
  ],
  prebuilt_dependencies = [
    "bytestring",
    "base",
    "array",
    "integer-gmp",
    "binary",
    "ghc-prim",
    "deepseq",
  ],
  compiler_flags = [
    "-DHAVE_DEEPSEQ",
    "-DINTEGER_GMP",
  ],
  srcs = [
    "Data/Text/Internal/Lazy.hs",
    "Data/Text/Internal/Lazy/Encoding/Fusion.hs",
    "Data/Text/Internal/Lazy/Fusion.hs",
    "Data/Text/Internal/Lazy/Search.hs",
    "Data/Text/Lazy.hs",
    "Data/Text/Lazy/Encoding.hs",
    "Data/Text/Lazy/Internal.hs",
  ],
)

haskell_library(
  name = "builder",
  visibility = ["//visibility:public"],
  deps = [
    ":text",
    ":lazy",
    ":cbits",
  ],
  prebuilt_dependencies = [
    "bytestring",
    "base",
    "array",
    "integer-gmp",
    "binary",
    "ghc-prim",
    "deepseq",
  ],
  compiler_flags = [
    "-DHAVE_DEEPSEQ",
    "-DINTEGER_GMP",
  ],
  srcs = [
    "Data/Text/Lazy/Builder.hs",
    "Data/Text/Lazy/Builder/Int.hs",
    "Data/Text/Lazy/Builder/RealFloat.hs",
    "Data/Text/Internal/Builder.hs",
    "Data/Text/Internal/Builder/Functions.hs",

    # Note: these two could be split into a separate target without dependency
    # on "lazy". Not sure if that is worth it though.
    "Data/Text/Internal/Builder/Int/Digits.hs",
    "Data/Text/Internal/Builder/RealFloat/Functions.hs",
  ],
)

haskell_library(
  name = "io",
  visibility = ["//visibility:public"],
  deps = [
    ":text",
    ":cbits",
  ],
  prebuilt_dependencies = [
    "bytestring",
    "base",
    "array",
    "integer-gmp",
    "binary",
    "ghc-prim",
    "deepseq",
  ],
  compiler_flags = [
    "-DHAVE_DEEPSEQ",
    "-DINTEGER_GMP",
  ],
  srcs = [
    "Data/Text/Foreign.hs",
    "Data/Text/IO.hs",
    "Data/Text/Internal/IO.hs",
    "Data/Text/Internal/Read.hs",
    "Data/Text/Read.hs",
  ],
)

haskell_library(
  name = "lazy_io",
  visibility = ["//visibility:public"],
  deps = [
    ":text",
    ":lazy",
    ":io",
    ":cbits",
  ],
  prebuilt_dependencies = [
    "bytestring",
    "base",
    "array",
    "integer-gmp",
    "binary",
    "ghc-prim",
    "deepseq",
  ],
  compiler_flags = [
    "-DHAVE_DEEPSEQ",
    "-DINTEGER_GMP",
  ],
  srcs = [
    "Data/Text/Lazy/IO.hs",
    "Data/Text/Lazy/Read.hs",
  ],
)
