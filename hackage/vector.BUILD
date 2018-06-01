load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

cc_library(
  name = "cbits",
  includes = ["include", "internal"],
  hdrs = [
    "include/vector.h",
    "internal/unbox-tuple-instances",
  ],
)

haskell_library(
  name = "vector",
  visibility = ["//visibility:public"],
  deps = [
    ":cbits",
    "@hackage_primitive//:primitive",
  ],
  prebuilt_dependencies = [
    "base",
    "ghc-prim",
    "deepseq",
  ],
  compiler_flags = [
    "-cpp",
    "-XBangPatterns",
    "-XDeriveDataTypeable",
    "-XExistentialQuantification",
    "-XFlexibleContexts",
    "-XGADTs",
    "-XKindSignatures",
    "-XMagicHash",
    "-XMultiParamTypeClasses",
    "-XRank2Types",
    "-XScopedTypeVariables",
    "-XStandaloneDeriving",
    "-XTypeFamilies",
  ],
  srcs = [
    "Data/Vector.hs",
    "Data/Vector/Unboxed.hs",
    "Data/Vector/Generic.hs",
    "Data/Vector/Primitive.hs",
    "Data/Vector/Storable.hs",
    "Data/Vector/Mutable.hs",
    "Data/Vector/Storable/Internal.hs",
    "Data/Vector/Storable/Mutable.hs",
    "Data/Vector/Fusion/Bundle.hs",
    "Data/Vector/Fusion/Util.hs",
    "Data/Vector/Fusion/Bundle/Monadic.hs",
    "Data/Vector/Fusion/Bundle/Size.hs",
    "Data/Vector/Fusion/Stream/Monadic.hs",
    "Data/Vector/Generic/New.hs",
    "Data/Vector/Generic/Base.hs",
    "Data/Vector/Generic/Mutable.hs",
    "Data/Vector/Generic/Mutable/Base.hs",
    "Data/Vector/Internal/Check.hs",
    "Data/Vector/Unboxed/Base.hs",
    "Data/Vector/Unboxed/Mutable.hs",
    "Data/Vector/Primitive/Mutable.hs",
  ],
)
