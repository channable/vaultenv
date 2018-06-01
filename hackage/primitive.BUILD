load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

cc_library(
  name = "cbits",
  includes = ["cbits"],
  deps = ["@ghc//:threaded-rts"],
  srcs = ["cbits/primitive-memops.c"],
  hdrs = ["cbits/primitive-memops.h"],
)

haskell_library(
  name = "primitive",
  visibility = ["//visibility:public"],
  deps = [":cbits"],
  prebuilt_dependencies = [
    "base",
    "ghc-prim",
    "transformers",
  ],
  srcs = [
    "Data/Primitive.hs",
    "Data/Primitive/Array.hs",
    "Data/Primitive/MutVar.hs",
    "Data/Primitive/Addr.hs",
    "Data/Primitive/UnliftedArray.hs",
    "Data/Primitive/Types.hs",
    "Data/Primitive/ByteArray.hs",
    "Data/Primitive/SmallArray.hs",
    "Data/Primitive/MachDeps.hs",
    "Data/Primitive/Internal/Compat.hs",
    "Data/Primitive/Internal/Operations.hs",
    "Control/Monad/Primitive.hs",
  ],
)
