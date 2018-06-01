load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

cc_library(
  name = "cbits",
  includes = ["."],
  hdrs = ["fusion-macros.h"],
)

haskell_library(
  name = "conduit",
  visibility = ["//visibility:public"],
  deps = [
    ":cbits",
    "@hackage_exceptions//:exceptions",
    "@hackage_monad_control//:monad-control",
    "@hackage_lifted_base//:lifted-base",
    "@hackage_mtl//:mtl",
    "@hackage_mmorph//:mmorph",
    "@hackage_transformers_base//:transformers-base",
    "@hackage_resourcet//:resourcet",
    "@hackage_primitive//:primitive",
    "@hackage_transformers_compat//:transformers-compat",
  ],
  prebuilt_dependencies = [
    "base",
    "transformers",
  ],
  srcs = [
    "Data/Conduit.hs",
    "Data/Conduit/List.hs",
    "Data/Conduit/Internal.hs",
    "Data/Conduit/Lift.hs",
    "Data/Conduit/Internal/Fusion.hs",
    "Data/Conduit/Internal/List/Stream.hs",
    "Data/Conduit/Internal/Pipe.hs",
    "Data/Conduit/Internal/Conduit.hs",
  ],
)
