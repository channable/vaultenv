load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

cc_library(
  name = "cbits",
  includes = ["include"],
  hdrs = ["include/inlinable.h"],
)

haskell_library(
  name = "lifted-base",
  visibility = ["//visibility:public"],
  deps = [
    ":cbits",
    "@hackage_monad_control//:monad-control",
    "@hackage_transformers_base//:transformers-base",
  ],
  prebuilt_dependencies = [
    "base",
  ],
  srcs = [
    "Data/IORef/Lifted.hs",
    "Foreign/Marshal/Utils/Lifted.hs",
    "System/Timeout/Lifted.hs",
    "Control/Concurrent/Lifted.hs",
    "Control/Concurrent/Chan/Lifted.hs",
    "Control/Concurrent/QSem/Lifted.hs",
    "Control/Concurrent/MVar/Lifted.hs",
    "Control/Concurrent/QSemN/Lifted.hs",
    "Control/Exception/Lifted.hs",
  ],
)
