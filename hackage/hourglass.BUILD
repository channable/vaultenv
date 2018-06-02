load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

cc_library(
  name = "cbits",
  includes = ["include"],
  srcs = ["cbits/unix.c"],
)

haskell_library(
  name = "hourglass",
  visibility = ["//visibility:public"],
  deps = [
    ":cbits",
  ],
  prebuilt_dependencies = [
    "base",
    "deepseq",
  ],
  srcs = [
    "Time/Types.hs",
    "Time/System.hs",
    "Time/Compat.hs",
    "Data/Hourglass.hs",
    "Data/Hourglass/Types.hs",
    "Data/Hourglass/Epoch.hs",
    "Data/Hourglass/Compat.hs",
    "System/Hourglass.hs",
    "Data/Hourglass/Time.hs",
    "Data/Hourglass/Format.hs",
    "Data/Hourglass/Diff.hs",
    "Data/Hourglass/Local.hs",
    "Data/Hourglass/Calendar.hs",
    "Data/Hourglass/Zone.hs",
    "Data/Hourglass/Internal.hs",
    "Data/Hourglass/Utils.hs",
    "Data/Hourglass/Internal/Unix.hs",
  ],
)

