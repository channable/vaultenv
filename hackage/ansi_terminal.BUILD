load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "ansi-terminal",
  visibility = ["//visibility:public"],
  hdrs = [
    "includes/Common-Include.hs",
    "includes/Common-Include-Emulator.hs",
    "includes/Common-Include-Enabled.hs",
    "includes/Exports-Include.hs",
  ],
  srcs = [
    "System/Console/ANSI.hs",
    "System/Console/ANSI/Types.hs",
    "System/Console/ANSI/Codes.hs",
    "System/Console/ANSI/Unix.hs",
  ],
  deps = [
    "@hackage_colour//:colour",
  ],
  prebuilt_dependencies = [
    "base",
  ],
  compiler_flags = ["-cpp", "-DUNIX"],
)
