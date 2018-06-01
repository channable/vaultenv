load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_binary",
  "haskell_library",
  "haskell_toolchain",
)

haskell_toolchain(
  name = "ghc",
  version = "8.2.2",
  tools = "@ghc//:bin",
)

haskell_library(
  name = "vaultenv_config",
  srcs = ["src/Config.hs"],
  deps = [
    "@hackage_optparse_applicative",
  ],
  prebuilt_dependencies = [
    "base",
    "bytestring",
  ],
)

haskell_binary(
  name = "vaultenv",
  srcs = ["app/Main.hs"],
  deps = [":vaultenv_config"],
  prebuilt_dependencies = [
    "base",
    "bytestring"
  ],
)
