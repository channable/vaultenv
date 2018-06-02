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
  deps = [
    "@hackage_optparse_applicative//:optparse-applicative",
  ],
  prebuilt_dependencies = [
    "base",
    "bytestring",
  ],
  srcs = ["src/Config.hs"],
  src_strip_prefix = "src",
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
