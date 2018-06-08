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
  deps = [
    ":vaultenv_config",
    "@hackage_async//:async",
    "@hackage_connection//:connection",
    "@hackage_http_client//:http-client",
    "@hackage_http_conduit//:conduit",
    "@hackage_http_conduit//:http-conduit",
    "@hackage_lens//:control_lens",
    "@hackage_lens//:map_set",
    "@hackage_lens_aeson//:lens-aeson",
    "@hackage_mtl//:mtl",
    "@hackage_retry//:retry",
    "@hackage_text//:text",
  ],
  prebuilt_dependencies = [
    "base",
    "bytestring",
    "containers",
  ],
  srcs = ["app/Main.hs"],
  src_strip_prefix = "app",
)
