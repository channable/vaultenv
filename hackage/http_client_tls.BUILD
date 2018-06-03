load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "http-client-tls",
  visibility = ["//visibility:public"],
  deps = [
    "@hackage_http_client//:http-client",
    "@hackage_exceptions//:exceptions",
    "@hackage_case_insensitive//:case-insensitive",
    "@hackage_data_default_class//:data-default-class",
    "@hackage_text//:text",
    "@hackage_tls//:tls",
    "@hackage_network//:network",
    "@hackage_connection//:connection",
    "@hackage_memory//:memory",
    "@hackage_cryptonite//:hash",
    "@hackage_network_uri//:network-uri",
    "@hackage_http_types//:http-types",
  ],
  prebuilt_dependencies = [
    "bytestring",
    "base",
    "containers",
    "transformers",
  ],
  srcs = [
    "Network/HTTP/Client/TLS.hs",
  ],
)
