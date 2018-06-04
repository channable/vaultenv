load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "connection",
  visibility = ["//visibility:public"],
  deps = [
    "@hackage_byteable//:byteable",
    "@hackage_data_default_class//:data-default-class",
    "@hackage_network//:network",
    "@hackage_socks//:socks",
    "@hackage_tls//:wire",
    "@hackage_tls//:tls",
    "@hackage_x509_store//:x509-store",
    "@hackage_x509_system//:x509-system",
  ],
  srcs = [
    "Network/Connection.hs",
    "Network/Connection/Types.hs",
  ],
)

