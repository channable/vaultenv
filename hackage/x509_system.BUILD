load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "x509-system",
  visibility = ["//visibility:public"],
  deps = [
    "@hackage_pem//:pem",
    "@hackage_x509_store//:x509-store",
    "@hackage_x509//:x509",
    "@hackage_mtl//:mtl",
  ],
  prebuilt_dependencies = [
    "bytestring",
    "base",
    "filepath",
    "process",
    "containers",
    "directory",
  ],
  srcs = [
    "System/X509.hs",
    "System/X509/Unix.hs",
    "System/X509/MacOS.hs",
  ],
)

