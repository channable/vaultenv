load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "x509-store",
  visibility = ["//visibility:public"],
  deps = [
    "@hackage_pem//:pem",
    "@hackage_asn1_types//:asn1-types",
    "@hackage_x509//:x509",
    "@hackage_cryptonite//:number",
    "@hackage_cryptonite//:pubkey_hash",
    "@hackage_cryptonite//:pubkey_nohash",
    "@hackage_mtl//:mtl",
    "@hackage_asn1_encoding//:asn1-encoding",
  ],
  prebuilt_dependencies = [
    "bytestring",
    "base",
    "filepath",
    "containers",
    "directory",
  ],
  srcs = [
    "Data/X509/CertificateStore.hs",
    "Data/X509/File.hs",
    "Data/X509/Memory.hs",
  ],
)
