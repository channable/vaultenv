load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "x509-validation",
  visibility = ["//visibility:public"],
  deps = [
    "@hackage_hourglass//:hourglass",
    "@hackage_data_default_class//:data-default-class",
    "@hackage_pem//:pem",
    "@hackage_asn1_types//:asn1-types",
    "@hackage_x509_store//:x509-store",
    "@hackage_memory//:memory",
    "@hackage_x509//:x509",
    "@hackage_cryptonite//:hash",
    "@hackage_cryptonite//:pubkey_hash",
    "@hackage_cryptonite//:pubkey_nohash",
    "@hackage_mtl//:mtl",
    "@hackage_byteable//:byteable",
    "@hackage_asn1_encoding//:asn1-encoding",
  ],
  prebuilt_dependencies = [
    "bytestring",
    "base",
    "containers",
  ],
  srcs = [
    "Data/X509/Validation.hs",
    "Data/X509/Validation/Signature.hs",
    "Data/X509/Validation/Fingerprint.hs",
    "Data/X509/Validation/Cache.hs",
    "Data/X509/Validation/Types.hs",
  ],
)
