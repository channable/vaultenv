load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "asn1-types",
  visibility = ["//visibility:public"],
  deps = [
    "@hackage_hourglass//:hourglass",
    "@hackage_memory//:memory",
  ],
  prebuilt_dependencies = [
    "bytestring",
    "base",
  ],
  srcs = [
    "Data/ASN1/BitArray.hs",
    "Data/ASN1/OID.hs",
    "Data/ASN1/Pretty.hs",
    "Data/ASN1/Types.hs",
    "Data/ASN1/Types/Lowlevel.hs",
    "Data/ASN1/Types/String.hs",
  ],
)
