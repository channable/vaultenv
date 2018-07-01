load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "core",
  visibility = ["//visibility:public"],
  deps = [
    "@hackage_hourglass//:hourglass",
    "@hackage_asn1_types//:asn1-types",
    "@hackage_mtl//:mtl",
    "@hackage_asn1_parse//:asn1-parse",
    "@hackage_asn1_encoding//:asn1-encoding",
  ],
  prebuilt_dependencies = [
    "base",
    "bytestring",
  ],
  compiler_flags = ["-XDatatypeContexts"],
  srcs = [
    "Data/X509/Internal.hs",
    "Data/X509/AlgorithmIdentifier.hs",
    "Data/X509/DistinguishedName.hs",
    "Data/X509/Ext.hs",
    "Data/X509/ExtensionRaw.hs",
    "Data/X509/CRL.hs",
    "Data/X509/Signed.hs",
  ],
)

haskell_library(
  name = "oid",
  visibility = ["//visibility:public"],
  deps = [
    "@hackage_asn1_types//:asn1-types",
    "@hackage_cryptonite//:pubkey_nohash",
  ],
  prebuilt_dependencies = ["base"],
  srcs = [
    "Data/X509/OID.hs",
  ],
)

haskell_library(
  name = "x509",
  visibility = ["//visibility:public"],
  deps = [
    ":oid",
    ":core",
    "@hackage_hourglass//:hourglass",
    "@hackage_asn1_types//:asn1-types",
    "@hackage_memory//:byte_array",
    "@hackage_cryptonite//:number",
    "@hackage_cryptonite//:pubkey_nohash",
    "@hackage_cryptonite//:pubkey_hash",
    "@hackage_cryptonite//:hash",
    "@hackage_asn1_encoding//:asn1-encoding",
  ],
  prebuilt_dependencies = [
    "base",
    "bytestring",
  ],
  srcs = [
    "Data/X509.hs",
    "Data/X509/CertificateChain.hs",
    "Data/X509/EC.hs",
    "Data/X509/PrivateKey.hs",
    "Data/X509/Cert.hs",
    "Data/X509/PublicKey.hs",
  ],
)
