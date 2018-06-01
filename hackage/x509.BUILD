load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "x509",
  visibility = ["//visibility:public"],
  deps = [
    "@hackage_hourglass//:hourglass",
    "@hackage_pem//:pem",
    "@hackage_asn1_types//:asn1-types",
    "@hackage_memory//:memory",
    "@hackage_cryptonite//:cryptonite",
    "@hackage_mtl//:mtl",
    "@hackage_asn1_parse//:asn1-parse",
    "@hackage_asn1_encoding//:asn1-encoding",
  ],
  prebuilt_dependencies = [
    "bytestring",
    "base",
    "containers",
  ],
  compiler_flags = ["-XDatatypeContexts"],
  srcs = [
    "Data/X509.hs",
    "Data/X509/EC.hs",
    "Data/X509/Internal.hs",
    "Data/X509/CertificateChain.hs",
    "Data/X509/AlgorithmIdentifier.hs",
    "Data/X509/DistinguishedName.hs",
    "Data/X509/Cert.hs",
    "Data/X509/PublicKey.hs",
    "Data/X509/PrivateKey.hs",
    "Data/X509/Ext.hs",
    "Data/X509/ExtensionRaw.hs",
    "Data/X509/CRL.hs",
    "Data/X509/OID.hs",
    "Data/X509/Signed.hs",
  ],
)
