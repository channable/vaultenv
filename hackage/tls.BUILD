load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "tls_base",
  visibility = ["//visibility:public"],
  deps = [
    "@hackage_asn1_encoding//:asn1-encoding",
    "@hackage_asn1_types//:asn1-types",
    "@hackage_async//:async",
    "@hackage_mtl//:mtl",
    "@hackage_network//:network",
  ],
  compiler_flags = [
    "-DSSLV2_COMPATIBLE",
    "-DINCLUDE_NETWORK",
  ],
  srcs = [
    "Network/TLS/Backend.hs",
    "Network/TLS/Cap.hs",
    "Network/TLS/Compression.hs",
    "Network/TLS/Crypto/Types.hs",
    "Network/TLS/ErrT.hs",
    "Network/TLS/Measurement.hs",
    "Network/TLS/Session.hs",
    "Network/TLS/Types.hs",
    "Network/TLS/Util.hs",
    "Network/TLS/Util/ASN1.hs",
  ],
)

haskell_library(
  name = "imports",
  visibility = ["//visibility:public"],
  deps = [
    "@hackage_memory//:memory",
  ],
  compiler_flags = [
    "-DSSLV2_COMPATIBLE",
    "-DINCLUDE_NETWORK",
  ],
  srcs = [
    "Network/TLS/Imports.hs",
  ],
)

haskell_library(
  name = "serialization",
  visibility = ["//visibility:public"],
  deps = [
    "@hackage_cryptonite//:number",
  ],
  compiler_flags = [
    "-DSSLV2_COMPATIBLE",
    "-DINCLUDE_NETWORK",
  ],
  srcs = [
    "Network/TLS/Util/Serialization.hs",
  ],
)

haskell_library(
  name = "rng",
  visibility = ["//visibility:public"],
  deps = [
    "@hackage_cryptonite//:random",
  ],
  compiler_flags = [
    "-DSSLV2_COMPATIBLE",
    "-DINCLUDE_NETWORK",
  ],
  srcs = [
    "Network/TLS/RNG.hs",
  ],
)

haskell_library(
  name = "crypto_nohash",
  visibility = ["//visibility:public"],
  deps = [
    ":imports",
    ":rng",
    ":serialization",
    ":tls_base",
    "@hackage_cryptonite//:core",
    "@hackage_cryptonite//:number",
    "@hackage_cryptonite//:pubkey_nohash",
    "@hackage_memory//:memory",
  ],
  compiler_flags = [
    "-DSSLV2_COMPATIBLE",
    "-DINCLUDE_NETWORK",
  ],
  srcs = [
    "Network/TLS/Crypto/DH.hs",
    "Network/TLS/Crypto/IES.hs",
    "Network/TLS/Extra/FFDHE.hs",
  ],
)

haskell_library(
  name = "wire",
  visibility = ["//visibility:public"],
  deps = [
    ":crypto_nohash",
    ":imports",
    ":rng",
    ":serialization",
    ":tls_base",
    "@hackage_asn1_encoding//:asn1-encoding",
    "@hackage_asn1_types//:asn1-types",
    "@hackage_cereal//:cereal",
    "@hackage_cryptonite//:cipher",
    "@hackage_cryptonite//:core",
    "@hackage_cryptonite//:hash",
    "@hackage_cryptonite//:number",
    "@hackage_cryptonite//:pubkey_hash",
    "@hackage_cryptonite//:pubkey_nohash",
    "@hackage_cryptonite//:random",
    "@hackage_memory//:memory",
    "@hackage_mtl//:mtl",
    "@hackage_x509//:x509",
  ],
  compiler_flags = [
    "-DSSLV2_COMPATIBLE",
    "-DINCLUDE_NETWORK",
  ],
  srcs = [
    "Network/TLS/Cipher.hs",
    "Network/TLS/Crypto.hs",
    "Network/TLS/Extension.hs",
    "Network/TLS/Extra.hs",
    "Network/TLS/Extra/Cipher.hs",
    "Network/TLS/Handshake/State.hs",
    "Network/TLS/MAC.hs",
    "Network/TLS/Packet.hs",
    "Network/TLS/Record.hs",
    "Network/TLS/Record/Disengage.hs",
    "Network/TLS/Record/Engage.hs",
    "Network/TLS/Record/State.hs",
    "Network/TLS/Record/Types.hs",
    "Network/TLS/State.hs",
    "Network/TLS/Struct.hs",
    "Network/TLS/Wire.hs",
  ],
)

haskell_library(
  name = "tls",
  visibility = ["//visibility:public"],
  deps = [
    ":wire",
    ":rng",
    ":tls_base",
    ":imports",
    "@hackage_data_default_class//:data-default-class",
    "@hackage_mtl//:mtl",
    "@hackage_network//:network",
    "@hackage_x509//:x509",
    "@hackage_x509_store//:x509-store",
    "@hackage_x509_validation//:x509-validation",
  ],
  compiler_flags = [
    "-DSSLV2_COMPATIBLE",
    "-DINCLUDE_NETWORK",
  ],
  srcs = [
    "Network/TLS.hs",
    "Network/TLS/Context.hs",
    "Network/TLS/Context/Internal.hs",
    "Network/TLS/Core.hs",
    "Network/TLS/Credentials.hs",
    "Network/TLS/Handshake.hs",
    "Network/TLS/Handshake/Certificate.hs",
    "Network/TLS/Handshake/Client.hs",
    "Network/TLS/Handshake/Common.hs",
    "Network/TLS/Handshake/Key.hs",
    "Network/TLS/Handshake/Process.hs",
    "Network/TLS/Handshake/Server.hs",
    "Network/TLS/Handshake/Signature.hs",
    "Network/TLS/Hooks.hs",
    "Network/TLS/IO.hs",
    "Network/TLS/Internal.hs",
    "Network/TLS/Parameters.hs",
    "Network/TLS/Receiving.hs",
    "Network/TLS/Sending.hs",
    "Network/TLS/X509.hs",
  ],
)
