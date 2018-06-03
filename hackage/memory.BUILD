load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

# HACK: The "memory" package hard-codes a MIN_VERSION_basement preprocessor
# check for conditional compilation, but we split up that package into multiple
# smaller ones with different names, hence the macro breaks. Work around that by
# creating a dependency with the same name and version. We have to put a file in
# there as well. It is gross, I am sorry.
haskell_library(
  name = "basement",
  version = "0.0.4",
  prebuilt_dependencies = ["base"],
  srcs = [
    "Data/Memory/MemMap/Posix.hsc",
  ],
)

# HACK: Same as above. :(
haskell_library(
  name = "foundation",
  version = "0.0.17",
  prebuilt_dependencies = ["base"],
  srcs = [
    "Data/Memory/Internal/Compat.hs",
  ],
)

haskell_library(
  name = "memory",
  visibility = ["//visibility:public"],
  deps = [
    ":basement",
    ":foundation",
    "@hackage_basement//:compat_and_numerical",
    "@hackage_basement//:core",
    "@hackage_basement//:encoding",
    "@hackage_basement//:memory",
    "@hackage_foundation//:array",
  ],
  prebuilt_dependencies = [
    "bytestring",
    "base",
    "ghc-prim",
    "deepseq",
  ],
  compiler_flags = [
    "-DWITH_BYTESTRING_SUPPORT",
    "-DWITH_DEEPSEQ_SUPPORT",
    "-DWITH_FOUNDATION_SUPPORT",
  ],
  srcs = [
    "Data/ByteArray.hs",
    "Data/ByteArray/Encoding.hs",
    "Data/ByteArray/Mapping.hs",
    "Data/ByteArray/Pack.hs",
    "Data/ByteArray/Parse.hs",
    "Data/ByteArray/Hash.hs",
    "Data/Memory/PtrMethods.hs",
    "Data/Memory/ExtendedWords.hs",
    "Data/Memory/Encoding/Base16.hs",
    "Data/Memory/Encoding/Base32.hs",
    "Data/Memory/Encoding/Base64.hs",
    "Data/Memory/Internal/CompatPrim.hs",
    "Data/Memory/Internal/CompatPrim64.hs",
    "Data/Memory/Internal/DeepSeq.hs",
    "Data/Memory/Internal/Imports.hs",
    "Data/Memory/Internal/Scrubber.hs",
    "Data/Memory/Hash/SipHash.hs",
    "Data/Memory/Hash/FNV.hs",
    "Data/ByteArray/Pack/Internal.hs",
    "Data/ByteArray/Types.hs",
    "Data/ByteArray/Bytes.hs",
    "Data/ByteArray/ScrubbedBytes.hs",
    "Data/ByteArray/Methods.hs",
    "Data/ByteArray/MemView.hs",
    "Data/ByteArray/View.hs",
    "Data/Memory/Endian.hs",
  ],
)
