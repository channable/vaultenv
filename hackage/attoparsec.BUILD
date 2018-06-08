load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "core",
  visibility = ["//visibility:public"],
  prebuilt_dependencies = [
    "base",
    "bytestring",
    "deepseq",
  ],
  srcs = [
    "Data/Attoparsec/Internal/Fhthagn.hs",
    "Data/Attoparsec/Number.hs",
    "Data/Attoparsec/Zepto.hs",
  ],
)

haskell_library(
  name = "attoparsec",
  visibility = ["//visibility:public"],
  deps = [
    ":core",
    "@hackage_text//:text",
    "@hackage_scientific//:scientific",
  ],
  prebuilt_dependencies = [
    "base",
  ],
  srcs = [
    "Data/Attoparsec.hs",
    "Data/Attoparsec/ByteString.hs",
    "Data/Attoparsec/ByteString/Buffer.hs",
    "Data/Attoparsec/ByteString/FastSet.hs",
    "Data/Attoparsec/ByteString/Internal.hs",
    "Data/Attoparsec/ByteString/Lazy.hs",
    "Data/Attoparsec/Combinator.hs",
    "Data/Attoparsec/Internal.hs",
    "Data/Attoparsec/Internal/Types.hs",
    "Data/Attoparsec/Lazy.hs",
    "Data/Attoparsec/Text/Buffer.hs",
    "Data/Attoparsec/Text/FastSet.hs",
    "Data/Attoparsec/Text/Internal.hs",
    "Data/Attoparsec/Types.hs",

    # These files could be in a separate target, such that the :attoparsec
    # target can avoid the dependency on "scientific". But unfortenately doing
    # so causes linker errors in "aeson". :(
    "Data/Attoparsec/ByteString/Char8.hs",
    "Data/Attoparsec/Char8.hs",
    "Data/Attoparsec/Text.hs",
    "Data/Attoparsec/Text/Lazy.hs",
  ],
)
