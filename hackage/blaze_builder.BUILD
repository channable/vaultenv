load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "blaze-builder",
  visibility = ["//visibility:public"],
  deps = [
    "@hackage_text//:text",
    "@hackage_text//:lazy",
  ],
  prebuilt_dependencies = [
    "bytestring",
    "base",
    "deepseq",
  ],
  srcs = [
    "Blaze/ByteString/Builder.hs",
    "Blaze/ByteString/Builder/Word.hs",
    "Blaze/ByteString/Builder/Char8.hs",
    "Blaze/ByteString/Builder/Int.hs",
    "Blaze/ByteString/Builder/HTTP.hs",
    "Blaze/ByteString/Builder/ByteString.hs",
    "Blaze/ByteString/Builder/Internal/Write.hs",
    "Blaze/ByteString/Builder/Char/Utf8.hs",
    "Blaze/ByteString/Builder/Compat/Write.hs",
    "Blaze/ByteString/Builder/Html/Utf8.hs",
  ],
)
