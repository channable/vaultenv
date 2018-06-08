load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "lens-aeson",
  visibility = ["//visibility:public"],
  deps = [
    "@hackage_unordered_containers//:unordered-containers",
    "@hackage_text//:text",
    "@hackage_lens//:control_lens",
    "@hackage_lens//:text",
    "@hackage_attoparsec//:attoparsec",
    "@hackage_scientific//:scientific",
    "@hackage_aeson//:aeson",
    "@hackage_aeson//:core",
    "@hackage_vector//:vector",
  ],
  prebuilt_dependencies = [
    "bytestring",
    "base",
  ],
  srcs = [
    "src/Data/Aeson/Lens.hs",
  ],
  src_strip_prefix = "src",
)
