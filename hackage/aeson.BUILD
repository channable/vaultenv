load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "attoparsec_iso8601",
  deps = [
    "@hackage_attoparsec//:attoparsec",
    "@hackage_base_compat//:base-compat",
    "@hackage_text//:text",
  ],
  prebuilt_dependencies = [
    "base",
    "time",
  ],
  srcs = [
    "attoparsec-iso8601/Data/Attoparsec/Time.hs",
    "attoparsec-iso8601/Data/Attoparsec/Time/Internal.hs"
  ],
  src_strip_prefix = "attoparsec-iso8601",
)

cc_library(
  name = "cbits",
  includes = ["include"],
  hdrs = [
    "include/incoherent-compat.h",
    "include/overlapping-compat.h",
  ],
)

haskell_library(
  name = "unescape_pure",
  deps = ["@hackage_text//:text"],
  prebuilt_dependencies = ["base"],
  srcs = ["pure/Data/Aeson/Parser/UnescapePure.hs"],
  src_strip_prefix = "pure",
)

haskell_library(
  name = "aeson",
  visibility = ["//visibility:public"],
  deps = [
    ":attoparsec_iso8601",
    ":cbits",
    ":unescape_pure",
    "@hackage_attoparsec//:attoparsec",
    "@hackage_base_compat//:base-compat",
    "@hackage_dlist//:dlist",
    "@hackage_hashable//:hashable",
    "@hackage_scientific//:scientific",
    "@hackage_tagged//:tagged",
    "@hackage_text//:text",
    "@hackage_th_abstraction//:th-abstraction",
    "@hackage_time_locale_compat//:time-locale-compat",
    "@hackage_unordered_containers//:unordered-containers",
    "@hackage_uuid_types//:uuid-types",
    "@hackage_vector//:vector",
  ],
  prebuilt_dependencies = [
    "bytestring",
    "base",
    "time",
    "containers",
    "ghc-prim",
    "deepseq",
    "template-haskell",
  ],
  srcs = [
    "Data/Aeson.hs",
    "Data/Aeson/Compat.hs",
    "Data/Aeson/Encode.hs",
    "Data/Aeson/Encoding.hs",
    "Data/Aeson/Internal.hs",
    "Data/Aeson/Parser.hs",
    "Data/Aeson/Text.hs",
    "Data/Aeson/TH.hs",
    "Data/Aeson/Types.hs",
    "Data/Aeson/Encoding/Builder.hs",
    "Data/Aeson/Encoding/Internal.hs",
    "Data/Aeson/Internal/Functions.hs",
    "Data/Aeson/Internal/Time.hs",
    "Data/Aeson/Parser/Internal.hs",
    "Data/Aeson/Parser/Time.hs",
    "Data/Aeson/Parser/Unescape.hs",
    "Data/Aeson/Types/Class.hs",
    "Data/Aeson/Types/FromJSON.hs",
    "Data/Aeson/Types/Generic.hs",
    "Data/Aeson/Types/Internal.hs",
    "Data/Aeson/Types/ToJSON.hs",
  ],
)
