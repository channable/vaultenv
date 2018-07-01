load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "attoparsec_iso8601",
  deps = [
    "@hackage_attoparsec//:text_lazy",
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
  prebuilt_dependencies = [
    "base",
    "bytestring",
  ],
  srcs = ["pure/Data/Aeson/Parser/UnescapePure.hs"],
  src_strip_prefix = "pure",
)

haskell_library(
  name = "core",
  visibility = ["//visibility:public"],
  deps = [
    ":cbits",
    ":unescape_pure",
    "@hackage_attoparsec//:attoparsec",
    "@hackage_attoparsec//:text_lazy",
    "@hackage_attoparsec//:char8",
    "@hackage_base_compat//:base-compat",
    "@hackage_hashable//:hashable",
    "@hackage_scientific//:scientific",
    "@hackage_text//:text",
    "@hackage_unordered_containers//:unordered-containers",
    "@hackage_vector//:vector",
  ],
  prebuilt_dependencies = [
    "base",
    "bytestring",
    "containers",
    "deepseq",
    "ghc-prim",
    "template-haskell",
    "time",
  ],
  srcs = [
    "Data/Aeson/Compat.hs",
    "Data/Aeson/Internal/Functions.hs",
    "Data/Aeson/Parser.hs",
    "Data/Aeson/Parser/Internal.hs",
    "Data/Aeson/Parser/Unescape.hs",
    "Data/Aeson/Types/Generic.hs",
    "Data/Aeson/Types/Internal.hs",
  ],
)

haskell_library(
  name = "aeson_time",
  visibility = ["//visibility:public"],
  deps = [
    ":attoparsec_iso8601",
    ":core",
    "@hackage_attoparsec//:text_lazy",
    "@hackage_base_compat//:base-compat",
    "@hackage_text//:text",
  ],
  prebuilt_dependencies = [
    "base",
    "bytestring",
    "time",
  ],
  srcs = [
    "Data/Aeson/Internal/Time.hs",
    "Data/Aeson/Parser/Time.hs",
  ],
)

haskell_library(
  name = "encoding",
  visibility = ["//visibility:public"],
  deps = [
    ":core",
    ":aeson_time",
    "@hackage_base_compat//:base-compat",
    "@hackage_scientific//:scientific",
    "@hackage_text//:text",
    "@hackage_text//:lazy",
    "@hackage_unordered_containers//:unordered-containers",
    "@hackage_vector//:vector",
  ],
  prebuilt_dependencies = [
    "base",
    "bytestring",
    "time",
  ],
  srcs = [
    "Data/Aeson/Encoding.hs",
    "Data/Aeson/Encoding/Internal.hs",
    "Data/Aeson/Encoding/Builder.hs",
  ],
)

haskell_library(
  name = "from_json",
  visibility = ["//visibility:public"],
  deps = [
    ":aeson_time",
    ":cbits",
    ":core",
    "@hackage_attoparsec//:core",
    "@hackage_attoparsec//:char8",
    "@hackage_base_compat//:base-compat",
    "@hackage_dlist//:dlist",
    "@hackage_hashable//:hashable",
    "@hackage_scientific//:scientific",
    "@hackage_tagged//:tagged",
    "@hackage_text//:text",
    "@hackage_text//:lazy",
    "@hackage_time_locale_compat//:time-locale-compat",
    "@hackage_unordered_containers//:unordered-containers",
    "@hackage_uuid_types//:uuid-types",
    "@hackage_vector//:vector",
  ],
  prebuilt_dependencies = [
    "base",
    "bytestring",
    "containers",
    "time",
  ],
  srcs = [
    "Data/Aeson/Types/FromJSON.hs",
  ],
)

haskell_library(
  name = "to_json",
  visibility = ["//visibility:public"],
  deps = [
    ":cbits",
    ":core",
    ":encoding",
    "@hackage_attoparsec//:core",
    "@hackage_base_compat//:base-compat",
    "@hackage_dlist//:dlist",
    "@hackage_hashable//:hashable",
    "@hackage_scientific//:scientific",
    "@hackage_tagged//:tagged",
    "@hackage_text//:text",
    "@hackage_text//:lazy",
    "@hackage_time_locale_compat//:time-locale-compat",
    "@hackage_unordered_containers//:unordered-containers",
    "@hackage_uuid_types//:uuid-types",
    "@hackage_vector//:vector",
  ],
  prebuilt_dependencies = [
    "base",
    "bytestring",
    "containers",
    "time",
  ],
  srcs = [
    "Data/Aeson/Types/ToJSON.hs",
  ],
)

haskell_library(
  name = "internal",
  visibility = ["//visibility:public"],
  deps = [
    ":core",
    ":from_json",
  ],
  prebuilt_dependencies = [
    "base",
    "bytestring",
  ],
  srcs = [
    "Data/Aeson/Internal.hs",
  ],
)

haskell_library(
  name = "types",
  visibility = ["//visibility:public"],
  deps = [
    ":core",
    ":to_json",
    ":encoding",
    ":from_json",
    "@hackage_base_compat//:base-compat",
    "@hackage_hashable//:hashable",
    "@hackage_scientific//:scientific",
    "@hackage_text//:builder",
    "@hackage_text//:lazy",
    "@hackage_text//:text",
    "@hackage_unordered_containers//:unordered-containers",
    "@hackage_vector//:vector",
  ],
  prebuilt_dependencies = [
    "base",
    "bytestring",
  ],
  srcs = [
    "Data/Aeson/Types/Class.hs",
    "Data/Aeson/Types.hs",
    "Data/Aeson/Text.hs",
  ],
)

haskell_library(
  name = "aeson",
  visibility = ["//visibility:public"],
  deps = [
    ":cbits",
    ":core",
    ":encoding",
    ":from_json",
    ":types",
    "@hackage_base_compat//:base-compat",
    "@hackage_hashable//:hashable",
    "@hackage_text//:builder",
    "@hackage_text//:text",
    "@hackage_th_abstraction//:th-abstraction",
    "@hackage_unordered_containers//:unordered-containers",
    "@hackage_vector//:vector",
  ],
  prebuilt_dependencies = [
    "base",
    "bytestring",
    "containers",
    "template-haskell",
  ],
  srcs = [
    "Data/Aeson.hs",
    "Data/Aeson/Encode.hs",
    "Data/Aeson/TH.hs",
  ],
)
