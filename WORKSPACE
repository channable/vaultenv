# Current master has some fixes that are not in v0.5.
rules_haskell_version = "a7ed28171ff9e4dfad23c4669eead8c170bc8e0c"

http_archive(
  name = "io_tweag_rules_haskell",
  strip_prefix = "rules_haskell-" + rules_haskell_version,
  urls = ["https://github.com/tweag/rules_haskell/archive/" + rules_haskell_version + ".tar.gz"],
  sha256 = "2b2ce55757bf44fce050dc308fd33f5645a0c57415cbdce496d822bcea8bc987",
)

load("@io_tweag_rules_haskell//haskell:repositories.bzl", "haskell_repositories")

haskell_repositories()

load("@io_tweag_rules_haskell//haskell:haskell.bzl", "ghc_bindist")

ghc_bindist(
  name    = "ghc",
  version = "8.2.2",
)

register_toolchains("//:ghc")

new_http_archive(
  name = "hackage_utf8_string",
  urls = [
    "https://hackage.haskell.org/package/utf8-string-1.0.1.1/utf8-string-1.0.1.1.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/utf8-string-1.0.1.1.tar.gz",
  ],
  strip_prefix = "utf8-string-1.0.1.1",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "utf8-string",
  visibility = ["//visibility:public"],
  deps = [
  ],
  prebuilt_dependencies = [
    "bytestring",
    "base",
  ],
  srcs = [
    "Codec/Binary/UTF8/Generic.hs",
    "Codec/Binary/UTF8/String.hs",
    "Data/ByteString/UTF8.hs",
    "Data/ByteString/Lazy/UTF8.hs",
    "Data/String/UTF8.hs",
  ],
)
""",
)

new_http_archive(
  name = "hackage_text",
  urls = [
    "https://hackage.haskell.org/package/text-1.2.2.2/text-1.2.2.2.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/text-1.2.2.2.tar.gz",
  ],
  strip_prefix = "text-1.2.2.2",
  build_file = "hackage/text.BUILD",
)

new_http_archive(
  name = "hackage_hashable",
  urls = [
    "https://hackage.haskell.org/package/hashable-1.2.6.1/hashable-1.2.6.1.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/hashable-1.2.6.1.tar.gz",
  ],
  strip_prefix = "hashable-1.2.6.1",
  build_file = "hackage/hashable.BUILD",
)

new_http_archive(
  name = "hackage_unordered_containers",
  urls = [
    "https://hackage.haskell.org/package/unordered-containers-0.2.8.0/unordered-containers-0.2.8.0.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/unordered-containers-0.2.8.0.tar.gz",
  ],
  strip_prefix = "unordered-containers-0.2.8.0",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "unordered-containers",
  visibility = ["//visibility:public"],
  deps = [
    "@hackage_hashable//:hashable",
  ],
  prebuilt_dependencies = [
    "base",
    "deepseq",
  ],
  srcs = [
    "Data/HashSet.hs",
    "Data/HashMap/Base.hs",
    "Data/HashMap/Lazy.hs",
    "Data/HashMap/UnsafeShift.hs",
    "Data/HashMap/Strict.hs",
    "Data/HashMap/PopCount.hs",
    "Data/HashMap/Unsafe.hs",
    "Data/HashMap/Array.hs",
  ],
)
""",
)

new_http_archive(
  name = "hackage_random",
  urls = [
    "https://hackage.haskell.org/package/random-1.1/random-1.1.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/random-1.1.tar.gz",
  ],
  strip_prefix = "random-1.1",
  build_file = "hackage/random.BUILD",
)

new_http_archive(
  name = "hackage_data_default_class",
  urls = [
    "https://hackage.haskell.org/package/data-default-class-0.1.2.0/data-default-class-0.1.2.0.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/data-default-class-0.1.2.0.tar.gz",
  ],
  strip_prefix = "data-default-class-0.1.2.0",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "data-default-class",
  visibility = ["//visibility:public"],
  deps = [
  ],
  prebuilt_dependencies = [
    "base",
  ],
  srcs = [
    "Data/Default/Class.hs",
  ],
)
""",
)

new_http_archive(
  name = "hackage_transformers_compat",
  urls = [
    "https://hackage.haskell.org/package/transformers-compat-0.5.1.4/transformers-compat-0.5.1.4.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/transformers-compat-0.5.1.4.tar.gz",
  ],
  strip_prefix = "transformers-compat-0.5.1.4",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "transformers-compat",
  visibility = ["//visibility:public"],
  deps = [
  ],
  prebuilt_dependencies = [
    "base",
    "ghc-prim",
    "transformers",
  ],
  srcs = [
    "src/Control/Monad/Trans/Instances.hs",
  ],
  src_strip_prefix = "src",
)
""",
)

new_http_archive(
  name = "hackage_mtl",
  urls = [
    "https://hackage.haskell.org/package/mtl-2.2.1/mtl-2.2.1.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/mtl-2.2.1.tar.gz",
  ],
  strip_prefix = "mtl-2.2.1",
  build_file = "hackage/mtl.BUILD",
)

new_http_archive(
  name = "hackage_stm",
  urls = [
    "https://hackage.haskell.org/package/stm-2.4.5.0/stm-2.4.5.0.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/stm-2.4.5.0.tar.gz",
  ],
  strip_prefix = "stm-2.4.5.0",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "stm",
  visibility = ["//visibility:public"],
  deps = [
  ],
  prebuilt_dependencies = [
    "base",
    "array",
  ],
  srcs = [
    "Control/Concurrent/STM.hs",
    "Control/Concurrent/STM/TArray.hs",
    "Control/Concurrent/STM/TVar.hs",
    "Control/Concurrent/STM/TChan.hs",
    "Control/Concurrent/STM/TMVar.hs",
    "Control/Concurrent/STM/TQueue.hs",
    "Control/Concurrent/STM/TBQueue.hs",
    "Control/Concurrent/STM/TSem.hs",
    "Control/Monad/STM.hs",
    "Control/Sequential/STM.hs",
  ],
)
""",
)

new_http_archive(
  name = "hackage_exceptions",
  urls = [
    "https://hackage.haskell.org/package/exceptions-0.8.3/exceptions-0.8.3.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/exceptions-0.8.3.tar.gz",
  ],
  strip_prefix = "exceptions-0.8.3",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "exceptions",
  # TODO(ruuda): Extract to its own BUILD file if this makes a difference.
  # Lens does inspec the version for conditional compilation.
  version = "0.8.3",
  visibility = ["//visibility:public"],
  deps = [
    "@hackage_stm//:stm",
    "@hackage_mtl//:mtl",
    "@hackage_transformers_compat//:transformers-compat",
  ],
  prebuilt_dependencies = [
    "base",
    "transformers",
    "template-haskell",
  ],
  srcs = [
    "src/Control/Monad/Catch.hs",
    "src/Control/Monad/Catch/Pure.hs",
  ],
  src_strip_prefix = "src",
)
""",
)

new_http_archive(
  name = "hackage_retry",
  urls = [
    "https://hackage.haskell.org/package/retry-0.7.6.0/retry-0.7.6.0.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/retry-0.7.6.0.tar.gz",
  ],
  strip_prefix = "retry-0.7.6.0",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "retry",
  visibility = ["//visibility:public"],
  deps = [
    "@hackage_exceptions//:exceptions",
    "@hackage_data_default_class//:data-default-class",
    "@hackage_random//:random",
  ],
  prebuilt_dependencies = [
    "base",
    "ghc-prim",
    "transformers",
  ],
  srcs = [
    "src/Control/Retry.hs",
  ],
  src_strip_prefix = "src",
)
""",
)

new_http_archive(
  name = "hackage_primitive",
  urls = [
    "https://hackage.haskell.org/package/primitive-0.6.3.0/primitive-0.6.3.0.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/primitive-0.6.3.0.tar.gz",
  ],
  strip_prefix = "primitive-0.6.3.0",
  build_file = "hackage/primitive.BUILD",
)

new_http_archive(
  name = "hackage_vector",
  urls = [
    "https://hackage.haskell.org/package/vector-0.12.0.1/vector-0.12.0.1.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/vector-0.12.0.1.tar.gz",
  ],
  strip_prefix = "vector-0.12.0.1",
  build_file = "hackage/vector.BUILD",
)

new_http_archive(
  name = "hackage_time_locale_compat",
  urls = [
    "https://hackage.haskell.org/package/time-locale-compat-0.1.1.3/time-locale-compat-0.1.1.3.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/time-locale-compat-0.1.1.3.tar.gz",
  ],
  strip_prefix = "time-locale-compat-0.1.1.3",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "time-locale-compat",
  visibility = ["//visibility:public"],
  deps = [
  ],
  prebuilt_dependencies = [
    "base",
    "time",
  ],
  srcs = [
    "src/Data/Time/Locale/Compat.hs",
  ],
  src_strip_prefix = "src",
)
""",
)

new_http_archive(
  name = "hackage_integer_logarithms",
  urls = [
    "https://hackage.haskell.org/package/integer-logarithms-1.0.2/integer-logarithms-1.0.2.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/integer-logarithms-1.0.2.tar.gz",
  ],
  strip_prefix = "integer-logarithms-1.0.2",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "integer-logarithms",
  visibility = ["//visibility:public"],
  deps = [
  ],
  prebuilt_dependencies = [
    "base",
    "array",
    "integer-gmp",
    "ghc-prim",
  ],
  srcs = [
    "src/GHC/Integer/Logarithms/Compat.hs",
    "src/Math/NumberTheory/Logarithms.hs",
    "src/Math/NumberTheory/Powers/Natural.hs",
    "src/Math/NumberTheory/Powers/Integer.hs",
  ],
  src_strip_prefix = "src",
)
""",
)

new_http_archive(
  name = "hackage_scientific",
  urls = [
    "https://hackage.haskell.org/package/scientific-0.3.5.2/scientific-0.3.5.2.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/scientific-0.3.5.2.tar.gz",
  ],
  strip_prefix = "scientific-0.3.5.2",
  build_file = "hackage/scientific.BUILD",
)

new_http_archive(
  name = "hackage_uuid_types",
  urls = [
    "https://hackage.haskell.org/package/uuid-types-1.0.3/uuid-types-1.0.3.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/uuid-types-1.0.3.tar.gz",
  ],
  strip_prefix = "uuid-types-1.0.3",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "uuid-types",
  visibility = ["//visibility:public"],
  deps = [
    "@hackage_text//:text",
    "@hackage_hashable//:hashable",
    "@hackage_random//:random",
  ],
  prebuilt_dependencies = [
    "bytestring",
    "base",
    "binary",
    "deepseq",
  ],
  srcs = [
    "Data/UUID/Types.hs",
    "Data/UUID/Types/Internal.hs",
    "Data/UUID/Types/Internal/Builder.hs",
  ],
)
""",
)

new_http_archive(
  name = "hackage_attoparsec",
  urls = [
    "https://hackage.haskell.org/package/attoparsec-0.13.2.2/attoparsec-0.13.2.2.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/attoparsec-0.13.2.2.tar.gz",
  ],
  strip_prefix = "attoparsec-0.13.2.2",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "attoparsec",
  visibility = ["//visibility:public"],
  deps = [
    "@hackage_text//:text",
    "@hackage_scientific//:scientific",
  ],
  prebuilt_dependencies = [
    "bytestring",
    "base",
    "array",
    "containers",
    "transformers",
    "deepseq",
  ],
  srcs = [
    "Data/Attoparsec.hs",
    "Data/Attoparsec/Lazy.hs",
    "Data/Attoparsec/Char8.hs",
    "Data/Attoparsec/Types.hs",
    "Data/Attoparsec/Text.hs",
    "Data/Attoparsec/Internal.hs",
    "Data/Attoparsec/Zepto.hs",
    "Data/Attoparsec/Number.hs",
    "Data/Attoparsec/ByteString.hs",
    "Data/Attoparsec/Combinator.hs",
    "Data/Attoparsec/Text/FastSet.hs",
    "Data/Attoparsec/Text/Lazy.hs",
    "Data/Attoparsec/Text/Buffer.hs",
    "Data/Attoparsec/Text/Internal.hs",
    "Data/Attoparsec/ByteString/FastSet.hs",
    "Data/Attoparsec/ByteString/Lazy.hs",
    "Data/Attoparsec/ByteString/Char8.hs",
    "Data/Attoparsec/ByteString/Buffer.hs",
    "Data/Attoparsec/ByteString/Internal.hs",
    "Data/Attoparsec/Internal/Types.hs",
    "Data/Attoparsec/Internal/Fhthagn.hs",
  ],
)
""",
)

new_http_archive(
  name = "hackage_base_compat",
  urls = [
    "https://hackage.haskell.org/package/base-compat-0.9.3/base-compat-0.9.3.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/base-compat-0.9.3.tar.gz",
  ],
  strip_prefix = "base-compat-0.9.3",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "base-compat",
  visibility = ["//visibility:public"],
  deps = [
  ],
  prebuilt_dependencies = [
    "unix",
    "base",
  ],
  srcs = [
    "src/Text/Read/Compat.hs",
    "src/Control/Concurrent/Compat.hs",
    "src/Control/Concurrent/MVar/Compat.hs",
    "src/Control/Monad/Compat.hs",
    "src/Control/Monad/ST/Lazy/Unsafe/Compat.hs",
    "src/Control/Monad/ST/Unsafe/Compat.hs",
    "src/Data/Bits/Compat.hs",
    "src/Data/Complex/Compat.hs",
    "src/Data/Monoid/Compat.hs",
    "src/Data/Type/Coercion/Compat.hs",
    "src/Data/STRef/Compat.hs",
    "src/Data/List/Compat.hs",
    "src/Data/Proxy/Compat.hs",
    "src/Data/Word/Compat.hs",
    "src/Data/Ratio/Compat.hs",
    "src/Data/Either/Compat.hs",
    "src/Data/IORef/Compat.hs",
    "src/Data/Functor/Compat.hs",
    "src/Data/Functor/Const/Compat.hs",
    "src/Data/Bool/Compat.hs",
    "src/Data/String/Compat.hs",
    "src/Data/Function/Compat.hs",
    "src/Data/Version/Compat.hs",
    "src/Data/Foldable/Compat.hs",
    "src/Numeric/Compat.hs",
    "src/Foreign/Compat.hs",
    "src/Foreign/Marshal/Compat.hs",
    "src/Foreign/Marshal/Alloc/Compat.hs",
    "src/Foreign/Marshal/Unsafe/Compat.hs",
    "src/Foreign/Marshal/Array/Compat.hs",
    "src/Foreign/Marshal/Utils/Compat.hs",
    "src/Foreign/Marshal/Safe/Compat.hs",
    "src/Foreign/ForeignPtr/Compat.hs",
    "src/Foreign/ForeignPtr/Unsafe/Compat.hs",
    "src/Foreign/ForeignPtr/Safe/Compat.hs",
    "src/System/IO/Unsafe/Compat.hs",
    "src/System/Environment/Compat.hs",
    "src/System/Exit/Compat.hs",
    "src/Prelude/Compat.hs",
    "src/Debug/Trace/Compat.hs",
  ],
  src_strip_prefix = "src",
)
""",
)

new_http_archive(
  name = "hackage_tagged",
  urls = [
    "https://hackage.haskell.org/package/tagged-0.8.5/tagged-0.8.5.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/tagged-0.8.5.tar.gz",
  ],
  strip_prefix = "tagged-0.8.5",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "tagged",
  visibility = ["//visibility:public"],
  deps = [
    "@hackage_transformers_compat//:transformers-compat",
  ],
  prebuilt_dependencies = [
    "base",
    "transformers",
    "deepseq",
    "template-haskell",
  ],
  srcs = [
    "src/Data/Tagged.hs",
    "src/Data/Proxy/TH.hs",
  ],
  src_strip_prefix = "src",
)
""",
)

new_http_archive(
  name = "hackage_th_abstraction",
  urls = [
    "https://hackage.haskell.org/package/th-abstraction-0.2.6.0/th-abstraction-0.2.6.0.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/th-abstraction-0.2.6.0.tar.gz",
  ],
  strip_prefix = "th-abstraction-0.2.6.0",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "th-abstraction",
  visibility = ["//visibility:public"],
  deps = [
  ],
  prebuilt_dependencies = [
    "base",
    "containers",
    "ghc-prim",
    "template-haskell",
  ],
  srcs = [
    "src/Language/Haskell/TH/Datatype.hs",
    "src/Language/Haskell/TH/Datatype/Internal.hs",
  ],
  src_strip_prefix = "src",
)
""",
)

new_http_archive(
  name = "hackage_dlist",
  urls = [
    "https://hackage.haskell.org/package/dlist-0.8.0.4/dlist-0.8.0.4.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/dlist-0.8.0.4.tar.gz",
  ],
  strip_prefix = "dlist-0.8.0.4",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "dlist",
  visibility = ["//visibility:public"],
  deps = [
  ],
  prebuilt_dependencies = [
    "base",
    "deepseq",
  ],
  srcs = [
    "Data/DList.hs",
  ],
)
""",
)

new_http_archive(
  name = "hackage_aeson",
  urls = [
    "https://hackage.haskell.org/package/aeson-1.2.4.0/aeson-1.2.4.0.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/aeson-1.2.4.0.tar.gz",
  ],
  strip_prefix = "aeson-1.2.4.0",
  build_file = "hackage/aeson.BUILD",
)

new_http_archive(
  name = "hackage_StateVar",
  urls = [
    "https://hackage.haskell.org/package/StateVar-1.1.0.4/StateVar-1.1.0.4.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/StateVar-1.1.0.4.tar.gz",
  ],
  strip_prefix = "StateVar-1.1.0.4",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "StateVar",
  visibility = ["//visibility:public"],
  deps = [
    "@hackage_stm//:stm",
  ],
  prebuilt_dependencies = [
    "base",
    "transformers",
  ],
  srcs = [
    "src/Data/StateVar.hs",
  ],
  src_strip_prefix = "src",
)
""",
)

new_http_archive(
  name = "hackage_contravariant",
  urls = [
    "https://hackage.haskell.org/package/contravariant-1.4.1/contravariant-1.4.1.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/contravariant-1.4.1.tar.gz",
  ],
  strip_prefix = "contravariant-1.4.1",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "contravariant",
  visibility = ["//visibility:public"],
  deps = [
    "@hackage_StateVar//:StateVar",
    "@hackage_transformers_compat//:transformers-compat",
  ],
  prebuilt_dependencies = [
    "base",
    "transformers",
  ],
  srcs = [
    "src/Data/Functor/Contravariant.hs",
    "src/Data/Functor/Contravariant/Compose.hs",
    "src/Data/Functor/Contravariant/Divisible.hs",
    "src/Data/Functor/Contravariant/Generic.hs",
  ],
  src_strip_prefix = "src",
)
""",
)

new_http_archive(
  name = "hackage_base_orphans",
  urls = [
    "https://hackage.haskell.org/package/base-orphans-0.6/base-orphans-0.6.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/base-orphans-0.6.tar.gz",
  ],
  strip_prefix = "base-orphans-0.6",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "base-orphans",
  visibility = ["//visibility:public"],
  deps = [
  ],
  prebuilt_dependencies = [
    "base",
    "ghc-prim",
  ],
  srcs = [
    "src/Data/Orphans.hs",
    "src/Data/Orphans/Prelude.hs",
  ],
  src_strip_prefix = "src",
)
""",
)

new_http_archive(
  name = "hackage_semigroups",
  urls = [
    "https://hackage.haskell.org/package/semigroups-0.18.4/semigroups-0.18.4.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/semigroups-0.18.4.tar.gz",
  ],
  strip_prefix = "semigroups-0.18.4",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "semigroups",
  visibility = ["//visibility:public"],
  deps = [
  ],
  prebuilt_dependencies = [
    "base",
  ],
  srcs = [
    "src/Data/Semigroup/Generic.hs",
  ],
  src_strip_prefix = "src",
)
""",
)

new_http_archive(
  name = "hackage_distributive",
  urls = [
    "https://hackage.haskell.org/package/distributive-0.5.3/distributive-0.5.3.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/distributive-0.5.3.tar.gz",
  ],
  strip_prefix = "distributive-0.5.3",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "distributive",
  visibility = ["//visibility:public"],
  deps = [
    "@hackage_base_orphans//:base-orphans",
    "@hackage_tagged//:tagged",
    "@hackage_transformers_compat//:transformers-compat",
  ],
  prebuilt_dependencies = [
    "base",
    "transformers",
  ],
  srcs = [
    "src/Data/Distributive.hs",
    "src/Data/Distributive/Generic.hs",
  ],
  src_strip_prefix = "src",
)
""",
)

new_http_archive(
  name = "hackage_comonad",
  urls = [
    "https://hackage.haskell.org/package/comonad-5.0.3/comonad-5.0.3.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/comonad-5.0.3.tar.gz",
  ],
  strip_prefix = "comonad-5.0.3",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "comonad",
  visibility = ["//visibility:public"],
  deps = [
    "@hackage_distributive//:distributive",
    "@hackage_semigroups//:semigroups",
    "@hackage_tagged//:tagged",
    "@hackage_contravariant//:contravariant",
    "@hackage_transformers_compat//:transformers-compat",
  ],
  prebuilt_dependencies = [
    "base",
    "containers",
    "transformers",
  ],
  srcs = [
    "src/Control/Comonad.hs",
    "src/Control/Comonad/Identity.hs",
    "src/Control/Comonad/Env.hs",
    "src/Control/Comonad/Store.hs",
    "src/Control/Comonad/Traced.hs",
    "src/Control/Comonad/Store/Class.hs",
    "src/Control/Comonad/Trans/Identity.hs",
    "src/Control/Comonad/Trans/Env.hs",
    "src/Control/Comonad/Trans/Class.hs",
    "src/Control/Comonad/Trans/Store.hs",
    "src/Control/Comonad/Trans/Traced.hs",
    "src/Control/Comonad/Traced/Class.hs",
    "src/Control/Comonad/Hoist/Class.hs",
    "src/Control/Comonad/Env/Class.hs",
    "src/Data/Functor/Composition.hs",
  ],
  src_strip_prefix = "src",
)
""",
)

new_http_archive(
  name = "hackage_bifunctors",
  urls = [
    "https://hackage.haskell.org/package/bifunctors-5.5.2/bifunctors-5.5.2.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/bifunctors-5.5.2.tar.gz",
  ],
  strip_prefix = "bifunctors-5.5.2",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "bifunctors",
  visibility = ["//visibility:public"],
  deps = [
    "@hackage_comonad//:comonad",
    "@hackage_semigroups//:semigroups",
    "@hackage_base_orphans//:base-orphans",
    "@hackage_th_abstraction//:th-abstraction",
    "@hackage_tagged//:tagged",
    "@hackage_transformers_compat//:transformers-compat",
  ],
  prebuilt_dependencies = [
    "base",
    "containers",
    "transformers",
    "template-haskell",
  ],
  srcs = [
    "src/Data/Biapplicative.hs",
    "src/Data/Bifunctor/TH.hs",
    "src/Data/Bifunctor/Flip.hs",
    "src/Data/Bifunctor/Wrapped.hs",
    "src/Data/Bifunctor/Joker.hs",
    "src/Data/Bifunctor/Product.hs",
    "src/Data/Bifunctor/Clown.hs",
    "src/Data/Bifunctor/Biff.hs",
    "src/Data/Bifunctor/Join.hs",
    "src/Data/Bifunctor/Fix.hs",
    "src/Data/Bifunctor/Tannen.hs",
    "src/Data/Bifunctor/Functor.hs",
    "src/Data/Bifunctor/Sum.hs",
    "src/Data/Bifunctor/TH/Internal.hs",
  ],
  src_strip_prefix = "src",
)
""",
)

new_http_archive(
  name = "hackage_profunctors",
  urls = [
    "https://hackage.haskell.org/package/profunctors-5.2.2/profunctors-5.2.2.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/profunctors-5.2.2.tar.gz",
  ],
  strip_prefix = "profunctors-5.2.2",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "profunctors",
  visibility = ["//visibility:public"],
  deps = [
    "@hackage_comonad//:comonad",
    "@hackage_distributive//:distributive",
    "@hackage_semigroups//:semigroups",
    "@hackage_base_orphans//:base-orphans",
    "@hackage_tagged//:tagged",
    "@hackage_bifunctors//:bifunctors",
    "@hackage_contravariant//:contravariant",
  ],
  prebuilt_dependencies = [
    "base",
    "transformers",
  ],
  srcs = [
    "src/Data/Profunctor.hs",
    "src/Data/Profunctor/Choice.hs",
    "src/Data/Profunctor/Cayley.hs",
    "src/Data/Profunctor/Ran.hs",
    "src/Data/Profunctor/Sieve.hs",
    "src/Data/Profunctor/Traversing.hs",
    "src/Data/Profunctor/Unsafe.hs",
    "src/Data/Profunctor/Yoneda.hs",
    "src/Data/Profunctor/Rep.hs",
    "src/Data/Profunctor/Adjunction.hs",
    "src/Data/Profunctor/Types.hs",
    "src/Data/Profunctor/Strong.hs",
    "src/Data/Profunctor/Closed.hs",
    "src/Data/Profunctor/Composition.hs",
    "src/Data/Profunctor/Monad.hs",
    "src/Data/Profunctor/Mapping.hs",
  ],
  src_strip_prefix = "src",
)
""",
)

new_http_archive(
  name = "hackage_call_stack",
  urls = [
    "https://hackage.haskell.org/package/call-stack-0.1.0/call-stack-0.1.0.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/call-stack-0.1.0.tar.gz",
  ],
  strip_prefix = "call-stack-0.1.0",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "call-stack",
  visibility = ["//visibility:public"],
  deps = [
  ],
  prebuilt_dependencies = [
    "base",
  ],
  srcs = [
    "src/Data/SrcLoc.hs",
    "src/Data/CallStack.hs",
  ],
  src_strip_prefix = "src",
)
""",
)

new_http_archive(
  name = "hackage_parallel",
  urls = [
    "https://hackage.haskell.org/package/parallel-3.2.1.1/parallel-3.2.1.1.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/parallel-3.2.1.1.tar.gz",
  ],
  strip_prefix = "parallel-3.2.1.1",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "parallel",
  visibility = ["//visibility:public"],
  deps = [
  ],
  prebuilt_dependencies = [
    "base",
    "array",
    "containers",
    "deepseq",
  ],
  srcs = [
    "Control/Seq.hs",
    "Control/Parallel.hs",
    "Control/Parallel/Strategies.hs",
  ],
)
""",
)

new_http_archive(
  name = "hackage_void",
  urls = [
    "https://hackage.haskell.org/package/void-0.7.2/void-0.7.2.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/void-0.7.2.tar.gz",
  ],
  strip_prefix = "void-0.7.2",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "void",
  visibility = ["//visibility:public"],
  deps = [
  ],
  prebuilt_dependencies = [
    "base",
  ],
  srcs = [
    "src/Data/Void/Unsafe.hs",
  ],
  src_strip_prefix = "src",
)
""",
)

new_http_archive(
  name = "hackage_prelude_extras",
  urls = [
    "https://hackage.haskell.org/package/prelude-extras-0.4.0.3/prelude-extras-0.4.0.3.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/prelude-extras-0.4.0.3.tar.gz",
  ],
  strip_prefix = "prelude-extras-0.4.0.3",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "prelude-extras",
  visibility = ["//visibility:public"],
  deps = [
  ],
  prebuilt_dependencies = [
    "base",
  ],
  srcs = [
    "src/Prelude/Extras.hs",
  ],
  src_strip_prefix = "src",
)
""",
)

new_http_archive(
  name = "hackage_semigroupoids",
  urls = [
    "https://hackage.haskell.org/package/semigroupoids-5.2.1/semigroupoids-5.2.1.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/semigroupoids-5.2.1.tar.gz",
  ],
  strip_prefix = "semigroupoids-5.2.1",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "semigroupoids",
  visibility = ["//visibility:public"],
  deps = [
    "@hackage_comonad//:comonad",
    "@hackage_unordered_containers//:unordered-containers",
    "@hackage_distributive//:distributive",
    "@hackage_semigroups//:semigroups",
    "@hackage_base_orphans//:base-orphans",
    "@hackage_tagged//:tagged",
    "@hackage_bifunctors//:bifunctors",
    "@hackage_contravariant//:contravariant",
    "@hackage_hashable//:hashable",
    "@hackage_transformers_compat//:transformers-compat",
  ],
  prebuilt_dependencies = [
    "base",
    "containers",
    "transformers",
  ],
  srcs = [
    "src/Data/Isomorphism.hs",
    "src/Data/Semigroupoid.hs",
    "src/Data/Groupoid.hs",
    "src/Data/Semigroupoid/Dual.hs",
    "src/Data/Semigroupoid/Static.hs",
    "src/Data/Semigroupoid/Ob.hs",
    "src/Data/Bifunctor/Apply.hs",
    "src/Data/Functor/Bind.hs",
    "src/Data/Functor/Apply.hs",
    "src/Data/Functor/Alt.hs",
    "src/Data/Functor/Extend.hs",
    "src/Data/Functor/Plus.hs",
    "src/Data/Functor/Bind/Class.hs",
    "src/Data/Functor/Bind/Trans.hs",
    "src/Data/Traversable/Instances.hs",
    "src/Data/Semigroup/Bifoldable.hs",
    "src/Data/Semigroup/Foldable.hs",
    "src/Data/Semigroup/Bitraversable.hs",
    "src/Data/Semigroup/Traversable.hs",
    "src/Data/Semigroup/Traversable/Class.hs",
    "src/Data/Semigroup/Foldable/Class.hs",
  ],
  src_strip_prefix = "src",
)
""",
)

new_http_archive(
  name = "hackage_free",
  urls = [
    "https://hackage.haskell.org/package/free-4.12.4/free-4.12.4.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/free-4.12.4.tar.gz",
  ],
  strip_prefix = "free-4.12.4",
  build_file = "hackage/free.BUILD",
)

new_http_archive(
  name = "hackage_adjunctions",
  urls = [
    "https://hackage.haskell.org/package/adjunctions-4.3/adjunctions-4.3.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/adjunctions-4.3.tar.gz",
  ],
  strip_prefix = "adjunctions-4.3",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "adjunctions",
  visibility = ["//visibility:public"],
  deps = [
    "@hackage_semigroupoids//:semigroupoids",
    "@hackage_free//:free",
    "@hackage_void//:void",
    "@hackage_comonad//:comonad",
    "@hackage_distributive//:distributive",
    "@hackage_semigroups//:semigroups",
    "@hackage_tagged//:tagged",
    "@hackage_contravariant//:contravariant",
    "@hackage_mtl//:mtl",
    "@hackage_transformers_compat//:transformers-compat",
    "@hackage_profunctors//:profunctors",
  ],
  prebuilt_dependencies = [
    "base",
    "array",
    "containers",
    "transformers",
  ],
  srcs = [
    "src/Control/Comonad/Representable/Store.hs",
    "src/Control/Comonad/Trans/Adjoint.hs",
    "src/Control/Monad/Representable/Reader.hs",
    "src/Control/Monad/Representable/State.hs",
    "src/Control/Monad/Trans/Adjoint.hs",
    "src/Control/Monad/Trans/Conts.hs",
    "src/Control/Monad/Trans/Contravariant/Adjoint.hs",
    "src/Data/Functor/Adjunction.hs",
    "src/Data/Functor/Rep.hs",
    "src/Data/Functor/Contravariant/Adjunction.hs",
    "src/Data/Functor/Contravariant/Rep.hs",
  ],
  src_strip_prefix = "src",
)
""",
)

new_http_archive(
  name = "hackage_kan_extensions",
  urls = [
    "https://hackage.haskell.org/package/kan-extensions-5.0.2/kan-extensions-5.0.2.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/kan-extensions-5.0.2.tar.gz",
  ],
  strip_prefix = "kan-extensions-5.0.2",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "kan-extensions",
  visibility = ["//visibility:public"],
  deps = [
    "@hackage_semigroupoids//:semigroupoids",
    "@hackage_free//:free",
    "@hackage_comonad//:comonad",
    "@hackage_adjunctions//:adjunctions",
    "@hackage_distributive//:distributive",
    "@hackage_tagged//:tagged",
    "@hackage_contravariant//:contravariant",
    "@hackage_mtl//:mtl",
  ],
  prebuilt_dependencies = [
    "base",
    "array",
    "containers",
    "transformers",
  ],
  srcs = [
    "src/Control/Comonad/Density.hs",
    "src/Control/Monad/Co.hs",
    "src/Control/Monad/Codensity.hs",
    "src/Data/Functor/Coyoneda.hs",
    "src/Data/Functor/Day.hs",
    "src/Data/Functor/Yoneda.hs",
    "src/Data/Functor/Contravariant/Coyoneda.hs",
    "src/Data/Functor/Contravariant/Day.hs",
    "src/Data/Functor/Contravariant/Yoneda.hs",
    "src/Data/Functor/Day/Curried.hs",
    "src/Data/Functor/Kan/Lan.hs",
    "src/Data/Functor/Kan/Ran.hs",
  ],
  src_strip_prefix = "src",
)
""",
)

new_http_archive(
  name = "hackage_reflection",
  urls = [
    "https://hackage.haskell.org/package/reflection-2.1.3/reflection-2.1.3.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/reflection-2.1.3.tar.gz",
  ],
  strip_prefix = "reflection-2.1.3",
  build_file = "hackage/reflection.BUILD",
)

new_http_archive(
  name = "hackage_lens",
  urls = [
    "https://hackage.haskell.org/package/lens-4.15.4/lens-4.15.4.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/lens-4.15.4.tar.gz",
  ],
  strip_prefix = "lens-4.15.4",
  build_file = "hackage/lens.BUILD",
)

new_http_archive(
  name = "hackage_lens_aeson",
  urls = [
    "https://hackage.haskell.org/package/lens-aeson-1.0.2/lens-aeson-1.0.2.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/lens-aeson-1.0.2.tar.gz",
  ],
  strip_prefix = "lens-aeson-1.0.2",
  build_file = "hackage/lens_aeson.BUILD",
)

new_http_archive(
  name = "hackage_blaze_builder",
  urls = [
    "https://hackage.haskell.org/package/blaze-builder-0.4.0.2/blaze-builder-0.4.0.2.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/blaze-builder-0.4.0.2.tar.gz",
  ],
  strip_prefix = "blaze-builder-0.4.0.2",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "blaze-builder",
  visibility = ["//visibility:public"],
  deps = [
    "@hackage_text//:text",
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
""",
)

new_http_archive(
  name = "hackage_case_insensitive",
  urls = [
    "https://hackage.haskell.org/package/case-insensitive-1.2.0.10/case-insensitive-1.2.0.10.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/case-insensitive-1.2.0.10.tar.gz",
  ],
  strip_prefix = "case-insensitive-1.2.0.10",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "case-insensitive",
  visibility = ["//visibility:public"],
  deps = [
    "@hackage_text//:text",
    "@hackage_hashable//:hashable",
  ],
  prebuilt_dependencies = [
    "bytestring",
    "base",
    "deepseq",
  ],
  srcs = [
    "Data/CaseInsensitive.hs",
    "Data/CaseInsensitive/Internal.hs",
    "Data/CaseInsensitive/Unsafe.hs",
  ],
)
""",
)

new_http_archive(
  name = "hackage_http_types",
  urls = [
    "https://hackage.haskell.org/package/http-types-0.9.1/http-types-0.9.1.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/http-types-0.9.1.tar.gz",
  ],
  strip_prefix = "http-types-0.9.1",
  build_file = "hackage/http_types.BUILD",
)

new_http_archive(
  name = "hackage_transformers_base",
  urls = [
    "https://hackage.haskell.org/package/transformers-base-0.4.4/transformers-base-0.4.4.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/transformers-base-0.4.4.tar.gz",
  ],
  strip_prefix = "transformers-base-0.4.4",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "transformers-base",
  visibility = ["//visibility:public"],
  deps = [
    "@hackage_stm//:stm",
    "@hackage_transformers_compat//:transformers-compat",
  ],
  prebuilt_dependencies = [
    "base",
    "transformers",
  ],
  srcs = [
    "src/Control/Monad/Base.hs",
  ],
  src_strip_prefix = "src",
)
""",
)

new_http_archive(
  name = "hackage_mmorph",
  urls = [
    "https://hackage.haskell.org/package/mmorph-1.1.0/mmorph-1.1.0.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/mmorph-1.1.0.tar.gz",
  ],
  strip_prefix = "mmorph-1.1.0",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "mmorph",
  visibility = ["//visibility:public"],
  deps = [
    "@hackage_mtl//:mtl",
    "@hackage_transformers_compat//:transformers-compat",
  ],
  prebuilt_dependencies = [
    "base",
    "transformers",
  ],
  srcs = [
    "src/Control/Monad/Morph.hs",
    "src/Control/Monad/Trans/Compose.hs",
  ],
  src_strip_prefix = "src",
)
""",
)

new_http_archive(
  name = "hackage_unliftio_core",
  urls = [
    "https://hackage.haskell.org/package/unliftio-core-0.1.1.0/unliftio-core-0.1.1.0.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/unliftio-core-0.1.1.0.tar.gz",
  ],
  strip_prefix = "unliftio-core-0.1.1.0",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "unliftio-core",
  visibility = ["//visibility:public"],
  deps = [
  ],
  prebuilt_dependencies = [
    "base",
    "transformers",
  ],
  srcs = [
    "src/Control/Monad/IO/Unlift.hs",
  ],
  src_strip_prefix = "src",
)
""",
)

new_http_archive(
  name = "hackage_monad_control",
  urls = [
    "https://hackage.haskell.org/package/monad-control-1.0.2.2/monad-control-1.0.2.2.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/monad-control-1.0.2.2.tar.gz",
  ],
  strip_prefix = "monad-control-1.0.2.2",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "monad-control",
  visibility = ["//visibility:public"],
  deps = [
    "@hackage_stm//:stm",
    "@hackage_transformers_base//:transformers-base",
    "@hackage_transformers_compat//:transformers-compat",
  ],
  prebuilt_dependencies = [
    "base",
    "transformers",
  ],
  srcs = [
    "Control/Monad/Trans/Control.hs",
  ],
)
""",
)

new_http_archive(
  name = "hackage_lifted_base",
  urls = [
    "https://hackage.haskell.org/package/lifted-base-0.2.3.11/lifted-base-0.2.3.11.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/lifted-base-0.2.3.11.tar.gz",
  ],
  strip_prefix = "lifted-base-0.2.3.11",
  build_file = "hackage/lifted_base.BUILD",
)

new_http_archive(
  name = "hackage_resourcet",
  urls = [
    "https://hackage.haskell.org/package/resourcet-1.1.11/resourcet-1.1.11.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/resourcet-1.1.11.tar.gz",
  ],
  strip_prefix = "resourcet-1.1.11",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "resourcet",
  visibility = ["//visibility:public"],
  deps = [
    "@hackage_exceptions//:exceptions",
    "@hackage_monad_control//:monad-control",
    "@hackage_lifted_base//:lifted-base",
    "@hackage_unliftio_core//:unliftio-core",
    "@hackage_mtl//:mtl",
    "@hackage_mmorph//:mmorph",
    "@hackage_transformers_base//:transformers-base",
    "@hackage_transformers_compat//:transformers-compat",
  ],
  prebuilt_dependencies = [
    "base",
    "containers",
    "transformers",
  ],
  srcs = [
    "Control/Monad/Trans/Resource.hs",
    "Control/Monad/Trans/Resource/Internal.hs",
    "Data/Acquire.hs",
    "Data/Acquire/Internal.hs",
    "UnliftIO/Resource.hs",
  ],
)
""",
)

new_http_archive(
  name = "hackage_parsec",
  urls = [
    "https://hackage.haskell.org/package/parsec-3.1.13.0/parsec-3.1.13.0.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/parsec-3.1.13.0.tar.gz",
  ],
  strip_prefix = "parsec-3.1.13.0",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "parsec",
  visibility = ["//visibility:public"],
  deps = [
    "@hackage_text//:text",
    "@hackage_mtl//:mtl",
  ],
  prebuilt_dependencies = [
    "bytestring",
    "base",
  ],
  srcs = [
    "src/Text/Parsec.hs",
    "src/Text/Parsec/String.hs",
    "src/Text/Parsec/ByteString.hs",
    "src/Text/Parsec/Text.hs",
    "src/Text/Parsec/Pos.hs",
    "src/Text/Parsec/Error.hs",
    "src/Text/Parsec/Prim.hs",
    "src/Text/Parsec/Char.hs",
    "src/Text/Parsec/Combinator.hs",
    "src/Text/Parsec/Token.hs",
    "src/Text/Parsec/Expr.hs",
    "src/Text/Parsec/Language.hs",
    "src/Text/Parsec/Perm.hs",
    "src/Text/Parsec/ByteString/Lazy.hs",
    "src/Text/Parsec/Text/Lazy.hs",
    "src/Text/ParserCombinators/Parsec.hs",
    "src/Text/ParserCombinators/Parsec/Char.hs",
    "src/Text/ParserCombinators/Parsec/Combinator.hs",
    "src/Text/ParserCombinators/Parsec/Error.hs",
    "src/Text/ParserCombinators/Parsec/Expr.hs",
    "src/Text/ParserCombinators/Parsec/Language.hs",
    "src/Text/ParserCombinators/Parsec/Perm.hs",
    "src/Text/ParserCombinators/Parsec/Pos.hs",
    "src/Text/ParserCombinators/Parsec/Prim.hs",
    "src/Text/ParserCombinators/Parsec/Token.hs",
  ],
  src_strip_prefix = "src",
)
""",
)

new_http_archive(
  name = "hackage_network_uri",
  urls = [
    "https://hackage.haskell.org/package/network-uri-2.6.1.0/network-uri-2.6.1.0.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/network-uri-2.6.1.0.tar.gz",
  ],
  strip_prefix = "network-uri-2.6.1.0",
  build_file = "hackage/network_uri.BUILD",
)

new_http_archive(
  name = "hackage_basement",
  urls = [
    "https://hackage.haskell.org/package/basement-0.0.4/basement-0.0.4.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/basement-0.0.4.tar.gz",
  ],
  strip_prefix = "basement-0.0.4",
  build_file = "hackage/basement.BUILD",
)

new_http_archive(
  name = "hackage_foundation",
  urls = [
    "https://hackage.haskell.org/package/foundation-0.0.17/foundation-0.0.17.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/foundation-0.0.17.tar.gz",
  ],
  strip_prefix = "foundation-0.0.17",
  build_file = "hackage/foundation.BUILD",
)

new_http_archive(
  name = "hackage_memory",
  urls = [
    "https://hackage.haskell.org/package/memory-0.14.11/memory-0.14.11.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/memory-0.14.11.tar.gz",
  ],
  strip_prefix = "memory-0.14.11",
  build_file = "hackage/memory.BUILD",
)

new_http_archive(
  name = "hackage_cryptonite",
  urls = [
    "https://hackage.haskell.org/package/cryptonite-0.24/cryptonite-0.24.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/cryptonite-0.24.tar.gz",
  ],
  strip_prefix = "cryptonite-0.24",
  build_file = "hackage/cryptonite.BUILD",
)

new_http_archive(
  name = "hackage_byteable",
  urls = [
    "https://hackage.haskell.org/package/byteable-0.1.1/byteable-0.1.1.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/byteable-0.1.1.tar.gz",
  ],
  strip_prefix = "byteable-0.1.1",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "byteable",
  visibility = ["//visibility:public"],
  deps = [
  ],
  prebuilt_dependencies = [
    "bytestring",
    "base",
  ],
  srcs = [
    "Data/Byteable.hs",
  ],
)
""",
)

new_http_archive(
  name = "hackage_hourglass",
  urls = [
    "https://hackage.haskell.org/package/hourglass-0.2.11/hourglass-0.2.11.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/hourglass-0.2.11.tar.gz",
  ],
  strip_prefix = "hourglass-0.2.11",
  build_file = "hackage/hourglass.BUILD",
)

new_http_archive(
  name = "hackage_asn1_types",
  urls = [
    "https://hackage.haskell.org/package/asn1-types-0.3.2/asn1-types-0.3.2.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/asn1-types-0.3.2.tar.gz",
  ],
  strip_prefix = "asn1-types-0.3.2",
  build_file_content = """
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
""",
)

new_http_archive(
  name = "hackage_asn1_encoding",
  urls = [
    "https://hackage.haskell.org/package/asn1-encoding-0.9.5/asn1-encoding-0.9.5.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/asn1-encoding-0.9.5.tar.gz",
  ],
  strip_prefix = "asn1-encoding-0.9.5",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "asn1-encoding",
  visibility = ["//visibility:public"],
  deps = [
    "@hackage_hourglass//:hourglass",
    "@hackage_asn1_types//:asn1-types",
  ],
  prebuilt_dependencies = [
    "bytestring",
    "base",
  ],
  srcs = [
    "Data/ASN1/Error.hs",
    "Data/ASN1/BinaryEncoding.hs",
    "Data/ASN1/BinaryEncoding/Raw.hs",
    "Data/ASN1/Encoding.hs",
    "Data/ASN1/Stream.hs",
    "Data/ASN1/Object.hs",
    "Data/ASN1/Prim.hs",
    "Data/ASN1/BinaryEncoding/Parse.hs",
    "Data/ASN1/BinaryEncoding/Writer.hs",
    "Data/ASN1/Internal.hs",
    "Data/ASN1/Serialize.hs",
    "Data/ASN1/Get.hs",
  ],
)
""",
)

new_http_archive(
  name = "hackage_asn1_parse",
  urls = [
    "https://hackage.haskell.org/package/asn1-parse-0.9.4/asn1-parse-0.9.4.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/asn1-parse-0.9.4.tar.gz",
  ],
  strip_prefix = "asn1-parse-0.9.4",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "asn1-parse",
  visibility = ["//visibility:public"],
  deps = [
    "@hackage_asn1_types//:asn1-types",
    "@hackage_asn1_encoding//:asn1-encoding",
  ],
  prebuilt_dependencies = [
    "bytestring",
    "base",
  ],
  srcs = [
    "Data/ASN1/Parse.hs",
  ],
)
""",
)

new_http_archive(
  name = "hackage_base64_bytestring",
  urls = [
    "https://hackage.haskell.org/package/base64-bytestring-1.0.0.1/base64-bytestring-1.0.0.1.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/base64-bytestring-1.0.0.1.tar.gz",
  ],
  strip_prefix = "base64-bytestring-1.0.0.1",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "base64-bytestring",
  visibility = ["//visibility:public"],
  deps = [
  ],
  prebuilt_dependencies = [
    "bytestring",
    "base",
  ],
  srcs = [
    "Data/ByteString/Base64.hs",
    "Data/ByteString/Base64/Internal.hs",
    "Data/ByteString/Base64/Lazy.hs",
    "Data/ByteString/Base64/URL.hs",
    "Data/ByteString/Base64/URL/Lazy.hs",
  ],
)
""",
)

new_http_archive(
  name = "hackage_pem",
  urls = [
    "https://hackage.haskell.org/package/pem-0.2.2/pem-0.2.2.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/pem-0.2.2.tar.gz",
  ],
  strip_prefix = "pem-0.2.2",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "pem",
  visibility = ["//visibility:public"],
  deps = [
    "@hackage_base64_bytestring//:base64-bytestring",
    "@hackage_mtl//:mtl",
  ],
  prebuilt_dependencies = [
    "bytestring",
    "base",
  ],
  srcs = [
    "Data/PEM.hs",
    "Data/PEM/Parser.hs",
    "Data/PEM/Types.hs",
    "Data/PEM/Writer.hs",
  ],
)
""",
)

new_http_archive(
  name = "hackage_x509",
  urls = [
    "https://hackage.haskell.org/package/x509-1.7.2/x509-1.7.2.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/x509-1.7.2.tar.gz",
  ],
  strip_prefix = "x509-1.7.2",
  build_file = "hackage/x509.BUILD",
)

new_http_archive(
  name = "hackage_x509_store",
  urls = [
    "https://hackage.haskell.org/package/x509-store-1.6.5/x509-store-1.6.5.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/x509-store-1.6.5.tar.gz",
  ],
  strip_prefix = "x509-store-1.6.5",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "x509-store",
  visibility = ["//visibility:public"],
  deps = [
    "@hackage_pem//:pem",
    "@hackage_asn1_types//:asn1-types",
    "@hackage_x509//:x509",
    "@hackage_cryptonite//:cryptonite",
    "@hackage_mtl//:mtl",
    "@hackage_asn1_encoding//:asn1-encoding",
  ],
  prebuilt_dependencies = [
    "bytestring",
    "base",
    "filepath",
    "containers",
    "directory",
  ],
  srcs = [
    "Data/X509/CertificateStore.hs",
    "Data/X509/File.hs",
    "Data/X509/Memory.hs",
  ],
)
""",
)

new_http_archive(
  name = "hackage_x509_system",
  urls = [
    "https://hackage.haskell.org/package/x509-system-1.6.6/x509-system-1.6.6.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/x509-system-1.6.6.tar.gz",
  ],
  strip_prefix = "x509-system-1.6.6",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "x509-system",
  visibility = ["//visibility:public"],
  deps = [
    "@hackage_pem//:pem",
    "@hackage_x509_store//:x509-store",
    "@hackage_x509//:x509",
    "@hackage_mtl//:mtl",
  ],
  prebuilt_dependencies = [
    "bytestring",
    "base",
    "filepath",
    "process",
    "containers",
    "directory",
  ],
  srcs = [
    "System/X509.hs",
    "System/X509/Unix.hs",
    "System/X509/MacOS.hs",
  ],
)
""",
)

new_http_archive(
  name = "hackage_network",
  urls = [
    "https://hackage.haskell.org/package/network-2.6.3.3/network-2.6.3.3.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/network-2.6.3.3.tar.gz",
  ],
  strip_prefix = "network-2.6.3.3",
  build_file = "hackage/network.BUILD",
)

new_http_archive(
  name = "hackage_async",
  urls = [
    "https://hackage.haskell.org/package/async-2.1.1.1/async-2.1.1.1.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/async-2.1.1.1.tar.gz",
  ],
  strip_prefix = "async-2.1.1.1",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "async",
  visibility = ["//visibility:public"],
  deps = [
    "@hackage_stm//:stm",
  ],
  prebuilt_dependencies = [
    "base",
  ],
  srcs = [
    "Control/Concurrent/Async.hs",
  ],
)
""",
)

new_http_archive(
  name = "hackage_x509_validation",
  urls = [
    "https://hackage.haskell.org/package/x509-validation-1.6.9/x509-validation-1.6.9.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/x509-validation-1.6.9.tar.gz",
  ],
  strip_prefix = "x509-validation-1.6.9",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "x509-validation",
  visibility = ["//visibility:public"],
  deps = [
    "@hackage_hourglass//:hourglass",
    "@hackage_data_default_class//:data-default-class",
    "@hackage_pem//:pem",
    "@hackage_asn1_types//:asn1-types",
    "@hackage_x509_store//:x509-store",
    "@hackage_memory//:memory",
    "@hackage_x509//:x509",
    "@hackage_cryptonite//:cryptonite",
    "@hackage_mtl//:mtl",
    "@hackage_byteable//:byteable",
    "@hackage_asn1_encoding//:asn1-encoding",
  ],
  prebuilt_dependencies = [
    "bytestring",
    "base",
    "containers",
  ],
  srcs = [
    "Data/X509/Validation.hs",
    "Data/X509/Validation/Signature.hs",
    "Data/X509/Validation/Fingerprint.hs",
    "Data/X509/Validation/Cache.hs",
    "Data/X509/Validation/Types.hs",
  ],
)
""",
)

new_http_archive(
  name = "hackage_cereal",
  urls = [
    "https://hackage.haskell.org/package/cereal-0.5.5.0/cereal-0.5.5.0.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/cereal-0.5.5.0.tar.gz",
  ],
  strip_prefix = "cereal-0.5.5.0",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "cereal",
  visibility = ["//visibility:public"],
  deps = [
  ],
  prebuilt_dependencies = [
    "bytestring",
    "base",
    "array",
    "containers",
    "ghc-prim",
  ],
  srcs = [
    "src/Data/Serialize.hs",
    "src/Data/Serialize/IEEE754.hs",
    "src/Data/Serialize/Get.hs",
    "src/Data/Serialize/Put.hs",
  ],
  src_strip_prefix = "src",
)
""",
)

new_http_archive(
  name = "hackage_tls",
  urls = [
    "https://hackage.haskell.org/package/tls-1.4.0/tls-1.4.0.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/tls-1.4.0.tar.gz",
  ],
  strip_prefix = "tls-1.4.0",
  build_file = "hackage/tls.BUILD",
)

new_http_archive(
  name = "hackage_socks",
  urls = [
    "https://hackage.haskell.org/package/socks-0.5.6/socks-0.5.6.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/socks-0.5.6.tar.gz",
  ],
  strip_prefix = "socks-0.5.6",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "socks",
  visibility = ["//visibility:public"],
  deps = [
    "@hackage_cereal//:cereal",
    "@hackage_network//:network",
  ],
  prebuilt_dependencies = [
    "bytestring",
    "base",
  ],
  srcs = [
    "Network/Socks5.hs",
    "Network/Socks5/Lowlevel.hs",
    "Network/Socks5/Types.hs",
    "Network/Socks5/Wire.hs",
    "Network/Socks5/Conf.hs",
    "Network/Socks5/Command.hs",
    "Network/Socks5/Parse.hs",
  ],
)
""",
)

new_http_archive(
  name = "hackage_connection",
  urls = [
    "https://hackage.haskell.org/package/connection-0.2.8/connection-0.2.8.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/connection-0.2.8.tar.gz",
  ],
  strip_prefix = "connection-0.2.8",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "connection",
  visibility = ["//visibility:public"],
  deps = [
    "@hackage_socks//:socks",
    "@hackage_x509_validation//:x509-validation",
    "@hackage_data_default_class//:data-default-class",
    "@hackage_tls//:tls",
    "@hackage_network//:network",
    "@hackage_x509_store//:x509-store",
    "@hackage_x509//:x509",
    "@hackage_x509_system//:x509-system",
    "@hackage_byteable//:byteable",
  ],
  prebuilt_dependencies = [
    "bytestring",
    "base",
    "containers",
  ],
  srcs = [
    "Network/Connection.hs",
    "Network/Connection/Types.hs",
  ],
)
""",
)

new_http_archive(
  name = "hackage_mime_types",
  urls = [
    "https://hackage.haskell.org/package/mime-types-0.1.0.7/mime-types-0.1.0.7.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/mime-types-0.1.0.7.tar.gz",
  ],
  strip_prefix = "mime-types-0.1.0.7",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "mime-types",
  visibility = ["//visibility:public"],
  deps = [
    "@hackage_text//:text",
  ],
  prebuilt_dependencies = [
    "bytestring",
    "base",
    "containers",
  ],
  srcs = [
    "Network/Mime.hs",
  ],
)
""",
)

new_http_archive(
  name = "hackage_zlib",
  urls = [
    "https://hackage.haskell.org/package/zlib-0.6.1.2/zlib-0.6.1.2.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/zlib-0.6.1.2.tar.gz",
  ],
  strip_prefix = "zlib-0.6.1.2",
  build_file = "hackage/zlib.BUILD",
)

new_http_archive(
  name = "hackage_streaming_commons",
  urls = [
    "https://hackage.haskell.org/package/streaming-commons-0.1.19/streaming-commons-0.1.19.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/streaming-commons-0.1.19.tar.gz",
  ],
  strip_prefix = "streaming-commons-0.1.19",
  build_file = "hackage/streaming_commons.BUILD",
)

new_http_archive(
  name = "hackage_old_locale",
  urls = [
    "https://hackage.haskell.org/package/old-locale-1.0.0.7/old-locale-1.0.0.7.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/old-locale-1.0.0.7.tar.gz",
  ],
  strip_prefix = "old-locale-1.0.0.7",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "old-locale",
  visibility = ["//visibility:public"],
  deps = [
  ],
  prebuilt_dependencies = [
    "base",
  ],
  srcs = [
    "System/Locale.hs",
  ],
)
""",
)

new_http_archive(
  name = "hackage_cookie",
  urls = [
    "https://hackage.haskell.org/package/cookie-0.4.3/cookie-0.4.3.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/cookie-0.4.3.tar.gz",
  ],
  strip_prefix = "cookie-0.4.3",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "cookie",
  visibility = ["//visibility:public"],
  deps = [
    "@hackage_data_default_class//:data-default-class",
    "@hackage_text//:text",
    "@hackage_blaze_builder//:blaze-builder",
    "@hackage_old_locale//:old-locale",
  ],
  prebuilt_dependencies = [
    "bytestring",
    "base",
    "time",
    "deepseq",
  ],
  srcs = [
    "Web/Cookie.hs",
  ],
)
""",
)

new_http_archive(
  name = "hackage_http_client",
  urls = [
    "https://hackage.haskell.org/package/http-client-0.5.10/http-client-0.5.10.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/http-client-0.5.10.tar.gz",
  ],
  strip_prefix = "http-client-0.5.10",
  build_file = "hackage/http_client.BUILD",
)

new_http_archive(
  name = "hackage_http_client_tls",
  urls = [
    "https://hackage.haskell.org/package/http-client-tls-0.3.5.3/http-client-tls-0.3.5.3.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/http-client-tls-0.3.5.3.tar.gz",
  ],
  strip_prefix = "http-client-tls-0.3.5.3",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "http-client-tls",
  visibility = ["//visibility:public"],
  deps = [
    "@hackage_http_client//:http-client",
    "@hackage_exceptions//:exceptions",
    "@hackage_case_insensitive//:case-insensitive",
    "@hackage_data_default_class//:data-default-class",
    "@hackage_text//:text",
    "@hackage_tls//:tls",
    "@hackage_network//:network",
    "@hackage_connection//:connection",
    "@hackage_memory//:memory",
    "@hackage_cryptonite//:cryptonite",
    "@hackage_network_uri//:network-uri",
    "@hackage_http_types//:http-types",
  ],
  prebuilt_dependencies = [
    "bytestring",
    "base",
    "containers",
    "transformers",
  ],
  srcs = [
    "Network/HTTP/Client/TLS.hs",
  ],
)
""",
)

new_http_archive(
  name = "hackage_typed_process",
  urls = [
    "https://hackage.haskell.org/package/typed-process-0.2.1.0/typed-process-0.2.1.0.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/typed-process-0.2.1.0.tar.gz",
  ],
  strip_prefix = "typed-process-0.2.1.0",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "typed-process",
  visibility = ["//visibility:public"],
  deps = [
    "@hackage_stm//:stm",
    "@hackage_async//:async",
  ],
  prebuilt_dependencies = [
    "bytestring",
    "base",
    "process",
    "transformers",
  ],
  srcs = [
    "src/System/Process/Typed.hs",
  ],
  src_strip_prefix = "src",
)
""",
)

new_http_archive(
  name = "hackage_conduit",
  urls = [
    "https://hackage.haskell.org/package/conduit-1.2.13/conduit-1.2.13.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/conduit-1.2.13.tar.gz",
  ],
  strip_prefix = "conduit-1.2.13",
  build_file = "hackage/conduit.BUILD",
)

new_http_archive(
  name = "hackage_conduit_extra",
  urls = [
    "https://hackage.haskell.org/package/conduit-extra-1.2.3.2/conduit-extra-1.2.3.2.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/conduit-extra-1.2.3.2.tar.gz",
  ],
  strip_prefix = "conduit-extra-1.2.3.2",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "conduit-extra",
  visibility = ["//visibility:public"],
  deps = [
    "@hackage_exceptions//:exceptions",
    "@hackage_streaming_commons//:streaming-commons",
    "@hackage_stm//:stm",
    "@hackage_text//:text",
    "@hackage_monad_control//:monad-control",
    "@hackage_network//:network",
    "@hackage_async//:async",
    "@hackage_conduit//:conduit",
    "@hackage_blaze_builder//:blaze-builder",
    "@hackage_unliftio_core//:unliftio-core",
    "@hackage_typed_process//:typed-process",
    "@hackage_transformers_base//:transformers-base",
    "@hackage_attoparsec//:attoparsec",
    "@hackage_resourcet//:resourcet",
    "@hackage_primitive//:primitive",
  ],
  prebuilt_dependencies = [
    "bytestring",
    "base",
    "filepath",
    "process",
    "transformers",
    "directory",
  ],
  srcs = [
    "Data/Conduit/Attoparsec.hs",
    "Data/Conduit/Binary.hs",
    "Data/Conduit/Blaze.hs",
    "Data/Conduit/ByteString/Builder.hs",
    "Data/Conduit/Filesystem.hs",
    "Data/Conduit/Foldl.hs",
    "Data/Conduit/Lazy.hs",
    "Data/Conduit/Network.hs",
    "Data/Conduit/Network/UDP.hs",
    "Data/Conduit/Process.hs",
    "Data/Conduit/Text.hs",
    "Data/Conduit/Zlib.hs",
    "Data/Conduit/Network/Unix.hs",
    "Data/Conduit/Process/Typed.hs",
  ],
)
""",
)

new_http_archive(
  name = "hackage_http_conduit",
  urls = [
    "https://hackage.haskell.org/package/http-conduit-2.2.4/http-conduit-2.2.4.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/http-conduit-2.2.4.tar.gz",
  ],
  strip_prefix = "http-conduit-2.2.4",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "http-conduit",
  visibility = ["//visibility:public"],
  deps = [
    "@hackage_http_client//:http-client",
    "@hackage_exceptions//:exceptions",
    "@hackage_monad_control//:monad-control",
    "@hackage_lifted_base//:lifted-base",
    "@hackage_conduit//:conduit",
    "@hackage_conduit_extra//:conduit-extra",
    "@hackage_http_client_tls//:http-client-tls",
    "@hackage_mtl//:mtl",
    "@hackage_resourcet//:resourcet",
    "@hackage_http_types//:http-types",
    "@hackage_aeson//:aeson",
  ],
  prebuilt_dependencies = [
    "bytestring",
    "base",
    "transformers",
  ],
  srcs = [
    "Network/HTTP/Conduit.hs",
    "Network/HTTP/Client/Conduit.hs",
    "Network/HTTP/Simple.hs",
  ],
)
""",
)

new_http_archive(
  name = "hackage_colour",
  urls = [
    "https://hackage.haskell.org/package/colour-2.3.4/colour-2.3.4.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/colour-2.3.4.tar.gz",
  ],
  strip_prefix = "colour-2.3.4",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "colour",
  visibility = ["//visibility:public"],
  deps = [
  ],
  prebuilt_dependencies = [
    "base",
  ],
  srcs = [
    "Data/Colour/Names.hs",
    "Data/Colour/RGB.hs",
    "Data/Colour/Matrix.hs",
    "Data/Colour/Internal.hs",
    "Data/Colour/CIE/Chromaticity.hs",
    "Data/Colour/CIE/Illuminant.hs",
    "Data/Colour/SRGB/Linear.hs",
    "Data/Colour/RGBSpace.hs",
    "Data/Colour/RGBSpace/HSV.hs",
    "Data/Colour/RGBSpace/HSL.hs",
    "Data/Colour/CIE.hs",
    "Data/Colour/SRGB.hs",
    "Data/Colour/Chan.hs",
    "Data/Colour.hs",
  ],
)
""",
)

new_http_archive(
  name = "hackage_ansi_terminal",
  urls = [
    "https://hackage.haskell.org/package/ansi-terminal-0.7.1.1/ansi-terminal-0.7.1.1.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/ansi-terminal-0.7.1.1.tar.gz",
  ],
  strip_prefix = "ansi-terminal-0.7.1.1",
  build_file = "hackage/ansi_terminal.BUILD",
)

new_http_archive(
  name = "hackage_ansi_wl_pprint",
  urls = [
    "https://hackage.haskell.org/package/ansi-wl-pprint-0.6.8.2/ansi-wl-pprint-0.6.8.2.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/ansi-wl-pprint-0.6.8.2.tar.gz",
  ],
  strip_prefix = "ansi-wl-pprint-0.6.8.2",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "ansi-wl-pprint",
  visibility = ["//visibility:public"],
  deps = [
    "@hackage_ansi_terminal//:ansi-terminal",
  ],
  prebuilt_dependencies = [
    "base",
  ],
  srcs = [
    "Text/PrettyPrint/ANSI/Leijen.hs",
    "Text/PrettyPrint/ANSI/Leijen/Internal.hs",
  ],
)
""",
)

new_http_archive(
  name = "hackage_optparse_applicative",
  urls = [
    "https://hackage.haskell.org/package/optparse-applicative-0.14.0.0/optparse-applicative-0.14.0.0.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/optparse-applicative-0.14.0.0.tar.gz",
  ],
  strip_prefix = "optparse-applicative-0.14.0.0",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "optparse-applicative",
  visibility = ["//visibility:public"],
  deps = [
    "@hackage_ansi_wl_pprint//:ansi-wl-pprint",
    "@hackage_transformers_compat//:transformers-compat",
  ],
  prebuilt_dependencies = [
    "base",
    "process",
    "transformers",
  ],
  srcs = [
    "Options/Applicative.hs",
    "Options/Applicative/Arrows.hs",
    "Options/Applicative/BashCompletion.hs",
    "Options/Applicative/Builder.hs",
    "Options/Applicative/Common.hs",
    "Options/Applicative/Extra.hs",
    "Options/Applicative/Help.hs",
    "Options/Applicative/Internal.hs",
    "Options/Applicative/Types.hs",
    "Options/Applicative/Builder/Completer.hs",
    "Options/Applicative/Builder/Internal.hs",
    "Options/Applicative/Help/Chunk.hs",
    "Options/Applicative/Help/Core.hs",
    "Options/Applicative/Help/Levenshtein.hs",
    "Options/Applicative/Help/Pretty.hs",
    "Options/Applicative/Help/Types.hs",
  ],
)
""",
)
