# http_archive(
#   name = "io_tweag_rules_haskell",
#   strip_prefix = "rules_haskell-0.5",
#   urls = ["https://github.com/tweag/rules_haskell/archive/v0.5.tar.gz"],
#   sha256 = "0296c56ddca2dae172eccdecded815aea45985fa3cdd6d66ab392011beb89cdd",
# )

local_repository(
  name = "io_tweag_rules_haskell",
  path = "../../repos/rules_haskell",
)

http_archive(
  name = "io_tweag_rules_nixpkgs",
  strip_prefix = "rules_nixpkgs-0.2.2",
  urls = ["https://github.com/tweag/rules_nixpkgs/archive/v0.2.2.tar.gz"],
)

load("@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl", "nixpkgs_package")
load("@io_tweag_rules_haskell//haskell:repositories.bzl", "haskell_repositories")

haskell_repositories()

load("@io_tweag_rules_haskell//haskell:haskell.bzl", "ghc_bindist")

# Take GHC from the nixpkgs package. Requires nix to be installed on the host
# though.
nixpkgs_package(
  name = "ghc",
  attribute_path = "haskell.compiler.ghc822"
)

register_toolchains("//:ghc")

new_http_archive(
  name = "hackage_time_locale_compat",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/time-locale-compat-0.1.1.3/time-locale-compat-0.1.1.3.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/time-locale-compat-0.1.1.3.tar.gz",
  ],
  # sha256 = "3cda822de75ffaf6de9ff294ab91c7529b82463b7162bc67a1638f00ce8924e1",
  strip_prefix = "time-locale-compat-0.1.1.3",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "time-locale-compat",
  visibility = ["//visibility:public"],
  srcs = [
    "src/Data/Time/Locale/Compat.hs",
  ],
  src_strip_prefix = "src",
  deps = [
  ],
  prebuilt_dependencies = [
    "base",
    "time",
  ],
)
  """,
)

new_http_archive(
  name = "hackage_primitive",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/primitive-0.6.3.0/primitive-0.6.3.0.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/primitive-0.6.3.0.tar.gz",
  ],
  # sha256 = "97fe680ea7158dce999b54ea50865a10c5f907b4d416e6d88eb0f808f828cbdd",
  strip_prefix = "primitive-0.6.3.0",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "primitive",
  visibility = ["//visibility:public"],
  srcs = [
    "Data/Primitive.hs",
    "Data/Primitive/Array.hs",
    "Data/Primitive/MutVar.hs",
    "Data/Primitive/Addr.hs",
    "Data/Primitive/UnliftedArray.hs",
    "Data/Primitive/Types.hs",
    "Data/Primitive/ByteArray.hs",
    "Data/Primitive/SmallArray.hs",
    "Data/Primitive/MachDeps.hs",
    "Data/Primitive/Internal/Compat.hs",
    "Data/Primitive/Internal/Operations.hs",
    "Control/Monad/Primitive.hs",
  ],
  deps = [
  ],
  prebuilt_dependencies = [
    "base",
    "ghc-prim",
    "transformers",
  ],
)
  """,
)

new_http_archive(
  name = "hackage_vector",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/vector-0.12.0.1/vector-0.12.0.1.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/vector-0.12.0.1.tar.gz",
  ],
  # sha256 = "ded8b0accf2e19bb936d344768380c6d3a25f0acd34e832e64568da3f244f96f",
  strip_prefix = "vector-0.12.0.1",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "vector",
  visibility = ["//visibility:public"],
  srcs = [
    "Data/Vector.hs",
    "Data/Vector/Unboxed.hs",
    "Data/Vector/Generic.hs",
    "Data/Vector/Primitive.hs",
    "Data/Vector/Storable.hs",
    "Data/Vector/Mutable.hs",
    "Data/Vector/Storable/Internal.hs",
    "Data/Vector/Storable/Mutable.hs",
    "Data/Vector/Fusion/Bundle.hs",
    "Data/Vector/Fusion/Util.hs",
    "Data/Vector/Fusion/Bundle/Monadic.hs",
    "Data/Vector/Fusion/Bundle/Size.hs",
    "Data/Vector/Fusion/Stream/Monadic.hs",
    "Data/Vector/Generic/New.hs",
    "Data/Vector/Generic/Base.hs",
    "Data/Vector/Generic/Mutable.hs",
    "Data/Vector/Generic/Mutable/Base.hs",
    "Data/Vector/Internal/Check.hs",
    "Data/Vector/Unboxed/Base.hs",
    "Data/Vector/Unboxed/Mutable.hs",
    "Data/Vector/Primitive/Mutable.hs",
  ],
  deps = [
    "@hackage_primitive//:primitive",
  ],
  prebuilt_dependencies = [
    "base",
    "ghc-prim",
    "deepseq",
  ],
)
  """,
)

new_http_archive(
  name = "hackage_text",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/text-1.2.2.2/text-1.2.2.2.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/text-1.2.2.2.tar.gz",
  ],
  # sha256 = "11efccd291fb4ab4de2a20ad63d550422d30a33ba7a5963d3b8aff66dcd6cc77",
  strip_prefix = "text-1.2.2.2",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "text",
  visibility = ["//visibility:public"],
  srcs = [
    "Data/Text.hs",
    "Data/Text/Array.hs",
    "Data/Text/Encoding.hs",
    "Data/Text/Foreign.hs",
    "Data/Text/Internal.hs",
    "Data/Text/IO.hs",
    "Data/Text/Lazy.hs",
    "Data/Text/Read.hs",
    "Data/Text/Show.hs",
    "Data/Text/Unsafe.hs",
    "Data/Text/Encoding/Error.hs",
    "Data/Text/Internal/Builder.hs",
    "Data/Text/Internal/Functions.hs",
    "Data/Text/Internal/Fusion.hs",
    "Data/Text/Internal/IO.hs",
    "Data/Text/Internal/Lazy.hs",
    "Data/Text/Internal/Private.hs",
    "Data/Text/Internal/Read.hs",
    "Data/Text/Internal/Search.hs",
    "Data/Text/Internal/Unsafe.hs",
    "Data/Text/Internal/Builder/Functions.hs",
    "Data/Text/Internal/Builder/Int/Digits.hs",
    "Data/Text/Internal/Builder/RealFloat/Functions.hs",
    "Data/Text/Internal/Encoding/Fusion.hs",
    "Data/Text/Internal/Encoding/Utf16.hs",
    "Data/Text/Internal/Encoding/Utf32.hs",
    "Data/Text/Internal/Encoding/Utf8.hs",
    "Data/Text/Internal/Encoding/Fusion/Common.hs",
    "Data/Text/Internal/Fusion/CaseMapping.hs",
    "Data/Text/Internal/Fusion/Common.hs",
    "Data/Text/Internal/Fusion/Size.hs",
    "Data/Text/Internal/Fusion/Types.hs",
    "Data/Text/Internal/Lazy/Fusion.hs",
    "Data/Text/Internal/Lazy/Search.hs",
    "Data/Text/Internal/Lazy/Encoding/Fusion.hs",
    "Data/Text/Internal/Unsafe/Char.hs",
    "Data/Text/Internal/Unsafe/Shift.hs",
    "Data/Text/Lazy/Builder.hs",
    "Data/Text/Lazy/Encoding.hs",
    "Data/Text/Lazy/Internal.hs",
    "Data/Text/Lazy/IO.hs",
    "Data/Text/Lazy/Read.hs",
    "Data/Text/Lazy/Builder/Int.hs",
    "Data/Text/Lazy/Builder/RealFloat.hs",
  ],
  deps = [
  ],
  prebuilt_dependencies = [
    "bytestring",
    "base",
    "array",
    "integer-gmp",
    "binary",
    "ghc-prim",
    "deepseq",
  ],
)
  """,
)

new_http_archive(
  name = "hackage_hashable",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/hashable-1.2.6.1/hashable-1.2.6.1.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/hashable-1.2.6.1.tar.gz",
  ],
  # sha256 = "56dadd02b530fceee46c5e9dc3c6fb3f81963c58dadb3df812d312a0b4298770",
  strip_prefix = "hashable-1.2.6.1",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "hashable",
  visibility = ["//visibility:public"],
  srcs = [
    "Data/Hashable.hs",
    "Data/Hashable/Generic.hs",
    "Data/Hashable/Class.hs",
    "Data/Hashable/Lifted.hs",
    "Data/Hashable/RandomSource.hs",
    "Data/Hashable/SipHash.hs",
  ],
  deps = [
    "@hackage_text//:text",
  ],
  prebuilt_dependencies = [
    "bytestring",
    "base",
    "integer-gmp",
    "ghc-prim",
    "deepseq",
  ],
)
  """,
)

new_http_archive(
  name = "hackage_integer_logarithms",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/integer-logarithms-1.0.2/integer-logarithms-1.0.2.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/integer-logarithms-1.0.2.tar.gz",
  ],
  # sha256 = "086b6cfe58d0fbf5e6d4e417939c9da59607c8c6b4446b8f1a95656c1a688c69",
  strip_prefix = "integer-logarithms-1.0.2",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "integer-logarithms",
  visibility = ["//visibility:public"],
  srcs = [
    "src/GHC/Integer/Logarithms/Compat.hs",
    "src/Math/NumberTheory/Logarithms.hs",
    "src/Math/NumberTheory/Powers/Natural.hs",
    "src/Math/NumberTheory/Powers/Integer.hs",
  ],
  src_strip_prefix = "src",
  deps = [
  ],
  prebuilt_dependencies = [
    "base",
    "array",
    "integer-gmp",
    "ghc-prim",
  ],
)
  """,
)

new_http_archive(
  name = "hackage_scientific",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/scientific-0.3.5.2/scientific-0.3.5.2.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/scientific-0.3.5.2.tar.gz",
  ],
  # sha256 = "8a2265b27b109060abbe1e2273c182cacc437ea949d18a4c265850c4d3321581",
  strip_prefix = "scientific-0.3.5.2",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "scientific",
  visibility = ["//visibility:public"],
  srcs = [
    "src/Data/Scientific.hs",
    "src/Data/ByteString/Builder/Scientific.hs",
    "src/Data/Text/Lazy/Builder/Scientific.hs",
  ],
  src_strip_prefix = "src",
  deps = [
    "@hackage_integer_logarithms//:integer-logarithms",
    "@hackage_text//:text",
    "@hackage_hashable//:hashable",
    "@hackage_primitive//:primitive",
  ],
  prebuilt_dependencies = [
    "bytestring",
    "base",
    "integer-gmp",
    "containers",
    "binary",
    "deepseq",
  ],
)
  """,
)

new_http_archive(
  name = "hackage_random",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/random-1.1/random-1.1.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/random-1.1.tar.gz",
  ],
  # sha256 = "7b67624fd76ddf97c206de0801dc7e888097e9d572974be9b9ea6551d76965df",
  strip_prefix = "random-1.1",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "random",
  visibility = ["//visibility:public"],
  srcs = [
    "System/Random.hs",
  ],
  deps = [
  ],
  prebuilt_dependencies = [
    "base",
    "time",
  ],
)
  """,
)

new_http_archive(
  name = "hackage_uuid_types",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/uuid-types-1.0.3/uuid-types-1.0.3.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/uuid-types-1.0.3.tar.gz",
  ],
  # sha256 = "01887ed945e74c3c361b00700bd9aeead37d1124d39c0d4f190f89fb0e909c47",
  strip_prefix = "uuid-types-1.0.3",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "uuid-types",
  visibility = ["//visibility:public"],
  srcs = [
    "Data/UUID/Types.hs",
    "Data/UUID/Types/Internal.hs",
    "Data/UUID/Types/Internal/Builder.hs",
  ],
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
)
  """,
)

new_http_archive(
  name = "hackage_attoparsec",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/attoparsec-0.13.2.2/attoparsec-0.13.2.2.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/attoparsec-0.13.2.2.tar.gz",
  ],
  # sha256 = "69447dd3b077f7f89c2c25731e9c658a4f5a0121b77e41b6467a865143ff425b",
  strip_prefix = "attoparsec-0.13.2.2",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "attoparsec",
  visibility = ["//visibility:public"],
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
)
  """,
)

new_http_archive(
  name = "hackage_base_compat",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/base-compat-0.9.3/base-compat-0.9.3.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/base-compat-0.9.3.tar.gz",
  ],
  # sha256 = "66691ca16b7ceb8c38bd2bac6ffe8e0eee25f408020cb8150a0444f8824e7458",
  strip_prefix = "base-compat-0.9.3",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "base-compat",
  visibility = ["//visibility:public"],
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
  deps = [
  ],
  prebuilt_dependencies = [
    "unix",
    "base",
  ],
)
  """,
)

new_http_archive(
  name = "hackage_transformers_compat",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/transformers-compat-0.5.1.4/transformers-compat-0.5.1.4.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/transformers-compat-0.5.1.4.tar.gz",
  ],
  # sha256 = "1b4bfa8589afb1ca0e719129ab261bd90ef0cc3e6c0b9963f94970c082b61250",
  strip_prefix = "transformers-compat-0.5.1.4",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "transformers-compat",
  visibility = ["//visibility:public"],
  srcs = [
    "src/Control/Monad/Trans/Instances.hs",
  ],
  src_strip_prefix = "src",
  deps = [
  ],
  prebuilt_dependencies = [
    "base",
    "ghc-prim",
    "transformers",
  ],
)
  """,
)

new_http_archive(
  name = "hackage_tagged",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/tagged-0.8.5/tagged-0.8.5.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/tagged-0.8.5.tar.gz",
  ],
  # sha256 = "4545e05de52ad8f6c95d88cea4b260b30a463b77966eba45319a2c8099b35364",
  strip_prefix = "tagged-0.8.5",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "tagged",
  visibility = ["//visibility:public"],
  srcs = [
    "src/Data/Tagged.hs",
    "src/Data/Proxy/TH.hs",
  ],
  src_strip_prefix = "src",
  deps = [
    "@hackage_transformers_compat//:transformers-compat",
  ],
  prebuilt_dependencies = [
    "base",
    "transformers",
    "deepseq",
    "template-haskell",
  ],
)
  """,
)

new_http_archive(
  name = "hackage_th_abstraction",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/th-abstraction-0.2.6.0/th-abstraction-0.2.6.0.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/th-abstraction-0.2.6.0.tar.gz",
  ],
  # sha256 = "f515457800ed884a4211753c85c9fe70fa7d73095c86d5da043f06a7f7935773",
  strip_prefix = "th-abstraction-0.2.6.0",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "th-abstraction",
  visibility = ["//visibility:public"],
  srcs = [
    "src/Language/Haskell/TH/Datatype.hs",
    "src/Language/Haskell/TH/Datatype/Internal.hs",
  ],
  src_strip_prefix = "src",
  deps = [
  ],
  prebuilt_dependencies = [
    "base",
    "containers",
    "ghc-prim",
    "template-haskell",
  ],
)
  """,
)

new_http_archive(
  name = "hackage_dlist",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/dlist-0.8.0.4/dlist-0.8.0.4.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/dlist-0.8.0.4.tar.gz",
  ],
  # sha256 = "3ebbb7022246f446fc625b77d0e1f509d66eedf1deb22bbd6df5c8450aaf923a",
  strip_prefix = "dlist-0.8.0.4",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "dlist",
  visibility = ["//visibility:public"],
  srcs = [
    "Data/DList.hs",
  ],
  deps = [
  ],
  prebuilt_dependencies = [
    "base",
    "deepseq",
  ],
)
  """,
)

new_http_archive(
  name = "hackage_unordered_containers",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/unordered-containers-0.2.8.0/unordered-containers-0.2.8.0.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/unordered-containers-0.2.8.0.tar.gz",
  ],
  # sha256 = "aea434c2e3a4c6de9c656451577d21eabf28acb44b7c4f033a8fe0187ba45667",
  strip_prefix = "unordered-containers-0.2.8.0",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "unordered-containers",
  visibility = ["//visibility:public"],
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
  deps = [
    "@hackage_hashable//:hashable",
  ],
  prebuilt_dependencies = [
    "base",
    "deepseq",
  ],
)
  """,
)

new_http_archive(
  name = "hackage_aeson",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/aeson-1.2.4.0/aeson-1.2.4.0.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/aeson-1.2.4.0.tar.gz",
  ],
  # sha256 = "2ff56812644c3e272fef6b19aaff1ddab81c9984b1c1f8969521cb856bc22eab",
  strip_prefix = "aeson-1.2.4.0",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "aeson",
  visibility = ["//visibility:public"],
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
  deps = [
    "@hackage_unordered_containers//:unordered-containers",
    "@hackage_text//:text",
    "@hackage_dlist//:dlist",
    "@hackage_th_abstraction//:th-abstraction",
    "@hackage_tagged//:tagged",
    "@hackage_base_compat//:base-compat",
    "@hackage_hashable//:hashable",
    "@hackage_attoparsec//:attoparsec",
    "@hackage_uuid_types//:uuid-types",
    "@hackage_scientific//:scientific",
    "@hackage_vector//:vector",
    "@hackage_time_locale_compat//:time-locale-compat",
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
)
  """,
)

new_http_archive(
  name = "hackage_blaze_builder",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/blaze-builder-0.4.0.2/blaze-builder-0.4.0.2.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/blaze-builder-0.4.0.2.tar.gz",
  ],
  # sha256 = "a35663de7816bfc8faaee5fcd2b0fa400df4881d7a09f6511e6c74b4dcb019d4",
  strip_prefix = "blaze-builder-0.4.0.2",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "blaze-builder",
  visibility = ["//visibility:public"],
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
  deps = [
    "@hackage_text//:text",
  ],
  prebuilt_dependencies = [
    "bytestring",
    "base",
    "deepseq",
  ],
)
  """,
)

new_http_archive(
  name = "hackage_case_insensitive",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/case-insensitive-1.2.0.10/case-insensitive-1.2.0.10.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/case-insensitive-1.2.0.10.tar.gz",
  ],
  # sha256 = "d11db9c3854b28b816010110564c1e65cedadd262201479b879e1efe4e147d94",
  strip_prefix = "case-insensitive-1.2.0.10",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "case-insensitive",
  visibility = ["//visibility:public"],
  srcs = [
    "Data/CaseInsensitive.hs",
    "Data/CaseInsensitive/Internal.hs",
    "Data/CaseInsensitive/Unsafe.hs",
  ],
  deps = [
    "@hackage_text//:text",
    "@hackage_hashable//:hashable",
  ],
  prebuilt_dependencies = [
    "bytestring",
    "base",
    "deepseq",
  ],
)
  """,
)

new_http_archive(
  name = "hackage_http_types",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/http-types-0.9.1/http-types-0.9.1.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/http-types-0.9.1.tar.gz",
  ],
  # sha256 = "3fea670082117e235f03f79c6dd093710ee3482a3be9c98c50385dd057a0f1a9",
  strip_prefix = "http-types-0.9.1",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "http-types",
  visibility = ["//visibility:public"],
  srcs = [
    "Network/HTTP/Types.hs",
    "Network/HTTP/Types/QueryLike.hs",
    "Network/HTTP/Types/Header.hs",
    "Network/HTTP/Types/Method.hs",
    "Network/HTTP/Types/URI.hs",
    "Network/HTTP/Types/Status.hs",
    "Network/HTTP/Types/Version.hs",
  ],
  deps = [
    "@hackage_case_insensitive//:case-insensitive",
    "@hackage_text//:text",
    "@hackage_blaze_builder//:blaze-builder",
  ],
  prebuilt_dependencies = [
    "bytestring",
    "base",
    "array",
  ],
)
  """,
)

new_http_archive(
  name = "hackage_stm",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/stm-2.4.5.0/stm-2.4.5.0.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/stm-2.4.5.0.tar.gz",
  ],
  # sha256 = "8a8a091e7113f9c07eb9ad4f58a67eb4a5ec58b2857c6ba80fa4d8d45015524c",
  strip_prefix = "stm-2.4.5.0",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "stm",
  visibility = ["//visibility:public"],
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
  deps = [
  ],
  prebuilt_dependencies = [
    "base",
    "array",
  ],
)
  """,
)

new_http_archive(
  name = "hackage_transformers_base",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/transformers-base-0.4.4/transformers-base-0.4.4.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/transformers-base-0.4.4.tar.gz",
  ],
  # sha256 = "fb1a305f29cbf6ac182af7e67efaae9fcb9664d8d9606bb8a7f3414ad4c8d7a4",
  strip_prefix = "transformers-base-0.4.4",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "transformers-base",
  visibility = ["//visibility:public"],
  srcs = [
    "src/Control/Monad/Base.hs",
  ],
  src_strip_prefix = "src",
  deps = [
    "@hackage_stm//:stm",
    "@hackage_transformers_compat//:transformers-compat",
  ],
  prebuilt_dependencies = [
    "base",
    "transformers",
  ],
)
  """,
)

new_http_archive(
  name = "hackage_mtl",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/mtl-2.2.1/mtl-2.2.1.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/mtl-2.2.1.tar.gz",
  ],
  # sha256 = "4b5a800fe9edf168fc7ae48c7a3fc2aab6b418ac15be2f1dad43c0f48a494a3b",
  strip_prefix = "mtl-2.2.1",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "mtl",
  visibility = ["//visibility:public"],
  srcs = [
    "Control/Monad/Cont.hs",
    "Control/Monad/Error.hs",
    "Control/Monad/Except.hs",
    "Control/Monad/Identity.hs",
    "Control/Monad/List.hs",
    "Control/Monad/Reader.hs",
    "Control/Monad/RWS.hs",
    "Control/Monad/State.hs",
    "Control/Monad/Trans.hs",
    "Control/Monad/Writer.hs",
    "Control/Monad/Cont/Class.hs",
    "Control/Monad/Error/Class.hs",
    "Control/Monad/Reader/Class.hs",
    "Control/Monad/RWS/Class.hs",
    "Control/Monad/RWS/Lazy.hs",
    "Control/Monad/RWS/Strict.hs",
    "Control/Monad/State/Class.hs",
    "Control/Monad/State/Lazy.hs",
    "Control/Monad/State/Strict.hs",
    "Control/Monad/Writer/Class.hs",
    "Control/Monad/Writer/Lazy.hs",
    "Control/Monad/Writer/Strict.hs",
  ],
  deps = [
  ],
  prebuilt_dependencies = [
    "base",
    "transformers",
  ],
)
  """,
)

new_http_archive(
  name = "hackage_mmorph",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/mmorph-1.1.0/mmorph-1.1.0.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/mmorph-1.1.0.tar.gz",
  ],
  # sha256 = "91a7b38d11f8031c0462a7e544c5bd282051c3a357e804948a353fba5834ee5e",
  strip_prefix = "mmorph-1.1.0",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "mmorph",
  visibility = ["//visibility:public"],
  srcs = [
    "src/Control/Monad/Morph.hs",
    "src/Control/Monad/Trans/Compose.hs",
  ],
  src_strip_prefix = "src",
  deps = [
    "@hackage_mtl//:mtl",
    "@hackage_transformers_compat//:transformers-compat",
  ],
  prebuilt_dependencies = [
    "base",
    "transformers",
  ],
)
  """,
)

new_http_archive(
  name = "hackage_unliftio_core",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/unliftio-core-0.1.1.0/unliftio-core-0.1.1.0.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/unliftio-core-0.1.1.0.tar.gz",
  ],
  # sha256 = "b6ff4083ed814ea85fce524628bdccaad8fcaf9ccf72b5f80850bf91af559453",
  strip_prefix = "unliftio-core-0.1.1.0",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "unliftio-core",
  visibility = ["//visibility:public"],
  srcs = [
    "src/Control/Monad/IO/Unlift.hs",
  ],
  src_strip_prefix = "src",
  deps = [
  ],
  prebuilt_dependencies = [
    "base",
    "transformers",
  ],
)
  """,
)

new_http_archive(
  name = "hackage_monad_control",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/monad-control-1.0.2.2/monad-control-1.0.2.2.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/monad-control-1.0.2.2.tar.gz",
  ],
  # sha256 = "34a2b22c281bec66f231f00fb11dd68a6845be086000ff74e23126f0a77cc1ea",
  strip_prefix = "monad-control-1.0.2.2",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "monad-control",
  visibility = ["//visibility:public"],
  srcs = [
    "Control/Monad/Trans/Control.hs",
  ],
  deps = [
    "@hackage_stm//:stm",
    "@hackage_transformers_base//:transformers-base",
    "@hackage_transformers_compat//:transformers-compat",
  ],
  prebuilt_dependencies = [
    "base",
    "transformers",
  ],
)
  """,
)

new_http_archive(
  name = "hackage_lifted_base",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/lifted-base-0.2.3.11/lifted-base-0.2.3.11.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/lifted-base-0.2.3.11.tar.gz",
  ],
  # sha256 = "dd6eb5346fa0d79394ccfa2d4b1f271f0d84cf92473997cab95eec1e2498316f",
  strip_prefix = "lifted-base-0.2.3.11",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "lifted-base",
  visibility = ["//visibility:public"],
  srcs = [
    "Data/IORef/Lifted.hs",
    "Foreign/Marshal/Utils/Lifted.hs",
    "System/Timeout/Lifted.hs",
    "Control/Concurrent/Lifted.hs",
    "Control/Concurrent/Chan/Lifted.hs",
    "Control/Concurrent/QSem/Lifted.hs",
    "Control/Concurrent/SampleVar/Lifted.hs",
    "Control/Concurrent/MVar/Lifted.hs",
    "Control/Concurrent/QSemN/Lifted.hs",
    "Control/Exception/Lifted.hs",
  ],
  deps = [
    "@hackage_monad_control//:monad-control",
    "@hackage_transformers_base//:transformers-base",
  ],
  prebuilt_dependencies = [
    "base",
  ],
)
  """,
)

new_http_archive(
  name = "hackage_exceptions",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/exceptions-0.8.3/exceptions-0.8.3.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/exceptions-0.8.3.tar.gz",
  ],
  # sha256 = "968ef0124d3a79b63bd4728232ad92d59fb0c9eb2f01679f0c375be7b8b931a2",
  strip_prefix = "exceptions-0.8.3",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "exceptions",
  visibility = ["//visibility:public"],
  srcs = [
    "src/Control/Monad/Catch.hs",
    "src/Control/Monad/Catch/Pure.hs",
  ],
  src_strip_prefix = "src",
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
)
  """,
)

new_http_archive(
  name = "hackage_resourcet",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/resourcet-1.1.11/resourcet-1.1.11.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/resourcet-1.1.11.tar.gz",
  ],
  # sha256 = "68177bf60e8db0c96049e0c47047da19cb196b6fee9a6eef4378f98220a5c3bd",
  strip_prefix = "resourcet-1.1.11",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "resourcet",
  visibility = ["//visibility:public"],
  srcs = [
    "Control/Monad/Trans/Resource.hs",
    "Control/Monad/Trans/Resource/Internal.hs",
    "Data/Acquire.hs",
    "Data/Acquire/Internal.hs",
    "UnliftIO/Resource.hs",
  ],
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
)
  """,
)

new_http_archive(
  name = "hackage_parsec",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/parsec-3.1.13.0/parsec-3.1.13.0.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/parsec-3.1.13.0.tar.gz",
  ],
  # sha256 = "8dbd67e16adff703c48d69414f88bd86f59dccb1fdd509358905bdddfe09d705",
  strip_prefix = "parsec-3.1.13.0",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "parsec",
  visibility = ["//visibility:public"],
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
  deps = [
    "@hackage_text//:text",
    "@hackage_mtl//:mtl",
  ],
  prebuilt_dependencies = [
    "bytestring",
    "base",
  ],
)
  """,
)

new_http_archive(
  name = "hackage_network_uri",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/network-uri-2.6.1.0/network-uri-2.6.1.0.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/network-uri-2.6.1.0.tar.gz",
  ],
  # sha256 = "62cc45c66023e37ef921d5fb546aca56a9c786615e05925fb193a70bf0913690",
  strip_prefix = "network-uri-2.6.1.0",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "network-uri",
  visibility = ["//visibility:public"],
  srcs = [
    "Network/URI.hs",
  ],
  deps = [
    "@hackage_parsec//:parsec",
  ],
  prebuilt_dependencies = [
    "base",
    "deepseq",
  ],
)
  """,
)

new_http_archive(
  name = "hackage_basement",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/basement-0.0.4/basement-0.0.4.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/basement-0.0.4.tar.gz",
  ],
  # sha256 = "9ec7a99ce237f064496b33db57cbf8c690ea2116a07f869486a644b5b1de5eaf",
  strip_prefix = "basement-0.0.4",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "basement",
  visibility = ["//visibility:public"],
  srcs = [
    "Basement/Imports.hs",
    "Basement/Base16.hs",
    "Basement/Bindings/Memory.hs",
    "Basement/Endianness.hs",
    "Basement/Environment.hs",
    "Basement/PrimType.hs",
    "Basement/Exception.hs",
    "Basement/From.hs",
    "Basement/Types/Char7.hs",
    "Basement/Types/OffsetSize.hs",
    "Basement/Types/Ptr.hs",
    "Basement/Types/AsciiString.hs",
    "Basement/Types/Word128.hs",
    "Basement/Types/Word256.hs",
    "Basement/Monad.hs",
    "Basement/MutableBuilder.hs",
    "Basement/FinalPtr.hs",
    "Basement/Nat.hs",
    "Basement/BoxedArray.hs",
    "Basement/Block.hs",
    "Basement/Block/Mutable.hs",
    "Basement/UArray.hs",
    "Basement/UArray/Mutable.hs",
    "Basement/String.hs",
    "Basement/NonEmpty.hs",
    "Basement/NormalForm.hs",
    "Basement/These.hs",
    "Basement/Terminal.hs",
    "Basement/Terminal/ANSI.hs",
    "Basement/IntegralConv.hs",
    "Basement/Floating.hs",
    "Basement/Numerical/Number.hs",
    "Basement/Numerical/Additive.hs",
    "Basement/Numerical/Subtractive.hs",
    "Basement/Numerical/Multiplicative.hs",
    "Basement/Bounded.hs",
    "Basement/Compat/Base.hs",
    "Basement/Compat/Bifunctor.hs",
    "Basement/Compat/CallStack.hs",
    "Basement/Compat/ExtList.hs",
    "Basement/Compat/IsList.hs",
    "Basement/Compat/Identity.hs",
    "Basement/Compat/Primitive.hs",
    "Basement/Compat/PrimTypes.hs",
    "Basement/Compat/MonadTrans.hs",
    "Basement/Compat/Semigroup.hs",
    "Basement/Compat/Natural.hs",
    "Basement/Compat/NumLiteral.hs",
    "Basement/Compat/Typeable.hs",
    "Basement/BlockN.hs",
    "Basement/Sized/Block.hs",
    "Basement/Sized/UVect.hs",
    "Basement/Sized/Vect.hs",
    "Basement/Sized/List.hs",
    "Basement/Error.hs",
    "Basement/Show.hs",
    "Basement/Runtime.hs",
    "Basement/Alg/Class.hs",
    "Basement/Alg/Mutable.hs",
    "Basement/Alg/PrimArray.hs",
    "Basement/Alg/Native/Prim.hs",
    "Basement/Alg/Native/UTF8.hs",
    "Basement/Alg/Native/String.hs",
    "Basement/Alg/Foreign/Prim.hs",
    "Basement/Alg/Foreign/UTF8.hs",
    "Basement/Alg/Foreign/String.hs",
    "Basement/Numerical/Conversion.hs",
    "Basement/Block/Base.hs",
    "Basement/UTF8/Base.hs",
    "Basement/UTF8/Helper.hs",
    "Basement/UTF8/Table.hs",
    "Basement/UTF8/Types.hs",
    "Basement/UArray/Base.hs",
    "Basement/String/Encoding/Encoding.hs",
    "Basement/String/Encoding/UTF16.hs",
    "Basement/String/Encoding/UTF32.hs",
    "Basement/String/Encoding/ASCII7.hs",
    "Basement/String/Encoding/ISO_8859_1.hs",
    "Basement/Terminal/Size.hsc",
  ],
  deps = [
  ],
  prebuilt_dependencies = [
    "base",
    "ghc-prim",
  ],
)
  """,
)

new_http_archive(
  name = "hackage_foundation",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/foundation-0.0.17/foundation-0.0.17.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/foundation-0.0.17.tar.gz",
  ],
  # sha256 = "ebad15af9c6829cf5aa682a22945d08da7709252780e309c7957c71e52055b15",
  strip_prefix = "foundation-0.0.17",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "foundation",
  visibility = ["//visibility:public"],
  srcs = [
    "Foundation/Numerical.hs",
    "Foundation/Array.hs",
    "Foundation/Array/Internal.hs",
    "Foundation/Bits.hs",
    "Foundation/Class/Bifunctor.hs",
    "Foundation/Class/Storable.hs",
    "Foundation/Conduit.hs",
    "Foundation/Conduit/Textual.hs",
    "Foundation/Exception.hs",
    "Foundation/String.hs",
    "Foundation/String/Read.hs",
    "Foundation/String/Builder.hs",
    "Foundation/IO.hs",
    "Foundation/IO/FileMap.hs",
    "Foundation/IO/Terminal.hs",
    "Foundation/VFS.hs",
    "Foundation/VFS/Path.hs",
    "Foundation/VFS/FilePath.hs",
    "Foundation/VFS/URI.hs",
    "Foundation/Math/Trigonometry.hs",
    "Foundation/Hashing.hs",
    "Foundation/Foreign.hs",
    "Foundation/Collection.hs",
    "Foundation/Primitive.hs",
    "Foundation/List/DList.hs",
    "Foundation/Monad.hs",
    "Foundation/Monad/Except.hs",
    "Foundation/Monad/Reader.hs",
    "Foundation/Monad/State.hs",
    "Foundation/Network/IPv4.hs",
    "Foundation/Network/IPv6.hs",
    "Foundation/System/Info.hs",
    "Foundation/Strict.hs",
    "Foundation/Parser.hs",
    "Foundation/Random.hs",
    "Foundation/Check.hs",
    "Foundation/Check/Main.hs",
    "Foundation/Timing.hs",
    "Foundation/Timing/Main.hs",
    "Foundation/Time/Types.hs",
    "Foundation/Time/Bindings.hs",
    "Foundation/Time/StopWatch.hs",
    "Foundation/UUID.hs",
    "Foundation/System/Entropy.hs",
    "Foundation/System/Bindings.hs",
    "Foundation/Network/HostName.hsc",
    "Foundation/System/Bindings/Posix.hsc",
    "Foundation/System/Bindings/PosixDef.hsc",
    "Foundation/System/Bindings/Linux.hsc",
    "Foundation/System/Bindings/Macos.hsc",
    "Foundation/Tuple/Nth.hs",
    "Foundation/List/ListN.hs",
    "Foundation/Tuple.hs",
    "Foundation/Hashing/FNV.hs",
    "Foundation/Hashing/SipHash.hs",
    "Foundation/Hashing/Hasher.hs",
    "Foundation/Hashing/Hashable.hs",
    "Foundation/Check/Gen.hs",
    "Foundation/Check/Print.hs",
    "Foundation/Check/Arbitrary.hs",
    "Foundation/Check/Property.hs",
    "Foundation/Check/Config.hs",
    "Foundation/Check/Types.hs",
    "Foundation/Collection/Buildable.hs",
    "Foundation/Collection/List.hs",
    "Foundation/Collection/Element.hs",
    "Foundation/Collection/InnerFunctor.hs",
    "Foundation/Collection/Collection.hs",
    "Foundation/Collection/Copy.hs",
    "Foundation/Collection/Sequential.hs",
    "Foundation/Collection/Keyed.hs",
    "Foundation/Collection/Indexed.hs",
    "Foundation/Collection/Foldable.hs",
    "Foundation/Collection/Mutable.hs",
    "Foundation/Collection/Zippable.hs",
    "Foundation/Collection/Mappable.hs",
    "Foundation/Conduit/Internal.hs",
    "Foundation/Numerical/Floating.hs",
    "Foundation/IO/File.hs",
    "Foundation/Monad/MonadIO.hs",
    "Foundation/Monad/Exception.hs",
    "Foundation/Monad/Transformer.hs",
    "Foundation/Monad/Identity.hs",
    "Foundation/Monad/Base.hs",
    "Foundation/Random/Class.hs",
    "Foundation/Random/DRG.hs",
    "Foundation/Random/ChaChaDRG.hs",
    "Foundation/Random/XorShift.hs",
    "Foundation/Array/Chunked/Unboxed.hs",
    "Foundation/Array/Bitmap.hs",
    "Foundation/Foreign/Alloc.hs",
    "Foundation/Foreign/MemoryMap.hs",
    "Foundation/Foreign/MemoryMap/Types.hs",
    "Foundation/Partial.hs",
    "Foundation/System/Entropy/Common.hs",
    "Foundation/System/Bindings/Network.hsc",
    "Foundation/System/Bindings/Time.hsc",
    "Foundation/System/Bindings/Hs.hs",
    "Foundation/Foreign/MemoryMap/Posix.hsc",
    "Foundation/System/Entropy/Unix.hs",
  ],
  deps = [
    "@hackage_basement//:basement",
  ],
  prebuilt_dependencies = [
    "base",
    "ghc-prim",
  ],
)
  """,
)

new_http_archive(
  name = "hackage_memory",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/memory-0.14.11/memory-0.14.11.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/memory-0.14.11.tar.gz",
  ],
  # sha256 = "46ccc4016c39379955137e13c1a386e2a232924d063c650e2e0c219c3a8c3b78",
  strip_prefix = "memory-0.14.11",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "memory",
  visibility = ["//visibility:public"],
  srcs = [
    "Data/ByteArray.hs",
    "Data/ByteArray/Encoding.hs",
    "Data/ByteArray/Mapping.hs",
    "Data/ByteArray/Pack.hs",
    "Data/ByteArray/Parse.hs",
    "Data/ByteArray/Hash.hs",
    "Data/Memory/Endian.hs",
    "Data/Memory/PtrMethods.hs",
    "Data/Memory/ExtendedWords.hs",
    "Data/Memory/Encoding/Base16.hs",
    "Data/Memory/Encoding/Base32.hs",
    "Data/Memory/Encoding/Base64.hs",
    "Data/Memory/Internal/Compat.hs",
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
    "Data/Memory/MemMap/Posix.hsc",
  ],
  deps = [
    "@hackage_basement//:basement",
    "@hackage_foundation//:foundation",
  ],
  prebuilt_dependencies = [
    "bytestring",
    "base",
    "ghc-prim",
    "deepseq",
  ],
)
  """,
)

new_http_archive(
  name = "hackage_cryptonite",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/cryptonite-0.24/cryptonite-0.24.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/cryptonite-0.24.tar.gz",
  ],
  # sha256 = "4f0865ceb2b6e99f37cb2243831a63ac76b53960c20c75f890f729635551c7d7",
  strip_prefix = "cryptonite-0.24",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "cryptonite",
  visibility = ["//visibility:public"],
  srcs = [
    "Crypto/Cipher/AES.hs",
    "Crypto/Cipher/Blowfish.hs",
    "Crypto/Cipher/Camellia.hs",
    "Crypto/Cipher/ChaCha.hs",
    "Crypto/Cipher/ChaChaPoly1305.hs",
    "Crypto/Cipher/DES.hs",
    "Crypto/Cipher/RC4.hs",
    "Crypto/Cipher/Salsa.hs",
    "Crypto/Cipher/TripleDES.hs",
    "Crypto/Cipher/Twofish.hs",
    "Crypto/Cipher/Types.hs",
    "Crypto/Cipher/Utils.hs",
    "Crypto/Cipher/XSalsa.hs",
    "Crypto/ConstructHash/MiyaguchiPreneel.hs",
    "Crypto/Data/AFIS.hs",
    "Crypto/Data/Padding.hs",
    "Crypto/ECC.hs",
    "Crypto/Error.hs",
    "Crypto/MAC/CMAC.hs",
    "Crypto/MAC/Poly1305.hs",
    "Crypto/MAC/HMAC.hs",
    "Crypto/Number/Basic.hs",
    "Crypto/Number/F2m.hs",
    "Crypto/Number/Generate.hs",
    "Crypto/Number/ModArithmetic.hs",
    "Crypto/Number/Prime.hs",
    "Crypto/Number/Serialize.hs",
    "Crypto/Number/Serialize/Internal.hs",
    "Crypto/KDF/Argon2.hs",
    "Crypto/KDF/PBKDF2.hs",
    "Crypto/KDF/Scrypt.hs",
    "Crypto/KDF/BCrypt.hs",
    "Crypto/KDF/HKDF.hs",
    "Crypto/Hash.hs",
    "Crypto/Hash/IO.hs",
    "Crypto/Hash/Algorithms.hs",
    "Crypto/OTP.hs",
    "Crypto/PubKey/Curve25519.hs",
    "Crypto/PubKey/Curve448.hs",
    "Crypto/PubKey/MaskGenFunction.hs",
    "Crypto/PubKey/DH.hs",
    "Crypto/PubKey/DSA.hs",
    "Crypto/PubKey/ECC/Generate.hs",
    "Crypto/PubKey/ECC/Prim.hs",
    "Crypto/PubKey/ECC/DH.hs",
    "Crypto/PubKey/ECC/ECDSA.hs",
    "Crypto/PubKey/ECC/P256.hs",
    "Crypto/PubKey/ECC/Types.hs",
    "Crypto/PubKey/ECIES.hs",
    "Crypto/PubKey/Ed25519.hs",
    "Crypto/PubKey/Ed448.hs",
    "Crypto/PubKey/RSA.hs",
    "Crypto/PubKey/RSA/PKCS15.hs",
    "Crypto/PubKey/RSA/Prim.hs",
    "Crypto/PubKey/RSA/PSS.hs",
    "Crypto/PubKey/RSA/OAEP.hs",
    "Crypto/PubKey/RSA/Types.hs",
    "Crypto/Random.hs",
    "Crypto/Random/Types.hs",
    "Crypto/Random/Entropy.hs",
    "Crypto/Random/EntropyPool.hs",
    "Crypto/Random/Entropy/Unsafe.hs",
    "Crypto/Tutorial.hs",
    "Crypto/Cipher/AES/Primitive.hs",
    "Crypto/Cipher/Blowfish/Box.hs",
    "Crypto/Cipher/Blowfish/Primitive.hs",
    "Crypto/Cipher/Camellia/Primitive.hs",
    "Crypto/Cipher/DES/Primitive.hs",
    "Crypto/Cipher/Twofish/Primitive.hs",
    "Crypto/Cipher/Types/AEAD.hs",
    "Crypto/Cipher/Types/Base.hs",
    "Crypto/Cipher/Types/Block.hs",
    "Crypto/Cipher/Types/GF.hs",
    "Crypto/Cipher/Types/Stream.hs",
    "Crypto/Cipher/Types/Utils.hs",
    "Crypto/Error/Types.hs",
    "Crypto/Number/Compat.hs",
    "Crypto/Hash/Types.hs",
    "Crypto/Hash/Blake2s.hs",
    "Crypto/Hash/Blake2sp.hs",
    "Crypto/Hash/Blake2b.hs",
    "Crypto/Hash/Blake2bp.hs",
    "Crypto/Hash/SHA1.hs",
    "Crypto/Hash/SHA224.hs",
    "Crypto/Hash/SHA256.hs",
    "Crypto/Hash/SHA384.hs",
    "Crypto/Hash/SHA512.hs",
    "Crypto/Hash/SHA512t.hs",
    "Crypto/Hash/SHA3.hs",
    "Crypto/Hash/Keccak.hs",
    "Crypto/Hash/MD2.hs",
    "Crypto/Hash/MD4.hs",
    "Crypto/Hash/MD5.hs",
    "Crypto/Hash/RIPEMD160.hs",
    "Crypto/Hash/Skein256.hs",
    "Crypto/Hash/Skein512.hs",
    "Crypto/Hash/Tiger.hs",
    "Crypto/Hash/Whirlpool.hs",
    "Crypto/Random/Entropy/Source.hs",
    "Crypto/Random/Entropy/Backend.hs",
    "Crypto/Random/ChaChaDRG.hs",
    "Crypto/Random/SystemDRG.hs",
    "Crypto/Random/Probabilistic.hs",
    "Crypto/PubKey/Internal.hs",
    "Crypto/PubKey/ElGamal.hs",
    "Crypto/ECC/Simple/Types.hs",
    "Crypto/ECC/Simple/Prim.hs",
    "Crypto/Internal/Proxy.hs",
    "Crypto/Internal/ByteArray.hs",
    "Crypto/Internal/Compat.hs",
    "Crypto/Internal/CompatPrim.hs",
    "Crypto/Internal/DeepSeq.hs",
    "Crypto/Internal/Imports.hs",
    "Crypto/Internal/Words.hs",
    "Crypto/Internal/WordArray.hs",
    "Crypto/Hash/SHAKE.hs",
    "Crypto/Hash/Blake2.hs",
    "Crypto/Internal/Nat.hs",
    "Crypto/Random/Entropy/RDRand.hs",
    "Crypto/Random/Entropy/Unix.hs",
  ],
  deps = [
    "@hackage_foundation//:foundation",
    "@hackage_memory//:memory",
  ],
  prebuilt_dependencies = [
    "bytestring",
    "base",
    "integer-gmp",
    "ghc-prim",
    "deepseq",
  ],
)
  """,
)

new_http_archive(
  name = "hackage_byteable",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/byteable-0.1.1/byteable-0.1.1.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/byteable-0.1.1.tar.gz",
  ],
  # sha256 = "12eeda93251d4b5d510ac95cf578f5c89d4a399b14ca73116deaf4921a516fdf",
  strip_prefix = "byteable-0.1.1",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "byteable",
  visibility = ["//visibility:public"],
  srcs = [
    "Data/Byteable.hs",
  ],
  deps = [
  ],
  prebuilt_dependencies = [
    "bytestring",
    "base",
  ],
)
  """,
)

new_http_archive(
  name = "hackage_hourglass",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/hourglass-0.2.11/hourglass-0.2.11.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/hourglass-0.2.11.tar.gz",
  ],
  # sha256 = "7b20d78cbe0f08f994d4b4d0fdde184e1ae4d111bf7f13bdb25ccee81e7e628f",
  strip_prefix = "hourglass-0.2.11",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "hourglass",
  visibility = ["//visibility:public"],
  srcs = [
    "Time/Types.hs",
    "Time/System.hs",
    "Time/Compat.hs",
    "Data/Hourglass.hs",
    "Data/Hourglass/Types.hs",
    "Data/Hourglass/Epoch.hs",
    "Data/Hourglass/Compat.hs",
    "System/Hourglass.hs",
    "Data/Hourglass/Time.hs",
    "Data/Hourglass/Format.hs",
    "Data/Hourglass/Diff.hs",
    "Data/Hourglass/Local.hs",
    "Data/Hourglass/Calendar.hs",
    "Data/Hourglass/Zone.hs",
    "Data/Hourglass/Internal.hs",
    "Data/Hourglass/Utils.hs",
    "Data/Hourglass/Internal/Win.hs",
    "Data/Hourglass/Internal/Unix.hs",
  ],
  deps = [
  ],
  prebuilt_dependencies = [
    "base",
    "deepseq",
  ],
)
  """,
)

new_http_archive(
  name = "hackage_asn1_types",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/asn1-types-0.3.2/asn1-types-0.3.2.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/asn1-types-0.3.2.tar.gz",
  ],
  # sha256 = "2fc0d64079e9ac9ec7f349aa65c1775acb060f19601deee129a9769fdcf8f07a",
  strip_prefix = "asn1-types-0.3.2",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "asn1-types",
  visibility = ["//visibility:public"],
  srcs = [
    "Data/ASN1/BitArray.hs",
    "Data/ASN1/OID.hs",
    "Data/ASN1/Pretty.hs",
    "Data/ASN1/Types.hs",
    "Data/ASN1/Types/Lowlevel.hs",
    "Data/ASN1/Types/String.hs",
  ],
  deps = [
    "@hackage_hourglass//:hourglass",
    "@hackage_memory//:memory",
  ],
  prebuilt_dependencies = [
    "bytestring",
    "base",
  ],
)
  """,
)

new_http_archive(
  name = "hackage_asn1_encoding",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/asn1-encoding-0.9.5/asn1-encoding-0.9.5.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/asn1-encoding-0.9.5.tar.gz",
  ],
  # sha256 = "7d4acf178ef17aad650f2073dd2ed94febb1e468fddb0d5ca65c1a54caffcfb8",
  strip_prefix = "asn1-encoding-0.9.5",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "asn1-encoding",
  visibility = ["//visibility:public"],
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
  deps = [
    "@hackage_hourglass//:hourglass",
    "@hackage_asn1_types//:asn1-types",
  ],
  prebuilt_dependencies = [
    "bytestring",
    "base",
  ],
)
  """,
)

new_http_archive(
  name = "hackage_asn1_parse",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/asn1-parse-0.9.4/asn1-parse-0.9.4.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/asn1-parse-0.9.4.tar.gz",
  ],
  # sha256 = "748249e23024dde8fed1d99e85e7e952576ce51b3ce460b9d131b2e91ff9c5a4",
  strip_prefix = "asn1-parse-0.9.4",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "asn1-parse",
  visibility = ["//visibility:public"],
  srcs = [
    "Data/ASN1/Parse.hs",
  ],
  deps = [
    "@hackage_asn1_types//:asn1-types",
    "@hackage_asn1_encoding//:asn1-encoding",
  ],
  prebuilt_dependencies = [
    "bytestring",
    "base",
  ],
)
  """,
)

new_http_archive(
  name = "hackage_base64_bytestring",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/base64-bytestring-1.0.0.1/base64-bytestring-1.0.0.1.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/base64-bytestring-1.0.0.1.tar.gz",
  ],
  # sha256 = "40e088e357790fe3709ff7b9dd18d79b866e2936b13ade184bc9355315fb3ab5",
  strip_prefix = "base64-bytestring-1.0.0.1",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "base64-bytestring",
  visibility = ["//visibility:public"],
  srcs = [
    "Data/ByteString/Base64.hs",
    "Data/ByteString/Base64/Internal.hs",
    "Data/ByteString/Base64/Lazy.hs",
    "Data/ByteString/Base64/URL.hs",
    "Data/ByteString/Base64/URL/Lazy.hs",
  ],
  deps = [
  ],
  prebuilt_dependencies = [
    "bytestring",
    "base",
  ],
)
  """,
)

new_http_archive(
  name = "hackage_pem",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/pem-0.2.2/pem-0.2.2.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/pem-0.2.2.tar.gz",
  ],
  # sha256 = "65aa6d6b87a33e9562602ed4d103a734834a0b7325882cd4277e5280d26a3089",
  strip_prefix = "pem-0.2.2",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "pem",
  visibility = ["//visibility:public"],
  srcs = [
    "Data/PEM.hs",
    "Data/PEM/Parser.hs",
    "Data/PEM/Types.hs",
    "Data/PEM/Writer.hs",
  ],
  deps = [
    "@hackage_base64_bytestring//:base64-bytestring",
    "@hackage_mtl//:mtl",
  ],
  prebuilt_dependencies = [
    "bytestring",
    "base",
  ],
)
  """,
)

new_http_archive(
  name = "hackage_x509",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/x509-1.7.2/x509-1.7.2.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/x509-1.7.2.tar.gz",
  ],
  # sha256 = "c6b0cae6628398bb1e6ba71457fd9b64046ef051a0f472e1af657e22eb85b71e",
  strip_prefix = "x509-1.7.2",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "x509",
  visibility = ["//visibility:public"],
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
)
  """,
)

new_http_archive(
  name = "hackage_x509_store",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/x509-store-1.6.5/x509-store-1.6.5.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/x509-store-1.6.5.tar.gz",
  ],
  # sha256 = "4b5dbe9205e9ca16ba3eb5c64a0162babb21b67386c7eab5c2df9bb7337f5a36",
  strip_prefix = "x509-store-1.6.5",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "x509-store",
  visibility = ["//visibility:public"],
  srcs = [
    "Data/X509/CertificateStore.hs",
    "Data/X509/File.hs",
    "Data/X509/Memory.hs",
  ],
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
)
  """,
)

new_http_archive(
  name = "hackage_x509_system",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/x509-system-1.6.6/x509-system-1.6.6.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/x509-system-1.6.6.tar.gz",
  ],
  # sha256 = "3a1b9cc26715d7cb3cd1a3f8b6153f12c2d42187ac5df305c3973c78a061db05",
  strip_prefix = "x509-system-1.6.6",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "x509-system",
  visibility = ["//visibility:public"],
  srcs = [
    "System/X509.hs",
    "System/X509/Unix.hs",
    "System/X509/MacOS.hs",
    "System/X509/Win32.hs",
  ],
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
)
  """,
)

new_http_archive(
  name = "hackage_network",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/network-2.6.3.3/network-2.6.3.3.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/network-2.6.3.3.tar.gz",
  ],
  # sha256 = "ffae5599d1224fea4b434f2382f9f0dcb74ad8505d3bb7bca84d0ee9d7d2095a",
  strip_prefix = "network-2.6.3.3",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "network",
  visibility = ["//visibility:public"],
  srcs = [
    #"Network/BSD.hsc",
    "Network/Socket.hsc",
    "Network/Socket/ByteString.hsc",
    "Network/Socket/ByteString/Lazy.hs",
    "Network/Socket/Internal.hsc",
    "Network/Socket/ByteString/Internal.hs",
    "Network/Socket/Types.hsc",
    "Network/Socket/ByteString/IOVec.hsc",
    "Network/Socket/ByteString/Lazy/Posix.hs",
    "Network/Socket/ByteString/MsgHdr.hsc",
    "include/HsNet.h",
    "include/HsNetworkConfig.h",
  ],
  hdrs = [
    "include/HsNet.h",
    "include/HsNetworkConfig.h",
  ],
  deps = [
  ],
  prebuilt_dependencies = [
    "bytestring",
    "unix",
    "base",
  ],
)
  """,
)

new_http_archive(
  name = "hackage_async",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/async-2.1.1.1/async-2.1.1.1.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/async-2.1.1.1.tar.gz",
  ],
  # sha256 = "2ce05388bbd8d78e17f130acfe60b2bd2b5a09b995c9f4eb7c2d1ae41a8b1652",
  strip_prefix = "async-2.1.1.1",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "async",
  visibility = ["//visibility:public"],
  srcs = [
    "Control/Concurrent/Async.hs",
  ],
  deps = [
    "@hackage_stm//:stm",
  ],
  prebuilt_dependencies = [
    "base",
  ],
)
  """,
)

new_http_archive(
  name = "hackage_data_default_class",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/data-default-class-0.1.2.0/data-default-class-0.1.2.0.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/data-default-class-0.1.2.0.tar.gz",
  ],
  # sha256 = "63e62120b7efd733a5a17cf59ceb43268e9a929c748127172d7d42f4a336e327",
  strip_prefix = "data-default-class-0.1.2.0",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "data-default-class",
  visibility = ["//visibility:public"],
  srcs = [
    "Data/Default/Class.hs",
  ],
  deps = [
  ],
  prebuilt_dependencies = [
    "base",
  ],
)
  """,
)

new_http_archive(
  name = "hackage_x509_validation",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/x509-validation-1.6.9/x509-validation-1.6.9.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/x509-validation-1.6.9.tar.gz",
  ],
  # sha256 = "5d49e8f75a5b3b6a19c064ccf647cfcb4a89aff28ce868f48e9cc41f39d0c90a",
  strip_prefix = "x509-validation-1.6.9",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "x509-validation",
  visibility = ["//visibility:public"],
  srcs = [
    "Data/X509/Validation.hs",
    "Data/X509/Validation/Signature.hs",
    "Data/X509/Validation/Fingerprint.hs",
    "Data/X509/Validation/Cache.hs",
    "Data/X509/Validation/Types.hs",
  ],
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
)
  """,
)

new_http_archive(
  name = "hackage_cereal",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/cereal-0.5.5.0/cereal-0.5.5.0.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/cereal-0.5.5.0.tar.gz",
  ],
  # sha256 = "2222b2c7f0bd281527eb404a656116de8e6199d5d0c15c7cead9c1cea4d84414",
  strip_prefix = "cereal-0.5.5.0",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "cereal",
  visibility = ["//visibility:public"],
  srcs = [
    "src/Data/Serialize.hs",
    "src/Data/Serialize/IEEE754.hs",
    "src/Data/Serialize/Get.hs",
    "src/Data/Serialize/Put.hs",
  ],
  src_strip_prefix = "src",
  deps = [
  ],
  prebuilt_dependencies = [
    "bytestring",
    "base",
    "array",
    "containers",
    "ghc-prim",
  ],
)
  """,
)

new_http_archive(
  name = "hackage_tls",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/tls-1.4.0/tls-1.4.0.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/tls-1.4.0.tar.gz",
  ],
  # sha256 = "c17d9ae9a5e82ecfdc3a16ea7271277624ba719446e81c76d58d42ca6ddcd9cb",
  strip_prefix = "tls-1.4.0",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "tls",
  visibility = ["//visibility:public"],
  srcs = [
    "Network/TLS.hs",
    "Network/TLS/Cipher.hs",
    "Network/TLS/Compression.hs",
    "Network/TLS/Internal.hs",
    "Network/TLS/Extra.hs",
    "Network/TLS/Extra/Cipher.hs",
    "Network/TLS/Extra/FFDHE.hs",
    "Network/TLS/Cap.hs",
    "Network/TLS/Struct.hs",
    "Network/TLS/Core.hs",
    "Network/TLS/Context.hs",
    "Network/TLS/Context/Internal.hs",
    "Network/TLS/Credentials.hs",
    "Network/TLS/Backend.hs",
    "Network/TLS/Crypto.hs",
    "Network/TLS/Crypto/DH.hs",
    "Network/TLS/Crypto/IES.hs",
    "Network/TLS/Crypto/Types.hs",
    "Network/TLS/ErrT.hs",
    "Network/TLS/Extension.hs",
    "Network/TLS/Handshake.hs",
    "Network/TLS/Handshake/Common.hs",
    "Network/TLS/Handshake/Certificate.hs",
    "Network/TLS/Handshake/Key.hs",
    "Network/TLS/Handshake/Client.hs",
    "Network/TLS/Handshake/Server.hs",
    "Network/TLS/Handshake/Process.hs",
    "Network/TLS/Handshake/Signature.hs",
    "Network/TLS/Handshake/State.hs",
    "Network/TLS/Hooks.hs",
    "Network/TLS/IO.hs",
    "Network/TLS/Imports.hs",
    "Network/TLS/MAC.hs",
    "Network/TLS/Measurement.hs",
    "Network/TLS/Packet.hs",
    "Network/TLS/Parameters.hs",
    "Network/TLS/Record.hs",
    "Network/TLS/Record/Types.hs",
    "Network/TLS/Record/Engage.hs",
    "Network/TLS/Record/Disengage.hs",
    "Network/TLS/Record/State.hs",
    "Network/TLS/RNG.hs",
    "Network/TLS/State.hs",
    "Network/TLS/Session.hs",
    "Network/TLS/Sending.hs",
    "Network/TLS/Receiving.hs",
    "Network/TLS/Util.hs",
    "Network/TLS/Util/ASN1.hs",
    "Network/TLS/Util/Serialization.hs",
    "Network/TLS/Types.hs",
    "Network/TLS/Wire.hs",
    "Network/TLS/X509.hs",
  ],
  deps = [
    "@hackage_cereal//:cereal",
    "@hackage_x509_validation//:x509-validation",
    "@hackage_data_default_class//:data-default-class",
    "@hackage_asn1_types//:asn1-types",
    "@hackage_network//:network",
    "@hackage_async//:async",
    "@hackage_x509_store//:x509-store",
    "@hackage_memory//:memory",
    "@hackage_x509//:x509",
    "@hackage_cryptonite//:cryptonite",
    "@hackage_mtl//:mtl",
    "@hackage_asn1_encoding//:asn1-encoding",
  ],
  prebuilt_dependencies = [
    "bytestring",
    "base",
    "transformers",
  ],
)
  """,
)

new_http_archive(
  name = "hackage_socks",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/socks-0.5.6/socks-0.5.6.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/socks-0.5.6.tar.gz",
  ],
  # sha256 = "074bad28bedd0b3924b6fb2cea04fd2ed330d1dbd0dea3c87dbb4173c9dcffa1",
  strip_prefix = "socks-0.5.6",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "socks",
  visibility = ["//visibility:public"],
  srcs = [
    "Network/Socks5.hs",
    "Network/Socks5/Lowlevel.hs",
    "Network/Socks5/Types.hs",
    "Network/Socks5/Wire.hs",
    "Network/Socks5/Conf.hs",
    "Network/Socks5/Command.hs",
    "Network/Socks5/Parse.hs",
  ],
  deps = [
    "@hackage_cereal//:cereal",
    "@hackage_network//:network",
  ],
  prebuilt_dependencies = [
    "bytestring",
    "base",
  ],
)
  """,
)

new_http_archive(
  name = "hackage_connection",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/connection-0.2.8/connection-0.2.8.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/connection-0.2.8.tar.gz",
  ],
  # sha256 = "80671b805383147d41bc099a2b591442da5659d075b17f528c4729a39c99d6d8",
  strip_prefix = "connection-0.2.8",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "connection",
  visibility = ["//visibility:public"],
  srcs = [
    "Network/Connection.hs",
    "Network/Connection/Types.hs",
  ],
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
)
  """,
)

new_http_archive(
  name = "hackage_mime_types",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/mime-types-0.1.0.7/mime-types-0.1.0.7.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/mime-types-0.1.0.7.tar.gz",
  ],
  # sha256 = "a8129da584bf7da6396b66f81263291778d6ce03a82c10b5edb8f0bc8a902328",
  strip_prefix = "mime-types-0.1.0.7",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "mime-types",
  visibility = ["//visibility:public"],
  srcs = [
    "Network/Mime.hs",
  ],
  deps = [
    "@hackage_text//:text",
  ],
  prebuilt_dependencies = [
    "bytestring",
    "base",
    "containers",
  ],
)
  """,
)

new_http_archive(
  name = "hackage_zlib",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/zlib-0.6.1.2/zlib-0.6.1.2.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/zlib-0.6.1.2.tar.gz",
  ],
  # sha256 = "a0ac2b650ec3c38ff041e4da5a3b6969c1ca68a730fcd95d9ed78294305c6768",
  strip_prefix = "zlib-0.6.1.2",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "zlib",
  visibility = ["//visibility:public"],
  srcs = [
    "Codec/Compression/GZip.hs",
    "Codec/Compression/Zlib.hs",
    "Codec/Compression/Zlib/Raw.hs",
    "Codec/Compression/Zlib/Internal.hs",
    "Codec/Compression/Zlib/Stream.hsc",
  ],
  deps = [
  ],
  prebuilt_dependencies = [
    "bytestring",
    "base",
  ],
)
  """,
)

new_http_archive(
  name = "hackage_streaming_commons",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/streaming-commons-0.1.19/streaming-commons-0.1.19.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/streaming-commons-0.1.19.tar.gz",
  ],
  # sha256 = "3a02f84578f75eac1425dca877f8d697b68d379a21970c1dad96196620404803",
  strip_prefix = "streaming-commons-0.1.19",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "streaming-commons",
  visibility = ["//visibility:public"],
  srcs = [
    "Data/Streaming/Blaze.hs",
    "Data/Streaming/ByteString/Builder.hs",
    "Data/Streaming/ByteString/Builder/Buffer.hs",
    "Data/Streaming/ByteString/Builder/Class.hs",
    "Data/Streaming/FileRead.hs",
    "Data/Streaming/Filesystem.hs",
    "Data/Streaming/Network.hs",
    "Data/Streaming/Network/Internal.hs",
    "Data/Streaming/Process.hs",
    "Data/Streaming/Process/Internal.hs",
    "Data/Streaming/Text.hs",
    "Data/Streaming/Zlib.hs",
    "Data/Streaming/Zlib/Lowlevel.hs",
    "Data/Text/Internal/Unsafe/Char.hs",
    "Data/Text/Internal/Unsafe/Shift.hs",
    "Data/Text/Internal/Encoding/Utf8.hs",
    "Data/Text/Internal/Encoding/Utf16.hs",
    "Data/Text/Internal/Encoding/Utf32.hs",
  ],
  deps = [
    "@hackage_stm//:stm",
    "@hackage_text//:text",
    "@hackage_network//:network",
    "@hackage_async//:async",
    "@hackage_blaze_builder//:blaze-builder",
    "@hackage_zlib//:zlib",
    "@hackage_random//:random",
  ],
  prebuilt_dependencies = [
    "bytestring",
    "unix",
    "base",
    "process",
    "array",
    "transformers",
    "directory",
  ],
)
  """,
)

new_http_archive(
  name = "hackage_old_locale",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/old-locale-1.0.0.7/old-locale-1.0.0.7.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/old-locale-1.0.0.7.tar.gz",
  ],
  # sha256 = "fa998be2c7e00cd26a6e9075bea790caaf3932caa3e9497ad69bc20380dd6911",
  strip_prefix = "old-locale-1.0.0.7",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "old-locale",
  visibility = ["//visibility:public"],
  srcs = [
    "System/Locale.hs",
  ],
  deps = [
  ],
  prebuilt_dependencies = [
    "base",
  ],
)
  """,
)

new_http_archive(
  name = "hackage_cookie",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/cookie-0.4.3/cookie-0.4.3.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/cookie-0.4.3.tar.gz",
  ],
  # sha256 = "ab1e74e8fc7d96f8d67ed9f490e330cafe4fa42b3f7821213934023b075008a9",
  strip_prefix = "cookie-0.4.3",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "cookie",
  visibility = ["//visibility:public"],
  srcs = [
    "Web/Cookie.hs",
  ],
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
)
  """,
)

new_http_archive(
  name = "hackage_http_client",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/http-client-0.5.10/http-client-0.5.10.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/http-client-0.5.10.tar.gz",
  ],
  # sha256 = "a2e8f6f5617e7eed02ad3ff4e89ef2ebed525bc969114be14b6182a493cafe77",
  strip_prefix = "http-client-0.5.10",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "http-client",
  visibility = ["//visibility:public"],
  srcs = [
    "Network/HTTP/Client.hs",
    "Network/HTTP/Client/MultipartFormData.hs",
    "Network/HTTP/Client/Internal.hs",
    "Network/HTTP/Client/Body.hs",
    "Network/HTTP/Client/Connection.hs",
    "Network/HTTP/Client/Cookies.hs",
    "Network/HTTP/Client/Core.hs",
    "Network/HTTP/Client/Headers.hs",
    "Network/HTTP/Client/Manager.hs",
    "Network/HTTP/Client/Request.hs",
    "Network/HTTP/Client/Response.hs",
    "Network/HTTP/Client/Types.hs",
    "Network/HTTP/Client/Util.hs",
    "Network/HTTP/Proxy.hs",
  ],
  deps = [
    "@hackage_cookie//:cookie",
    "@hackage_exceptions//:exceptions",
    "@hackage_streaming_commons//:streaming-commons",
    "@hackage_case_insensitive//:case-insensitive",
    "@hackage_stm//:stm",
    "@hackage_base64_bytestring//:base64-bytestring",
    "@hackage_text//:text",
    "@hackage_network//:network",
    "@hackage_blaze_builder//:blaze-builder",
    "@hackage_mime_types//:mime-types",
    "@hackage_network_uri//:network-uri",
    "@hackage_random//:random",
    "@hackage_http_types//:http-types",
  ],
  prebuilt_dependencies = [
    "bytestring",
    "base",
    "time",
    "filepath",
    "array",
    "containers",
    "ghc-prim",
    "transformers",
    "deepseq",
  ],
)
  """,
)

new_http_archive(
  name = "hackage_http_client_tls",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/http-client-tls-0.3.5.3/http-client-tls-0.3.5.3.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/http-client-tls-0.3.5.3.tar.gz",
  ],
  # sha256 = "25d3b7c42198d0e1d193c0bb323b9a0071d56aacb41960ff448eb663055e15bf",
  strip_prefix = "http-client-tls-0.3.5.3",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "http-client-tls",
  visibility = ["//visibility:public"],
  srcs = [
    "Network/HTTP/Client/TLS.hs",
  ],
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
)
  """,
)

new_http_archive(
  name = "hackage_typed_process",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/typed-process-0.2.1.0/typed-process-0.2.1.0.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/typed-process-0.2.1.0.tar.gz",
  ],
  # sha256 = "5731c4b4417797ce3a0d27a86785c29b2cc80dcccb3f2875e191a3299d3a0d5d",
  strip_prefix = "typed-process-0.2.1.0",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "typed-process",
  visibility = ["//visibility:public"],
  srcs = [
    "src/System/Process/Typed.hs",
  ],
  src_strip_prefix = "src",
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
)
  """,
)

new_http_archive(
  name = "hackage_conduit",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/conduit-1.2.13/conduit-1.2.13.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/conduit-1.2.13.tar.gz",
  ],
  # sha256 = "05a1d74350f7f69f3774bf4ecb64e0a494b196113760a323c49f9cb7d2141f9e",
  strip_prefix = "conduit-1.2.13",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "conduit",
  visibility = ["//visibility:public"],
  srcs = [
    "Data/Conduit.hs",
    "Data/Conduit/List.hs",
    "Data/Conduit/Internal.hs",
    "Data/Conduit/Lift.hs",
    "Data/Conduit/Internal/Fusion.hs",
    "Data/Conduit/Internal/List/Stream.hs",
    "Data/Conduit/Internal/Pipe.hs",
    "Data/Conduit/Internal/Conduit.hs",
  ],
  deps = [
    "@hackage_exceptions//:exceptions",
    "@hackage_monad_control//:monad-control",
    "@hackage_lifted_base//:lifted-base",
    "@hackage_mtl//:mtl",
    "@hackage_mmorph//:mmorph",
    "@hackage_transformers_base//:transformers-base",
    "@hackage_resourcet//:resourcet",
    "@hackage_primitive//:primitive",
    "@hackage_transformers_compat//:transformers-compat",
  ],
  prebuilt_dependencies = [
    "base",
    "transformers",
  ],
)
  """,
)

new_http_archive(
  name = "hackage_conduit_extra",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/conduit-extra-1.2.3.2/conduit-extra-1.2.3.2.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/conduit-extra-1.2.3.2.tar.gz",
  ],
  # sha256 = "b46582425fb30993f1a2e1172c4cea1358cdb2a55d342971927055117615447f",
  strip_prefix = "conduit-extra-1.2.3.2",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "conduit-extra",
  visibility = ["//visibility:public"],
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
)
  """,
)

new_http_archive(
  name = "hackage_http_conduit",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/http-conduit-2.2.4/http-conduit-2.2.4.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/http-conduit-2.2.4.tar.gz",
  ],
  # sha256 = "0e1e8e6f9675ae3c2f17108614b2c2f792e0647f794d713db97403b664ae5a24",
  strip_prefix = "http-conduit-2.2.4",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "http-conduit",
  visibility = ["//visibility:public"],
  srcs = [
    "Network/HTTP/Conduit.hs",
    "Network/HTTP/Client/Conduit.hs",
    "Network/HTTP/Simple.hs",
  ],
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
)
  """,
)

new_http_archive(
  name = "hackage_colour",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/colour-2.3.4/colour-2.3.4.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/colour-2.3.4.tar.gz",
  ],
  # sha256 = "90257dac3d9149b2b384184638b610bf95d7d180e1e4d3a329e038f18c6b7859",
  strip_prefix = "colour-2.3.4",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "colour",
  visibility = ["//visibility:public"],
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
  deps = [
  ],
  prebuilt_dependencies = [
    "base",
  ],
)
  """,
)

new_http_archive(
  name = "hackage_ansi_terminal",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/ansi-terminal-0.7.1.1/ansi-terminal-0.7.1.1.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/ansi-terminal-0.7.1.1.tar.gz",
  ],
  # sha256 = "ba1735bd76ff3ee3861e7b66aae453eafc0be84b3383e89100748808109fd4f8",
  strip_prefix = "ansi-terminal-0.7.1.1",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "ansi-terminal",
  hdrs = [
    "includes/Common-Include.hs",
    "includes/Common-Include-Emulator.hs",
    "includes/Common-Include-Enabled.hs",
    "includes/Exports-Include.hs",
  ],
  visibility = ["//visibility:public"],
  srcs = [
    "System/Console/ANSI.hs",
    "System/Console/ANSI/Types.hs",
    "System/Console/ANSI/Codes.hs",
    "System/Console/ANSI/Unix.hs",
  ],
  deps = [
    "@hackage_colour//:colour",
  ],
  prebuilt_dependencies = [
    "base",
  ],
  compiler_flags = ["-DUNIX"],
)
  """,
)

new_http_archive(
  name = "hackage_ansi_wl_pprint",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/ansi-wl-pprint-0.6.8.2/ansi-wl-pprint-0.6.8.2.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/ansi-wl-pprint-0.6.8.2.tar.gz",
  ],
  # sha256 = "8cb4b04401948c08feafa49e7ee0982b8492d599163107b6934093f1a912f54e",
  strip_prefix = "ansi-wl-pprint-0.6.8.2",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "ansi-wl-pprint",
  visibility = ["//visibility:public"],
  srcs = [
    "Text/PrettyPrint/ANSI/Leijen.hs",
    "Text/PrettyPrint/ANSI/Leijen/Internal.hs",
  ],
  deps = [
    "@hackage_ansi_terminal//:ansi-terminal",
  ],
  prebuilt_dependencies = [
    "base",
  ],
)
  """,
)

new_http_archive(
  name = "hackage_optparse_applicative",
  urls = [
    # Note: Hackage can serve a file with a different hash than what the
    # Stackage snapshot specifies. But fpco mirrors time out.
    "https://hackage.haskell.org/package/optparse-applicative-0.14.0.0/optparse-applicative-0.14.0.0.tar.gz",
    "https://s3.amazonaws.com/hackage.fpcomplete.com/package/optparse-applicative-0.14.0.0.tar.gz",
  ],
  # sha256 = "28f55d5d1dd190a0b48377ba014acb0bd6e9eb117a3f69af26556b9f74609715",
  strip_prefix = "optparse-applicative-0.14.0.0",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_toolchain",
)

haskell_library(
  name = "optparse-applicative",
  visibility = ["//visibility:public"],
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
  deps = [
    "@hackage_ansi_wl_pprint//:ansi-wl-pprint",
    "@hackage_transformers_compat//:transformers-compat",
  ],
  prebuilt_dependencies = [
    "base",
    "process",
    "transformers",
  ],
)
  """,
)
