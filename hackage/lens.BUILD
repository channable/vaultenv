load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

# This BUILD file describes the Lens package. It is more complex than the
# other build files for two reasons:
#
#  * The package has been split into multiple targets to increase
#    parallelization potential and incremental builds.
#  * This split was also required to work around a Safe Haskell issue.
#    Adding -XTrustworthy and -DTRUSTWORTHY in a few places solved this.
#
# Note that I do not actually understand the Lens library; the division into
# targets is based on the build order determined by Cabal, and it is still very
# sequential. There are some modules cross-referencing each other that look like
# this could be accidental (like Vector and At needing to be in the same
# library), but like I said, I know nothing about the Lens package.

# Note also that because of the more granular targets, dependents need to depend
# on all of the targets that they need, so the Lens dependents cannot use the
# automatically generated BUILD files any more. In the end this is for the
# better as it creates a more fine-grained mesh of targets, but is also tedious.

haskell_library(
  name = "internal",
  deps = [
    "@hackage_base_orphans//:base-orphans",
    "@hackage_comonad//:comonad",
    "@hackage_contravariant//:contravariant",
    "@hackage_distributive//:distributive",
    "@hackage_free//:free",
    "@hackage_mtl//:mtl",
    "@hackage_profunctors//:profunctors",
    "@hackage_reflection//:reflection",
    "@hackage_semigroupoids//:semigroupoids",
    "@hackage_text//:text",
    "@hackage_text//:lazy",
    "@hackage_vector//:vector",
  ],
  prebuilt_dependencies = [
    "base",
    "bytestring",
    "containers",
    "template-haskell",
    "transformers",
  ],
  srcs = [
    "src/Control/Lens/Internal/Bazaar.hs",
    "src/Control/Lens/Internal/CTypes.hs",
    "src/Control/Lens/Internal/Coerce.hs",
    "src/Control/Lens/Internal/Context.hs",
    "src/Control/Lens/Internal/Fold.hs",
    "src/Control/Lens/Internal/Getter.hs",
    "src/Control/Lens/Internal/Indexed.hs",
    "src/Control/Lens/Internal/Instances.hs",
    "src/Control/Lens/Internal/Iso.hs",
    "src/Control/Lens/Internal/Level.hs",
    "src/Control/Lens/Internal/List.hs",
    "src/Control/Lens/Internal/Magma.hs",
    "src/Control/Lens/Internal/Prism.hs",
    "src/Control/Lens/Internal/Review.hs",
    "src/Control/Lens/Internal/Setter.hs",
    "src/Control/Lens/Internal/TH.hs",
    "src/Control/Lens/Internal/Zoom.hs",

  ],
  src_strip_prefix = "src",
)

haskell_library(
  name = "type_base",
  visibility = ["//visibility:public"],
  deps = [
    ":internal",
    "@hackage_call_stack//:call-stack",
    "@hackage_comonad//:comonad",
    "@hackage_contravariant//:contravariant",
    "@hackage_mtl//:mtl",
    "@hackage_profunctors//:profunctors",
    "@hackage_reflection//:reflection",
    "@hackage_semigroupoids//:semigroupoids",
    "@hackage_tagged//:tagged",
  ],
  prebuilt_dependencies = [
    "base",
    "transformers",
  ],
  compiler_flags = ["-XTrustworthy"],
  srcs = [
    "src/Control/Lens/Fold.hs",
    "src/Control/Lens/Getter.hs",
    "src/Control/Lens/Review.hs",
    "src/Control/Lens/Setter.hs",
    "src/Control/Lens/Type.hs",
  ],
  src_strip_prefix = "src",
)

haskell_library(
  name = "type",
  visibility = ["//visibility:public"],
  deps = [
    ":internal",
    ":type_base",
    "@hackage_call_stack//:call-stack",
    "@hackage_comonad//:comonad",
    "@hackage_contravariant//:contravariant",
    "@hackage_distributive//:distributive",
    "@hackage_kan_extensions//:kan-extensions",
    "@hackage_mtl//:mtl",
    "@hackage_profunctors//:profunctors",
    "@hackage_reflection//:reflection",
    "@hackage_semigroupoids//:semigroupoids",
    "@hackage_tagged//:tagged",
    "@hackage_vector//:vector",
  ],
  prebuilt_dependencies = [
    "base",
    "containers",
    "ghc-prim",
    "transformers",
  ],
  compiler_flags = [
    "-XTrustworthy",
    # C preprocessor warns on ?? trigraph which fails the build.
    # Disable that warning.
    "-optP -Wno-trigraphs"
  ],
  srcs = [
    "src/Control/Lens/Lens.hs",
    "src/Control/Lens/Level.hs",
    "src/Control/Lens/Prism.hs",
    "src/Control/Lens/Reified.hs",
    "src/Control/Lens/Traversal.hs",
    "src/Control/Lens/Tuple.hs",
  ],
  src_strip_prefix = "src",
)

haskell_library(
  name = "indexed",
  deps = [
    ":internal",
    ":type_base",
    ":type",
    "@hackage_comonad//:comonad",
    "@hackage_contravariant//:contravariant",
    "@hackage_exceptions//:exceptions",
    "@hackage_free//:free",
    "@hackage_profunctors//:profunctors",
    "@hackage_reflection//:reflection",
    "@hackage_semigroupoids//:semigroupoids",
    "@hackage_tagged//:tagged",
    "@hackage_unordered_containers//:unordered-containers",
    "@hackage_vector//:vector",
  ],
  prebuilt_dependencies = [
    "array",
    "base",
    "containers",
    "transformers",
  ],
  compiler_flags = ["-XTrustworthy"],
  srcs = [
    "src/Control/Lens/Internal/Exception.hs",
    "src/Control/Lens/Indexed.hs",
  ],
  src_strip_prefix = "src",
)

haskell_library(
  name = "control",
  visibility = ["//visibility:public"],
  deps = [
    ":internal",
    ":type_base",
    ":type",
    "@hackage_bifunctors//:bifunctors",
    "@hackage_comonad//:comonad",
    "@hackage_contravariant//:contravariant",
    "@hackage_exceptions//:exceptions",
    "@hackage_free//:free",
    "@hackage_free//:monad_comonad",
    "@hackage_hashable//:hashable",
    "@hackage_mtl//:mtl",
    "@hackage_profunctors//:profunctors",
    "@hackage_semigroupoids//:semigroupoids",
    "@hackage_tagged//:tagged",
    "@hackage_text//:text",
    "@hackage_text//:lazy",
    "@hackage_unordered_containers//:unordered-containers",
    "@hackage_vector//:vector",
  ],
  prebuilt_dependencies = [
    "base",
    "bytestring",
    "containers",
    "transformers",
  ],
  compiler_flags = ["-XTrustworthy"],
  srcs = [
    "src/Control/Lens/Equality.hs",
    "src/Control/Lens/Iso.hs",
    "src/Control/Lens/Empty.hs",
    "src/Control/Lens/Cons.hs",
    "src/Control/Lens/Wrapped.hs",
    "src/Control/Lens/Zoom.hs",
  ],
  src_strip_prefix = "src",
)

haskell_library(
  name = "data_lens",
  visibility = ["//visibility:public"],
  deps = [
    ":internal",
    ":type_base",
    ":type",
    ":control",
    "@hackage_unordered_containers//:unordered-containers",
  ],
  prebuilt_dependencies = ["base"],
  compiler_flags = ["-XTrustworthy"],
  srcs = ["src/Data/Data/Lens.hs"],
  src_strip_prefix = "src",
)

haskell_library(
  name = "map_set",
  visibility = ["//visibility:public"],
  deps = [
    ":type_base",
    ":control",
    "@hackage_hashable//:hashable",
    "@hackage_unordered_containers//:unordered-containers",
  ],
  prebuilt_dependencies = [
    "base",
    "containers",
  ],
  srcs = [
    "src/Data/HashSet/Lens.hs",
    "src/Data/Map/Lens.hs",
    "src/Data/Set/Lens.hs",
  ],
  src_strip_prefix = "src",
)

haskell_library(
  name = "text",
  visibility = ["//visibility:public"],
  deps = [
    ":type_base",
    ":type",
    ":control",
    "@hackage_text//:text",
    "@hackage_text//:lazy",
    "@hackage_text//:builder",
    "@hackage_vector//:vector",
  ],
  prebuilt_dependencies = [
    "base",
    "bytestring",
  ],
  srcs = [
    "src/Data/Text/Lazy/Lens.hs",
    "src/Data/Text/Lens.hs",
    "src/Data/Text/Strict/Lens.hs",
  ],
  src_strip_prefix = "src",
)

haskell_library(
  name = "control_lens",
  visibility = ["//visibility:public"],
  deps = [
    ":control",
    ":data_lens",
    ":indexed",
    ":internal",
    ":map_set",
    ":text",
    ":type",
    ":type_base",
    "@hackage_free//:free",
    "@hackage_free//:monad_comonad",
    "@hackage_hashable//:hashable",
    "@hackage_mtl//:mtl",
    "@hackage_text//:text",
    "@hackage_text//:lazy",
    "@hackage_th_abstraction//:th-abstraction",
    "@hackage_unordered_containers//:unordered-containers",
    "@hackage_vector//:vector",
  ],
  prebuilt_dependencies = [
    "array",
    "base",
    "bytestring",
    "containers",
    "template-haskell",
    "transformers",
  ],
  compiler_flags = [
    "-DTRUSTWORTHY=1",
  ],
  srcs = [
    "src/Control/Lens.hs",
    "src/Control/Lens/At.hs",
    "src/Control/Lens/Each.hs",
    "src/Control/Lens/Internal/ByteString.hs",
    "src/Control/Lens/Internal/FieldTH.hs",
    "src/Control/Lens/Internal/PrismTH.hs",
    "src/Control/Lens/Plated.hs",
    "src/Control/Lens/TH.hs",
    "src/Data/Vector/Generic/Lens.hs",
    "src/Data/Vector/Lens.hs",
    "src/Language/Haskell/TH/Lens.hs",
  ],
  src_strip_prefix = "src",
)

haskell_library(
  name = "lens",
  visibility = ["//visibility:public"],
  deps = [
    ":internal",
    ":type_base",
    ":type",
    ":control",
    ":indexed",
    ":data_lens",
    ":control_lens",
    "@hackage_call_stack//:call-stack",
    "@hackage_mtl//:mtl",
    "@hackage_semigroupoids//:semigroupoids",
    "@hackage_profunctors//:profunctors",
    "@hackage_parallel//:parallel",
    "@hackage_exceptions//:exceptions",
  ],
  prebuilt_dependencies = [
    "array",
    "base",
    "bytestring",
    "containers",
    "filepath",
    "transformers",
  ],
  srcs = [
    "src/Control/Exception/Lens.hs",
    "src/Control/Lens/Combinators.hs",
    "src/Control/Lens/Extras.hs",
    "src/Control/Lens/Internal.hs",
    "src/Control/Lens/Internal/Deque.hs",
    "src/Control/Lens/Operators.hs",
    "src/Control/Monad/Error/Lens.hs",
    "src/Control/Parallel/Strategies/Lens.hs",
    "src/Control/Seq/Lens.hs",
    "src/Data/Array/Lens.hs",
    "src/Data/Bits/Lens.hs",
    "src/Data/ByteString/Lazy/Lens.hs",
    "src/Data/ByteString/Lens.hs",
    "src/Data/ByteString/Strict/Lens.hs",
    "src/Data/Complex/Lens.hs",
    "src/Data/Dynamic/Lens.hs",
    "src/Data/IntSet/Lens.hs",
    "src/Data/List/Lens.hs",
    "src/Data/Sequence/Lens.hs",
    "src/Data/Tree/Lens.hs",
    "src/Data/Typeable/Lens.hs",
    "src/GHC/Generics/Lens.hs",
    "src/Numeric/Lens.hs",
    "src/System/Exit/Lens.hs",
    "src/System/FilePath/Lens.hs",
    "src/System/IO/Error/Lens.hs",
  ],
  src_strip_prefix = "src",
)
