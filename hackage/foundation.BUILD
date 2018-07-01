load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

cc_library(
  name = "cbits",
  includes = ["cbits"],
  deps = ["@ghc//:threaded-rts"],
  hdrs = [
    "cbits/foundation_prim.h",
    "cbits/foundation_bits.h",
    "cbits/foundation_system.h",
  ],
  srcs = [
    "cbits/foundation_random.c",
    "cbits/foundation_network.c",
    "cbits/foundation_time.c",
    "cbits/foundation_utf8.c",
  ],
)

haskell_library(
  name = "numerical",
  visibility = ["//visibility:public"],
  deps = [
    "@hackage_basement//:compat_and_numerical",
  ],
  prebuilt_dependencies = [
    "base",
    "ghc-prim"
  ],
  compiler_flags = [
    "-XNoImplicitPrelude",
  ],
  srcs = [
    "Foundation/Numerical.hs",
    "Foundation/Numerical/Floating.hs",
  ],
)

haskell_library(
  name = "collection_core",
  visibility = ["//visibility:public"],
  deps = [
    ":numerical",
    "@hackage_basement//:compat_and_numerical",
    "@hackage_basement//:core",
    "@hackage_basement//:memory",
  ],
  prebuilt_dependencies = ["base"],
  compiler_flags = [
    "-XNoImplicitPrelude",
    "-XTypeFamilies",
  ],
  srcs = [
    "Foundation/Collection/List.hs",
    "Foundation/Collection/Keyed.hs",
    "Foundation/Collection/Mutable.hs",
    "Foundation/Collection/Mappable.hs",
  ],
)

haskell_library(
  name = "collection",
  visibility = ["//visibility:public"],
  deps = [
    ":numerical",
    ":collection_core",
    "@hackage_basement//:ascii",
    "@hackage_basement//:compat_and_numerical",
    "@hackage_basement//:compat_misc",
    "@hackage_basement//:core",
    "@hackage_basement//:encoding",
    "@hackage_basement//:memory",
  ],
  prebuilt_dependencies = ["base"],
  compiler_flags = [
    "-XNoImplicitPrelude",
    "-XTypeFamilies",
  ],
  srcs = [
    "Foundation/Collection/Foldable.hs",
    "Foundation/Collection/Zippable.hs",
    "Foundation/Collection/Sequential.hs",
    "Foundation/Collection/InnerFunctor.hs",
    "Foundation/Collection/Indexed.hs",
    "Foundation/Collection.hs",
    "Foundation/Collection/Collection.hs",
    "Foundation/Collection/Copy.hs",
    "Foundation/Collection/Element.hs",
    "Foundation/Collection/Buildable.hs",
  ],
)

haskell_library(
  name = "string",
  visibility = ["//visibility:public"],
  deps = [
    "@hackage_basement//:compat_and_numerical",
    "@hackage_basement//:encoding",
  ],
  compiler_flags = ["-XNoImplicitPrelude"],
  srcs = [
    "Foundation/String.hs",
    "Foundation/String/Builder.hs",
    "Foundation/String/Read.hs",
  ],
)

haskell_library(
  name = "system",
  visibility = ["//visibility:public"],
  deps = [
    ":cbits",
    ":numerical",
    ":string",
    "@hackage_basement//:compat_and_numerical",
    "@hackage_basement//:core",
    "@hackage_basement//:memory",
  ],
  prebuilt_dependencies = ["base"],
  compiler_flags = [
    "-XNoImplicitPrelude",
    "-XDeriveDataTypeable",
  ],
  srcs = [
    "Foundation/System/Bindings.hs",
    "Foundation/System/Bindings/Hs.hs",
    "Foundation/System/Bindings/Linux.hsc",
    "Foundation/System/Bindings/Network.hsc",
    "Foundation/System/Bindings/Posix.hsc",
    "Foundation/System/Bindings/PosixDef.hsc",
    "Foundation/System/Bindings/Time.hsc",
    "Foundation/System/Entropy.hs",
    "Foundation/System/Entropy/Common.hs",
    "Foundation/System/Entropy/Unix.hs",
    "Foundation/System/Info.hs",
  ],
)

haskell_library(
  name = "bits",
  visibility = ["//visibility:public"],
  deps = [
    ":numerical",
    "@hackage_basement//:compat_and_numerical",
    "@hackage_basement//:core",
    "@hackage_basement//:sized",
    "@hackage_basement//:ascii",
    "@hackage_basement//:memory",
  ],
  prebuilt_dependencies = ["base"],
  compiler_flags = ["-XNoImplicitPrelude"],
  srcs = [
    "Foundation/Bits.hs",
    "Foundation/Primitive.hs",
  ],
)

haskell_library(
  name = "array",
  visibility = ["//visibility:public"],
  deps = [
    ":numerical",
    ":collection",
    ":bits",
    "@hackage_basement//:compat_and_numerical",
    "@hackage_basement//:compat_misc",
    "@hackage_basement//:core",
    "@hackage_basement//:memory",
  ],
  prebuilt_dependencies = ["base"],
  compiler_flags = [
    "-XNoImplicitPrelude",
    "-XTypeFamilies",
  ],
  srcs = [
    "Foundation/Array.hs",
    "Foundation/Array/Bitmap.hs",
    "Foundation/Array/Chunked/Unboxed.hs",
    "Foundation/Array/Internal.hs",
  ],
)

haskell_library(
  name = "tuple",
  visibility = ["//visibility:public"],
  deps = [
    ":bits",
    "@hackage_basement//:compat_and_numerical",
    "@hackage_basement//:compat_misc",
  ],
  prebuilt_dependencies = ["base"],
  compiler_flags = [
    "-XNoImplicitPrelude",
    "-XTypeFamilies",
  ],
  srcs = [
    "Foundation/Tuple.hs",
    "Foundation/Tuple/Nth.hs",
  ],
)

haskell_library(
  name = "hashing",
  visibility = ["//visibility:public"],
  deps = [
    ":numerical",
    ":string",
    ":array",
    ":collection",
    ":tuple",
    ":bits",
    "@hackage_basement//:compat_and_numerical",
    "@hackage_basement//:misc",
    "@hackage_basement//:core",
    "@hackage_basement//:memory",
  ],
  compiler_flags = [
    "-XNoImplicitPrelude",
    "-XTypeFamilies",
  ],
  srcs = [
    "Foundation/Hashing.hs",
    "Foundation/Hashing/FNV.hs",
    "Foundation/Hashing/Hashable.hs",
    "Foundation/Hashing/Hasher.hs",
    "Foundation/Hashing/SipHash.hs",
  ],
)

haskell_library(
  name = "storable",
  visibility = ["//visibility:public"],
  deps = [
    ":collection",
    ":numerical",
    "@hackage_basement//:core",
    "@hackage_basement//:compat_and_numerical",
  ],
  compiler_flags = ["-XNoImplicitPrelude"],
  srcs = [
    "Foundation/Class/Storable.hs",
  ],
)

haskell_library(
  name = "random",
  visibility = ["//visibility:public"],
  deps = [
    ":numerical",
    ":bits",
    ":system",
    ":storable",
    "@hackage_basement//:compat_and_numerical",
    "@hackage_basement//:compat_misc",
    "@hackage_basement//:misc",
    "@hackage_basement//:core",
    "@hackage_basement//:memory",
  ],
  compiler_flags = [
    "-XNoImplicitPrelude",
    "-XBangPatterns",
    "-XTypeFamilies",
  ],
  srcs = [
    "Foundation/Random.hs",
    "Foundation/Random/ChaChaDRG.hs",
    "Foundation/Random/Class.hs",
    "Foundation/Random/DRG.hs",
    "Foundation/Random/XorShift.hs",
  ],
)

haskell_library(
  name = "monad",
  visibility = ["//visibility:public"],
  deps = [
    ":numerical",
    "@hackage_basement//:compat_and_numerical",
    "@hackage_basement//:compat_misc",
    "@hackage_basement//:misc",
    "@hackage_basement//:core",
  ],
  prebuilt_dependencies = ["base"],
  compiler_flags = [
    "-XNoImplicitPrelude",
    "-XTypeFamilies",
  ],
  srcs = [
    "Foundation/Monad.hs",
    "Foundation/Monad/Base.hs",
    "Foundation/Monad/Except.hs",
    "Foundation/Monad/Exception.hs",
    "Foundation/Monad/Identity.hs",
    "Foundation/Monad/MonadIO.hs",
    "Foundation/Monad/Reader.hs",
    "Foundation/Monad/State.hs",
    "Foundation/Monad/Transformer.hs",
  ],
)

haskell_library(
  name = "vfs",
  visibility = ["//visibility:public"],
  deps = [
    ":array",
    ":collection",
    ":string",
    "@hackage_basement//:compat_and_numerical",
  ],
  prebuilt_dependencies = ["base"],
  compiler_flags = [
    "-XNoImplicitPrelude",
    "-XTypeFamilies",
  ],
  srcs = [
    "Foundation/VFS.hs",
    "Foundation/VFS/FilePath.hs",
    "Foundation/VFS/Path.hs",
    "Foundation/VFS/URI.hs",
  ],
)

haskell_library(
  name = "foreign",
  visibility = ["//visibility:public"],
  deps = [
    ":vfs",
    ":collection",
    "@hackage_basement//:compat_and_numerical",
    "@hackage_basement//:memory",
    "@hackage_basement//:misc",
  ],
  prebuilt_dependencies = ["base"],
  compiler_flags = ["-XNoImplicitPrelude"],
  srcs = [
    "Foundation/Foreign.hs",
    "Foundation/Foreign/Alloc.hs",
    "Foundation/Foreign/MemoryMap.hs",
    "Foundation/Foreign/MemoryMap/Posix.hsc",
    "Foundation/Foreign/MemoryMap/Types.hs",
  ],
)

haskell_library(
  name = "io",
  visibility = ["//visibility:public"],
  deps = [
    ":array",
    ":collection",
    ":foreign",
    ":numerical",
    ":vfs",
    "@hackage_basement//:compat_and_numerical",
    "@hackage_basement//:memory",
    "@hackage_basement//:misc",
  ],
  prebuilt_dependencies = ["base"],
  compiler_flags = ["-XNoImplicitPrelude"],
  srcs = [
    "Foundation/IO.hs",
    "Foundation/IO/File.hs",
    "Foundation/IO/FileMap.hs",
    "Foundation/IO/Terminal.hs",
  ],
)

haskell_library(
  name = "list",
  visibility = ["//visibility:public"],
  deps = [
    ":collection",
    "@hackage_basement//:compat_and_numerical",
    "@hackage_basement//:compat_misc",
    "@hackage_basement//:sized",
  ],
  compiler_flags = [
    "-XNoImplicitPrelude",
    "-XTypeFamilies",
  ],
  srcs = [
    "Foundation/List/DList.hs",
    "Foundation/List/ListN.hs",
  ],
)

haskell_library(
  name = "core",
  visibility = ["//visibility:public"],
  deps = [
    ":tuple",
    ":bits",
    ":numerical",
    ":collection",
    ":io",
    ":array",
    ":string",
    "@hackage_basement//:compat_and_numerical",
    "@hackage_basement//:compat_misc",
    "@hackage_basement//:misc",
  ],
  prebuilt_dependencies = ["base"],
  compiler_flags = ["-XNoImplicitPrelude"],
  srcs = [
    "Foundation/Partial.hs",
    "Foundation.hs",
  ],
)

# TODO(ruuda): Split up this library further.
haskell_library(
  name = "misc",
  visibility = ["//visibility:public"],
  deps = [
    ":array",
    ":bits",
    ":collection_core",
    ":collection",
    ":foreign",
    ":hashing",
    ":io",
    ":monad",
    ":numerical",
    ":core",
    ":random",
    ":storable",
    ":string",
    ":system",
    "@hackage_basement//:compat_and_numerical",
    "@hackage_basement//:compat_misc",
    "@hackage_basement//:core",
    "@hackage_basement//:encoding",
    "@hackage_basement//:memory",
    "@hackage_basement//:misc",
  ],
  prebuilt_dependencies = ["base"],
  compiler_flags = [
    "-XNoImplicitPrelude",
    "-XRebindableSyntax",
    "-XTypeFamilies",
    "-XBangPatterns",
  ],
  srcs = [
    "Foundation/Class/Bifunctor.hs",
    "Foundation/Conduit.hs",
    "Foundation/Conduit/Internal.hs",
    "Foundation/Conduit/Textual.hs",
    "Foundation/Exception.hs",
    "Foundation/Math/Trigonometry.hs",
    "Foundation/Network/HostName.hsc",
    "Foundation/Network/IPv4.hs",
    "Foundation/Network/IPv6.hs",
    "Foundation/Parser.hs",
    "Foundation/Strict.hs",
    "Foundation/Time/Bindings.hs",
    "Foundation/Time/StopWatch.hs",
    "Foundation/Time/Types.hs",
    "Foundation/Timing.hs",
    "Foundation/Timing/Main.hs",
    "Foundation/UUID.hs",
  ],
)

haskell_library(
  name = "check",
  visibility = ["//visibility:public"],
  deps = [
    ":bits",
    ":collection",
    ":hashing",
    ":io",
    ":list",
    ":monad",
    ":numerical",
    ":random",
    ":string",
    "@hackage_basement//:compat_and_numerical",
    "@hackage_basement//:compat_misc",
    "@hackage_basement//:misc",
    "@hackage_basement//:sized",
    "@hackage_basement//:terminal",
  ],
  prebuilt_dependencies = ["base"],
  compiler_flags = [
    "-XNoImplicitPrelude",
    "-XTypeFamilies",
    "-XBangPatterns",
  ],
  srcs = [
    "Foundation/Check.hs",
    "Foundation/Check/Arbitrary.hs",
    "Foundation/Check/Config.hs",
    "Foundation/Check/Gen.hs",
    "Foundation/Check/Main.hs",
    "Foundation/Check/Print.hs",
    "Foundation/Check/Property.hs",
    "Foundation/Check/Types.hs",
  ],
)
