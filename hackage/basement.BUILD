load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

# This file contains the build description for the Basement package. It has
# been split in many targets, because the Basement package was a bottleneck on
# the critical chain. By splitting into multiple targets, there is the potential
# for more paralellism, and dependents can take only the parts they need,
# allowing for further build time reductions.

cc_library(
  name = "cbits",
  includes = ["cbits"],
  deps = ["@ghc//:threaded-rts"],
  srcs = [
    "cbits/foundation_mem.c",
    "cbits/foundation_rts.c",
  ],
  hdrs = [
    "cbits/foundation_prim.h",
    "cbits/foundation_system.h",
  ],
)

haskell_library(
  name = "compat_and_numerical",
  visibility = ["//visibility:public"],
  deps = [],
  prebuilt_dependencies = [
    "base",
    "ghc-prim",
  ],
  compiler_flags = [
    "-XNoImplicitPrelude",
    "-XRebindableSyntax",
    "-XTypeFamilies",
    "-XBangPatterns",
  ],
  srcs = [
    "Basement/Bounded.hs",
    "Basement/Compat/Base.hs",
    "Basement/Compat/IsList.hs",
    "Basement/Compat/Natural.hs",
    "Basement/Compat/NumLiteral.hs",
    "Basement/Compat/PrimTypes.hs",
    "Basement/Compat/Primitive.hs",
    "Basement/Compat/Semigroup.hs",
    "Basement/Compat/Typeable.hs",
    "Basement/IntegralConv.hs",
    "Basement/Nat.hs",
    "Basement/Numerical/Additive.hs",
    "Basement/Numerical/Conversion.hs",
    "Basement/Numerical/Multiplicative.hs",
    "Basement/Numerical/Number.hs",
    "Basement/Numerical/Subtractive.hs",
    "Basement/Types/Char7.hs",
    "Basement/Types/OffsetSize.hs",
    "Basement/Types/Word128.hs",
    "Basement/Types/Word256.hs",
  ],
)

haskell_library(
  name = "compat_misc",
  visibility = ["//visibility:public"],
  deps = [":compat_and_numerical"],
  compiler_flags = [
    "-XNoImplicitPrelude",
    "-XBangPatterns",
  ],
  srcs = [
    "Basement/Compat/Bifunctor.hs",
    "Basement/Compat/CallStack.hs",
    "Basement/Compat/ExtList.hs",
    "Basement/Compat/Identity.hs",
    "Basement/Compat/MonadTrans.hs",
  ],
)

haskell_library(
  name = "core",
  visibility = ["//visibility:public"],
  deps = [":compat_and_numerical"],
  compiler_flags = [
    "-XNoImplicitPrelude",
    "-XTypeFamilies",
    "-XBangPatterns",
    "-DARCH_IS_LITTLE_ENDIAN",
  ],
  srcs = [
    "Basement/Endianness.hs",
    "Basement/Exception.hs",
    "Basement/Monad.hs",
    "Basement/NonEmpty.hs",
    "Basement/NormalForm.hs",
    "Basement/PrimType.hs",
    "Basement/Runtime.hs",
  ],
)

haskell_library(
  name = "memory",
  visibility = ["//visibility:public"],
  deps = [
    ":compat_and_numerical",
    ":compat_misc",
    ":core",
  ],
  compiler_flags = [
    "-XNoImplicitPrelude",
    "-XTypeFamilies",
    "-XBangPatterns",
  ],
  srcs = [
    "Basement/Alg/Class.hs",
    "Basement/Alg/Foreign/Prim.hs",
    "Basement/Alg/Foreign/UTF8.hs",
    "Basement/Alg/Mutable.hs",
    "Basement/Alg/Native/Prim.hs",
    "Basement/Alg/Native/UTF8.hs",
    "Basement/Alg/PrimArray.hs",
    "Basement/Base16.hs",
    "Basement/Bindings/Memory.hs",
    "Basement/Block.hs",
    "Basement/Block/Base.hs",
    "Basement/Block/Mutable.hs",
    "Basement/BoxedArray.hs",
    "Basement/FinalPtr.hs",
    "Basement/MutableBuilder.hs",
    "Basement/UArray.hs",
    "Basement/UArray/Base.hs",
    "Basement/UArray/Mutable.hs",
    "Basement/UTF8/Base.hs",
    "Basement/UTF8/Helper.hs",
    "Basement/UTF8/Table.hs",
    "Basement/UTF8/Types.hs",
  ],
)

haskell_library(
  name = "ascii",
  visibility = ["//visibility:public"],
  deps = [
    ":compat_and_numerical",
    ":memory",
  ],
  compiler_flags = ["-XNoImplicitPrelude"],
  srcs = [
    "Basement/Types/AsciiString.hs",
  ],
)

haskell_library(
  name = "floating",
  visibility = ["//visibility:public"],
  deps = [":compat_and_numerical"],
  compiler_flags = ["-XNoImplicitPrelude"],
  srcs = [
    "Basement/Floating.hs",
  ],
)

haskell_library(
  name = "encoding",
  visibility = ["//visibility:public"],
  deps = [
    ":compat_and_numerical",
    ":compat_misc",
    ":core",
    ":floating",
    ":memory",
  ],
  prebuilt_dependencies = ["base"],
  compiler_flags = [
    "-XNoImplicitPrelude",
    "-XTypeFamilies",
    "-XBangPatterns",
    "-XDeriveDataTypeable",
    "-XRebindableSyntax",
  ],
  srcs = [
    "Basement/Alg/Foreign/String.hs",
    "Basement/Alg/Native/String.hs",
    "Basement/String.hs",
    "Basement/String/Encoding/ASCII7.hs",
    "Basement/String/Encoding/Encoding.hs",
    "Basement/String/Encoding/ISO_8859_1.hs",
    "Basement/String/Encoding/UTF16.hs",
    "Basement/String/Encoding/UTF32.hs",
  ],
)

haskell_library(
  name = "misc",
  visibility = ["//visibility:public"],
  deps = [
    ":ascii",
    ":compat_and_numerical",
    ":compat_misc",
    ":core",
    ":memory",
  ],
  compiler_flags = [
    "-XNoImplicitPrelude",
    "-XTypeFamilies",
    "-XBangPatterns",
  ],
  srcs = [
    "Basement/Environment.hs",
    "Basement/Error.hs",
    "Basement/Imports.hs",
    "Basement/Show.hs",
    "Basement/Types/Ptr.hs",
  ],
)

haskell_library(
  name = "terminal",
  visibility = ["//visibility:public"],
  deps = [
    ":cbits",
    ":encoding",
    ":compat_and_numerical",
    ":misc",
  ],
  compiler_flags = ["-XNoImplicitPrelude"],
  srcs = [
    "Basement/Terminal.hs",
    "Basement/Terminal/ANSI.hs",
    "Basement/Terminal/Size.hsc",
  ],
)

haskell_library(
  name = "sized",
  visibility = ["//visibility:public"],
  deps = [
    ":ascii",
    ":compat_and_numerical",
    ":compat_misc",
    ":core",
    ":encoding",
    ":memory",
  ],
  compiler_flags = [
    "-XNoImplicitPrelude",
    "-XKindSignatures",
    "-XTypeFamilies",
  ],
  srcs = [
    "Basement/BlockN.hs",
    "Basement/From.hs",
    "Basement/Sized/Block.hs",
    "Basement/Sized/List.hs",
    "Basement/Sized/UVect.hs",
    "Basement/Sized/Vect.hs",
    "Basement/These.hs",
  ],
)
