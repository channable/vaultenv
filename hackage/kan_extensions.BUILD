load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "kan-extensions",
  visibility = ["//visibility:public"],
  deps = [
    "@hackage_adjunctions//:adjunctions",
    "@hackage_comonad//:comonad",
    "@hackage_contravariant//:contravariant",
    "@hackage_distributive//:distributive",
    "@hackage_free//:free",
    "@hackage_free//:monad_comonad",
    "@hackage_mtl//:mtl",
    "@hackage_semigroupoids//:semigroupoids",
    "@hackage_tagged//:tagged",
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
    "src/Data/Functor/Contravariant/Coyoneda.hs",
    "src/Data/Functor/Contravariant/Day.hs",
    "src/Data/Functor/Contravariant/Yoneda.hs",
    "src/Data/Functor/Coyoneda.hs",
    "src/Data/Functor/Day.hs",
    "src/Data/Functor/Day/Curried.hs",
    "src/Data/Functor/Kan/Lan.hs",
    "src/Data/Functor/Kan/Ran.hs",
    "src/Data/Functor/Yoneda.hs",
  ],
  src_strip_prefix = "src",
)
