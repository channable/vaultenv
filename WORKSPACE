http_archive(
  name = "io_tweag_rules_haskell",
  strip_prefix = "rules_haskell-0.5",
  urls = ["https://github.com/tweag/rules_haskell/archive/v0.5.tar.gz"],
  sha256 = "0296c56ddca2dae172eccdecded815aea45985fa3cdd6d66ab392011beb89cdd",
)

load("@io_tweag_rules_haskell//haskell:repositories.bzl", "haskell_repositories")

haskell_repositories()

load("@io_tweag_rules_haskell//haskell:haskell.bzl", "ghc_bindist")

# This repository rule creates @ghc repository.
ghc_bindist(
  name    = "ghc",
  version = "8.2.2",
)

register_toolchains("//:ghc")
