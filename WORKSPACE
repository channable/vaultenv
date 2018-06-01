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
