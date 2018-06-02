load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "reflection",
  # Normally we prefer to avoid setting the version in a library definition,
  # because it makes the build definition less portable. For many updates, the
  # build definition does not need to be changed, specifying the version causes
  # unnecessary churn. But for reflection, the lens package depends on it and
  # uses a MIN_VERSION_reflection macro for conditional compilation. So in this
  # case we do need an accurate version.
  version = "2.1.3",
  visibility = ["//visibility:public"],
  prebuilt_dependencies = [
    "base",
    "template-haskell",
  ],
  srcs = ["fast/Data/Reflection.hs"],
  src_strip_prefix = "fast",
)
