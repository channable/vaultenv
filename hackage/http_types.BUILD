load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
  name = "http-types",
  # Normally we prefer to avoid setting the version in a library definition,
  # because it makes the build definition less portable. For many updates, the
  # build definition does not need to be changed, specifying the version causes
  # unnecessary churn. But for http-types, the http-client package depends on it
  # and uses a MIN_VERSION_http_types macro for conditional compilation. So in
  # this case we do need an accurate version.
  version = "0.9.1",
  visibility = ["//visibility:public"],
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
  srcs = [
    "Network/HTTP/Types.hs",
    "Network/HTTP/Types/QueryLike.hs",
    "Network/HTTP/Types/Header.hs",
    "Network/HTTP/Types/Method.hs",
    "Network/HTTP/Types/URI.hs",
    "Network/HTTP/Types/Status.hs",
    "Network/HTTP/Types/Version.hs",
  ],
)
