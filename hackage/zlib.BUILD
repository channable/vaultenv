load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

cc_library(
  name = "cbits",
  srcs = [
    "cbits/adler32.c",
    "cbits/compress.c",
    "cbits/crc32.c",
    "cbits/deflate.c",
    "cbits/infback.c",
    "cbits/inffast.c",
    "cbits/inflate.c",
    "cbits/inftrees.c",
    "cbits/trees.c",
    "cbits/uncompr.c",
    "cbits/zutil.c",
  ],
  hdrs = [
    "cbits/crc32.h",
    "cbits/deflate.h",
    "cbits/gzguts.h",
    "cbits/inffast.h",
    "cbits/inffixed.h",
    "cbits/inflate.h",
    "cbits/inftrees.h",
    "cbits/trees.h",
    "cbits/zconf.h",
    "cbits/zlib.h",
    "cbits/zutil.h",
  ],
)

haskell_library(
  name = "zlib",
  visibility = ["//visibility:public"],
  deps = [
    ":cbits",
  ],
  prebuilt_dependencies = [
    "bytestring",
    "base",
  ],
  srcs = [
    "Codec/Compression/GZip.hs",
    "Codec/Compression/Zlib.hs",
    "Codec/Compression/Zlib/Raw.hs",
    "Codec/Compression/Zlib/Internal.hs",
    "Codec/Compression/Zlib/Stream.hsc",
  ],
)
