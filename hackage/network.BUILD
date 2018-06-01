load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

cc_library(
  name = "cbits",
  defines = ["_GNU_SOURCE", "CALLCONV=ccall"],
  includes = ["include"],
  srcs = [
    "cbits/HsNet.c"
  ],
  hdrs = [
    "include/HsNet.h",
    "include/HsNetworkConfig.h",
  ],
)

haskell_library(
  name = "network",
  visibility = ["//visibility:public"],
  deps = [
    ":cbits",
  ],
  prebuilt_dependencies = [
    "bytestring",
    "unix",
    "base",
  ],
  compiler_flags = ["-cpp"],
  srcs = [
    "Network.hs",
    "Network/BSD.hsc",
    "Network/Socket.hsc",
    "Network/Socket/ByteString.hsc",
    "Network/Socket/ByteString/Lazy.hs",
    "Network/Socket/Internal.hsc",
    "Network/Socket/ByteString/Internal.hs",
    "Network/Socket/Types.hsc",
    "Network/Socket/ByteString/IOVec.hsc",
    "Network/Socket/ByteString/Lazy/Posix.hs",
    "Network/Socket/ByteString/MsgHdr.hsc",
  ],
)
