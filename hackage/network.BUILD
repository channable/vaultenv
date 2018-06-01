load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

# Dummy target to propagate required preprocessor defines.
cc_library(
  name = "network_defines",
  defines = ["_GNU_SOURCE", "CALLCONV=ccall"],
)

haskell_library(
  name = "network",
  visibility = ["//visibility:public"],
  hdrs = [
    "include/HsNet.h",
    "include/HsNetworkConfig.h",
  ],
  srcs = [
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
  deps = [
    ":network_defines",
  ],
  prebuilt_dependencies = [
    "bytestring",
    "unix",
    "base",
  ],
  compiler_flags = ["-cpp"],
)
