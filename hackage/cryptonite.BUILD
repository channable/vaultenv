load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

cc_library(
  name = "cryptonite_headers",
  includes = ["cbits"],
  defines = ["SUPPORT_SSE"],
  hdrs = [
    "cbits/cryptonite_cpu.h",
    "cbits/cryptonite_bitfn.h",
    "cbits/cryptonite_align.h",
    "cbits/cryptonite_sha3.h",
    "cbits/cryptonite_sha512.h",
  ],
)

cc_library(
  name = "aes",
  deps = [":cryptonite_headers"],
  includes = ["cbits/aes"],
  # Note: could also define -WITH_PCLMUL if that instuction is available.
  copts = ["-mssse3", "-maes"],
  defines = ["WITH_AESNI", "SUPPORT_SSE"],
  srcs = [
    "cbits/aes/generic.c",
    "cbits/aes/gf.c",
    "cbits/aes/x86ni.c",
    "cbits/cryptonite_aes.c",
  ],
  hdrs = [
    "cbits/aes/block128.h",
    "cbits/aes/generic.h",
    "cbits/aes/gf.h",
    "cbits/aes/x86ni.h",
    "cbits/aes/x86ni_impl.c",
    "cbits/cryptonite_aes.h",
  ],
)

cc_library(
  name = "blake2",
  includes = ["cbits", "cbits/blake2/sse"],
  # Note: we would like to enalbe SSE using the define below, but unfortunately
  # there is an incorrect include path in the argon2 source which depends on
  # this library, and this bug gets exposed when we enable SSE. So disable for
  # now.
  # defines = ["SUPPORT_SSE"],
  srcs = [
    "cbits/blake2/sse/blake2s.c",
    "cbits/blake2/sse/blake2sp.c",
    "cbits/blake2/sse/blake2b.c",
    "cbits/blake2/sse/blake2bp.c",
    "cbits/cryptonite_blake2s.c",
    "cbits/cryptonite_blake2sp.c",
    "cbits/cryptonite_blake2b.c",
    "cbits/cryptonite_blake2bp.c",
  ],
  hdrs = [
    "cbits/blake2/sse/blake2-config.h",
    "cbits/blake2/sse/blake2-impl.h",
    "cbits/blake2/sse/blake2.h",
    "cbits/blake2/sse/blake2b-load-sse2.h",
    "cbits/blake2/sse/blake2b-load-sse41.h",
    "cbits/blake2/sse/blake2b-round.h",
    "cbits/blake2/sse/blake2s-load-sse2.h",
    "cbits/blake2/sse/blake2s-load-sse41.h",
    "cbits/blake2/sse/blake2s-load-xop.h",
    "cbits/blake2/sse/blake2s-round.h",
    "cbits/cryptonite_blake2s.h",
    "cbits/cryptonite_blake2sp.h",
    "cbits/cryptonite_blake2b.h",
    "cbits/cryptonite_blake2bp.h",
  ],
)

cc_library(
  name = "argon2",
  deps = [":blake2"],
  # Note: we would like to enalbe SSE using the define below, but unfortunately
  # there is an incorrect include path in the SSE version of the code. So do not
  # enable it for now.
  # defines = ["SUPPORT_SSE"],
  includes = ["cbits/argon2"],
  srcs = ["cbits/argon2/argon2.c"],
  hdrs = [
    "cbits/argon2/core.c",
    "cbits/argon2/core.h",
    "cbits/argon2/opt.c",
    "cbits/argon2/opt.h",
    "cbits/argon2/ref.c",
    "cbits/argon2/ref.h",
    "cbits/argon2/thread.c",
    "cbits/argon2/thread.h",
    "cbits/argon2/blamka-round-opt.h",
    "cbits/argon2/blamka-round-ref.h",
    "cbits/argon2/argon2.h",
  ],
)

cc_library(
  name = "curve25519",
  deps = [":cryptonite_headers"],
  defines = ["SUPPORT_SSE"],
  includes = [
    "cbits/curve25519",
    "cbits/ed25519",
  ],
  srcs = [
    "cbits/curve25519/curve25519-donna-c64.c",
    "cbits/ed25519/ed25519.c",
  ],
  hdrs = [
    "cbits/cryptonite_curve25519.h",
    "cbits/ed25519/curve25519-donna-64bit.h",
    "cbits/ed25519/curve25519-donna-helpers.h",
    "cbits/ed25519/ed25519-donna-64bit-tables.h",
    "cbits/ed25519/ed25519-donna-64bit-x86-32bit.h",
    "cbits/ed25519/ed25519-donna-64bit-x86.h",
    "cbits/ed25519/ed25519-donna-basepoint-table.h",
    "cbits/ed25519/ed25519-donna-batchverify.h",
    "cbits/ed25519/ed25519-donna-impl-base.h",
    "cbits/ed25519/ed25519-donna-portable-identify.h",
    "cbits/ed25519/ed25519-donna-portable.h",
    "cbits/ed25519/ed25519-donna.h",
    "cbits/ed25519/ed25519-hash.h",
    "cbits/ed25519/ed25519-randombytes.h",
    "cbits/ed25519/ed25519.h",
    "cbits/ed25519/modm-donna-64bit.h",
  ],
)

cc_library(
  name = "decaf",
  deps = [":cryptonite_headers"],
  defines = ["SUPPORT_SSE"],
  includes = [
    "cbits/decaf/include",
    "cbits/decaf/include/arch_ref64",
    "cbits/decaf/p448",
    "cbits/decaf/p448/arch_ref64",
  ],
  srcs = [
    "cbits/decaf/p448/arch_ref64/f_impl.c",
    "cbits/decaf/p448/f_generic.c",
    "cbits/decaf/p448/f_arithmetic.c",
    "cbits/decaf/utils.c",
    "cbits/decaf/ed448goldilocks/scalar.c",
    "cbits/decaf/ed448goldilocks/decaf_tables.c",
    "cbits/decaf/ed448goldilocks/decaf.c",
    "cbits/decaf/ed448goldilocks/eddsa.c",
  ],
  hdrs = [
    "cbits/decaf/include/arch_ref64/arch_intrinsics.h",
    "cbits/decaf/include/constant_time.h",
    "cbits/decaf/include/decaf.h",
    "cbits/decaf/include/decaf/common.h",
    "cbits/decaf/include/decaf/ed448.h",
    "cbits/decaf/include/decaf/point_255.h",
    "cbits/decaf/include/decaf/point_448.h",
    "cbits/decaf/include/decaf/sha512.h",
    "cbits/decaf/include/decaf/shake.h",
    "cbits/decaf/include/field.h",
    "cbits/decaf/include/portable_endian.h",
    "cbits/decaf/include/word.h",
    "cbits/decaf/p448/arch_ref64/f_impl.h",
    "cbits/decaf/p448/f_field.h",
  ],
)

cc_library(
  name = "cbits",
  defines = ["SUPPORT_SSE"],
  srcs = [
    "cbits/cryptonite_chacha.c",
    "cbits/cryptonite_salsa.c",
    "cbits/cryptonite_xsalsa.c",
    "cbits/cryptonite_rc4.c",
    "cbits/cryptonite_cpu.c",
    "cbits/cryptonite_rdrand.c",
    "cbits/p256/p256.c",
    "cbits/p256/p256_ec.c",
    "cbits/cryptonite_poly1305.c",
    "cbits/cryptonite_sha1.c",
    "cbits/cryptonite_sha256.c",
    "cbits/cryptonite_sha512.c",
    "cbits/cryptonite_sha3.c",
    "cbits/cryptonite_md2.c",
    "cbits/cryptonite_md4.c",
    "cbits/cryptonite_md5.c",
    "cbits/cryptonite_ripemd.c",
    "cbits/cryptonite_skein256.c",
    "cbits/cryptonite_skein512.c",
    "cbits/cryptonite_tiger.c",
    "cbits/cryptonite_whirlpool.c",
    "cbits/cryptonite_scrypt.c",
    "cbits/cryptonite_pbkdf2.c",
  ],
  # TODO(ruuda): Clean this up, the glob causes duplicate headers.
  hdrs = glob(["cbits/**/*.h"]),
  includes = [
    "cbits",
    "cbits/decaf/include/arch_ref64",
  ],
)

haskell_library(
  name = "internal",
  visibility = ["//visibility:public"],
  deps = [
    "@hackage_memory//:memory",
  ],
  prebuilt_dependencies = ["base"],
  srcs = [
    "Crypto/Internal/ByteArray.hs",
    "Crypto/Internal/Compat.hs",
    "Crypto/Internal/CompatPrim.hs",
    "Crypto/Internal/DeepSeq.hs",
    "Crypto/Internal/Imports.hs",
    "Crypto/Internal/Nat.hs",
    "Crypto/Internal/Proxy.hs",
    "Crypto/Internal/WordArray.hs",
    "Crypto/Internal/Words.hs",
  ],
)

haskell_library(
  name = "core",
  visibility = ["//visibility:public"],
  deps = [
    ":internal",
    "@hackage_memory//:memory",
  ],
  srcs = [
    "Crypto/Error.hs",
    "Crypto/Error/Types.hs",
    "Crypto/Cipher/Types.hs",
    "Crypto/Cipher/Types/AEAD.hs",
    "Crypto/Cipher/Types/Base.hs",
    "Crypto/Cipher/Types/Block.hs",
    "Crypto/Cipher/Types/GF.hs",
    "Crypto/Cipher/Types/Stream.hs",
    "Crypto/Cipher/Types/Utils.hs",
    "Crypto/Data/Padding.hs",
  ],
)

haskell_library(
  name = "cipher",
  visibility = ["//visibility:public"],
  deps = [
    ":aes",
    ":internal",
    ":core",
    "@hackage_memory//:memory",
  ],
  srcs = [
    "Crypto/Cipher/AES.hs",
    "Crypto/Cipher/Blowfish.hs",
    "Crypto/Cipher/Camellia.hs",
    "Crypto/Cipher/DES.hs",
    "Crypto/Cipher/RC4.hs",
    "Crypto/Cipher/Salsa.hs",
    "Crypto/Cipher/TripleDES.hs",
    "Crypto/Cipher/Twofish.hs",
    "Crypto/Cipher/Utils.hs",
    "Crypto/Cipher/XSalsa.hs",
    "Crypto/Cipher/AES/Primitive.hs",
    "Crypto/Cipher/Blowfish/Box.hs",
    "Crypto/Cipher/Blowfish/Primitive.hs",
    "Crypto/Cipher/Camellia/Primitive.hs",
    "Crypto/Cipher/DES/Primitive.hs",
    "Crypto/Cipher/Twofish/Primitive.hs",
  ],
)

haskell_library(
  name = "chacha",
  visibility = ["//visibility:public"],
  deps = [
    ":aes",
    ":cbits",
    ":internal",
    ":core",
    "@hackage_memory//:memory",
  ],
  srcs = [
    "Crypto/Cipher/ChaCha.hs",
    "Crypto/Cipher/ChaChaPoly1305.hs",
    "Crypto/MAC/Poly1305.hs",
  ],
)

haskell_library(
  name = "hash",
  visibility = ["//visibility:public"],
  deps = [
    ":blake2",
    ":cbits",
    ":internal",
    ":core",
    "@hackage_foundation//:core",
    "@hackage_foundation//:array",
  ],
  prebuilt_dependencies = ["base"],
  srcs = [
    "Crypto/ConstructHash/MiyaguchiPreneel.hs",
    "Crypto/Hash.hs",
    "Crypto/Hash/Algorithms.hs",
    "Crypto/Hash/Blake2.hs",
    "Crypto/Hash/Blake2b.hs",
    "Crypto/Hash/Blake2bp.hs",
    "Crypto/Hash/Blake2s.hs",
    "Crypto/Hash/Blake2sp.hs",
    "Crypto/Hash/IO.hs",
    "Crypto/Hash/Keccak.hs",
    "Crypto/Hash/MD2.hs",
    "Crypto/Hash/MD4.hs",
    "Crypto/Hash/MD5.hs",
    "Crypto/Hash/RIPEMD160.hs",
    "Crypto/Hash/SHA1.hs",
    "Crypto/Hash/SHA224.hs",
    "Crypto/Hash/SHA256.hs",
    "Crypto/Hash/SHA3.hs",
    "Crypto/Hash/SHA384.hs",
    "Crypto/Hash/SHA512.hs",
    "Crypto/Hash/SHA512t.hs",
    "Crypto/Hash/SHAKE.hs",
    "Crypto/Hash/Skein256.hs",
    "Crypto/Hash/Skein512.hs",
    "Crypto/Hash/Tiger.hs",
    "Crypto/Hash/Types.hs",
    "Crypto/Hash/Whirlpool.hs",
  ],
)

haskell_library(
  name = "number_random",
  visibility = ["//visibility:public"],
  deps = [
    ":cbits",
    ":internal",
    ":core",
    ":chacha",
    "@hackage_memory//:memory",
  ],
  prebuilt_dependencies = ["base"],
  srcs = [
    "Crypto/Random.hs",
    "Crypto/Random/ChaChaDRG.hs",
    "Crypto/Random/Entropy/Backend.hs",
    "Crypto/Random/Entropy/RDRand.hs",
    "Crypto/Random/Entropy/Source.hs",
    "Crypto/Random/Entropy/Unix.hs",
    "Crypto/Random/Entropy/Unsafe.hs",
    "Crypto/Random/EntropyPool.hs",
    "Crypto/Random/Probabilistic.hs",
    "Crypto/Random/SystemDRG.hs",
    "Crypto/Number/Basic.hs",
    "Crypto/Number/Compat.hs",
    "Crypto/Number/F2m.hs",
    "Crypto/Number/Generate.hs",
    "Crypto/Number/ModArithmetic.hs",
    "Crypto/Number/Prime.hs",
    "Crypto/Number/Serialize.hs",
    "Crypto/Number/Serialize/Internal.hs",
    "Crypto/Random/Types.hs",
    "Crypto/Random/Entropy.hs",
  ],
)

haskell_library(
  name = "pubkey",
  visibility = ["//visibility:public"],
  deps = [
    ":cbits",
    ":core",
    ":curve25519",
    ":decaf",
    ":hash",
    ":internal",
    ":number_random",
    "@hackage_memory//:memory",
  ],
  srcs = [
    "Crypto/ECC.hs",
    "Crypto/ECC/Simple/Prim.hs",
    "Crypto/ECC/Simple/Types.hs",
    "Crypto/PubKey/Curve25519.hs",
    "Crypto/PubKey/Curve448.hs",
    "Crypto/PubKey/DH.hs",
    "Crypto/PubKey/DSA.hs",
    "Crypto/PubKey/ECC/DH.hs",
    "Crypto/PubKey/ECC/ECDSA.hs",
    "Crypto/PubKey/ECC/Generate.hs",
    "Crypto/PubKey/ECC/P256.hs",
    "Crypto/PubKey/ECC/Prim.hs",
    "Crypto/PubKey/ECC/Types.hs",
    "Crypto/PubKey/ECIES.hs",
    "Crypto/PubKey/Ed25519.hs",
    "Crypto/PubKey/Ed448.hs",
    "Crypto/PubKey/ElGamal.hs",
    "Crypto/PubKey/Internal.hs",
    "Crypto/PubKey/MaskGenFunction.hs",
    "Crypto/PubKey/RSA.hs",
    "Crypto/PubKey/RSA/OAEP.hs",
    "Crypto/PubKey/RSA/PKCS15.hs",
    "Crypto/PubKey/RSA/PSS.hs",
    "Crypto/PubKey/RSA/Prim.hs",
    "Crypto/PubKey/RSA/Types.hs",
  ],
)

haskell_library(
  name = "mac",
  visibility = ["//visibility:public"],
  deps = [
    ":cbits",
    ":internal",
    ":core",
    ":hash",
    "@hackage_memory//:memory",
  ],
  srcs = [
    "Crypto/MAC/CMAC.hs",
    "Crypto/MAC/HMAC.hs",
  ],
)

haskell_library(
  name = "kdf",
  visibility = ["//visibility:public"],
  deps = [
    ":argon2",
    ":cbits",
    ":cipher",
    ":core",
    ":hash",
    ":internal",
    ":mac",
    ":number_random",
    "@hackage_memory//:memory",
  ],
  prebuilt_dependencies = ["base"],
  srcs = [
    "Crypto/KDF/Argon2.hs",
    "Crypto/KDF/BCrypt.hs",
    "Crypto/KDF/HKDF.hs",
    "Crypto/KDF/PBKDF2.hs",
    "Crypto/KDF/Scrypt.hs",
  ],
)

haskell_library(
  name = "misc",
  visibility = ["//visibility:public"],
  deps = [
    ":hash",
    ":internal",
    ":mac",
    ":number_random",
    "@hackage_memory//:memory",
  ],
  srcs = [
    "Crypto/Data/AFIS.hs",
    "Crypto/OTP.hs",
    "Crypto/Tutorial.hs",
  ],
)
