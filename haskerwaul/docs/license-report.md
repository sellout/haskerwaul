**NB**: This captures the licenses associated with a particular set of dependency versions. If your own build solves differently, it’s possible that the licenses may have changed, or even that the set of dependencies itself is different. Please make sure you run [`cabal-plan license-report`](https://hackage.haskell.org/package/cabal-plan) on your own components rather than assuming this is authoritative.

# Dependency License Report

Bold-faced **`package-name`**s denote standard libraries bundled with `ghc-9.10.1`.

## Direct dependencies of `haskerwaul:lib:haskerwaul`

| Name | Version | [SPDX](https://spdx.org/licenses/) License Id | Description | Also depended upon by |
| --- | --- | --- | --- | --- |
| **`base`** | [`4.20.0.0`](http://hackage.haskell.org/package/base-4.20.0.0) | [`BSD-3-Clause`](http://hackage.haskell.org/package/base-4.20.0.0/src/LICENSE) | Core data structures and operations | *(core library)* |
| `constraints` | [`0.14.2`](http://hackage.haskell.org/package/constraints-0.14.2) | [`BSD-2-Clause`](http://hackage.haskell.org/package/constraints-0.14.2/src/LICENSE) | Constraint manipulation |  |
| **`containers`** | [`0.7`](http://hackage.haskell.org/package/containers-0.7) | [`BSD-3-Clause`](http://hackage.haskell.org/package/containers-0.7/src/LICENSE) | Assorted concrete container types | `binary`, `hashable` |

## Indirect transitive dependencies

| Name | Version | [SPDX](https://spdx.org/licenses/) License Id | Description | Depended upon by |
| --- | --- | --- | --- | --- |
| **`array`** | [`0.5.7.0`](http://hackage.haskell.org/package/array-0.5.7.0) | [`BSD-3-Clause`](http://hackage.haskell.org/package/array-0.5.7.0/src/LICENSE) | Mutable and immutable arrays | `binary`, `containers`, `deepseq`, `stm`, `text` |
| **`binary`** | [`0.8.9.2`](http://hackage.haskell.org/package/binary-0.8.9.2) | [`BSD-3-Clause`](http://hackage.haskell.org/package/binary-0.8.9.2/src/LICENSE) | Binary serialisation for Haskell values using lazy ByteStrings | `constraints`, `text` |
| `boring` | [`0.2.2`](http://hackage.haskell.org/package/boring-0.2.2) | [`BSD-3-Clause`](http://hackage.haskell.org/package/boring-0.2.2/src/LICENSE) | Boring and Absurd types | `constraints` |
| **`bytestring`** | [`0.12.1.0`](http://hackage.haskell.org/package/bytestring-0.12.1.0) | [`BSD-3-Clause`](http://hackage.haskell.org/package/bytestring-0.12.1.0/src/LICENSE) | Fast, compact, strict and lazy byte strings with a list interface | `binary`, `filepath`, `hashable`, `os-string`, `text` |
| **`deepseq`** | [`1.5.0.0`](http://hackage.haskell.org/package/deepseq-1.5.0.0) | [`BSD-3-Clause`](http://hackage.haskell.org/package/deepseq-1.5.0.0/src/LICENSE) | Deep evaluation of data structures | `bytestring`, `constraints`, `containers`, `filepath`, `hashable`, `os-string`, `pretty`, `tagged`, `text` |
| **`exceptions`** | [`0.10.7`](http://hackage.haskell.org/package/exceptions-0.10.7) | [`BSD-3-Clause`](http://hackage.haskell.org/package/exceptions-0.10.7/src/LICENSE) | Extensible optionally-pure exceptions | `filepath`, `os-string` |
| **`filepath`** | [`1.5.2.0`](http://hackage.haskell.org/package/filepath-1.5.2.0) | [`BSD-3-Clause`](http://hackage.haskell.org/package/filepath-1.5.2.0/src/LICENSE) | Library for manipulating FilePaths in a cross platform way. | `hashable` |
| **`ghc-bignum`** | [`1.3`](http://hackage.haskell.org/package/ghc-bignum-1.3) | [`BSD-3-Clause`](http://hackage.haskell.org/package/ghc-bignum-1.3/src/LICENSE) | GHC BigNum library | `ghc-internal`, `hashable` |
| **`ghc-boot-th`** | [`9.10.1`](http://hackage.haskell.org/package/ghc-boot-th-9.10.1) | [`BSD-3-Clause`](http://hackage.haskell.org/package/ghc-boot-th-9.10.1/src/LICENSE) | Shared functionality between GHC and the @template-haskell@ library | `template-haskell` |
| **`ghc-internal`** | [`9.1001.0`](http://hackage.haskell.org/package/ghc-internal-9.1001.0) | [`BSD-3-Clause`](http://hackage.haskell.org/package/ghc-internal-9.1001.0/src/LICENSE) | Basic libraries | `base` |
| **`ghc-prim`** | [`0.11.0`](http://hackage.haskell.org/package/ghc-prim-0.11.0) | [`BSD-3-Clause`](http://hackage.haskell.org/package/ghc-prim-0.11.0/src/LICENSE) | GHC primitives | *(core library)* |
| `hashable` | [`1.5.0.0`](http://hackage.haskell.org/package/hashable-1.5.0.0) | [`BSD-3-Clause`](http://hackage.haskell.org/package/hashable-1.5.0.0/src/LICENSE) | A class for types that can be converted to a hash value | `constraints` |
| **`mtl`** | [`2.3.1`](http://hackage.haskell.org/package/mtl-2.3.1) | [`BSD-3-Clause`](http://hackage.haskell.org/package/mtl-2.3.1/src/LICENSE) | Monad classes for transformers, using functional dependencies | `constraints`, `exceptions` |
| **`os-string`** | [`2.0.2`](http://hackage.haskell.org/package/os-string-2.0.2) | [`BSD-3-Clause`](http://hackage.haskell.org/package/os-string-2.0.2/src/LICENSE) | Library for manipulating Operating system strings. | `filepath`, `hashable` |
| **`pretty`** | [`1.1.3.6`](http://hackage.haskell.org/package/pretty-1.1.3.6) | [`BSD-3-Clause`](http://hackage.haskell.org/package/pretty-1.1.3.6/src/LICENSE) | Pretty-printing library | `template-haskell` |
| **`stm`** | [`2.5.3.1`](http://hackage.haskell.org/package/stm-2.5.3.1) | [`BSD-3-Clause`](http://hackage.haskell.org/package/stm-2.5.3.1/src/LICENSE) | Software Transactional Memory | `exceptions` |
| `tagged` | [`0.8.9`](http://hackage.haskell.org/package/tagged-0.8.9) | [`BSD-3-Clause`](http://hackage.haskell.org/package/tagged-0.8.9/src/LICENSE) | Haskell 98 phantom types to avoid unsafely passing dummy arguments | `boring` |
| **`template-haskell`** | [`2.22.0.0`](http://hackage.haskell.org/package/template-haskell-2.22.0.0) | [`BSD-3-Clause`](http://hackage.haskell.org/package/template-haskell-2.22.0.0/src/LICENSE) | Support library for Template Haskell | `bytestring`, `containers`, `exceptions`, `filepath`, `os-string`, `tagged`, `text` |
| **`text`** | [`2.1.1`](http://hackage.haskell.org/package/text-2.1.1) | [`BSD-2-Clause`](http://hackage.haskell.org/package/text-2.1.1/src/LICENSE) | An efficient packed Unicode text type. | `hashable` |
| **`transformers`** | [`0.6.1.1`](http://hackage.haskell.org/package/transformers-0.6.1.1) | [`BSD-3-Clause`](http://hackage.haskell.org/package/transformers-0.6.1.1/src/LICENSE) | Concrete functor and monad transformers | `boring`, `constraints`, `exceptions`, `mtl`, `tagged` |

