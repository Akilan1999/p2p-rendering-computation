<div id="package-header">

<span class="caption">p2prc-0.1.0.0: P2PRC haskell library</span>

- [Contents](index.html)
- [Index](doc-index.html)

</div>

<div id="content">

<div id="module-header">

|              |                                         |
|--------------|-----------------------------------------|
| Copyright    | Copyright (C) 2006-2024 John MacFarlane |
| License      | GNU GPL, version 2 or above             |
| Maintainer   | John MacFarlane \<jgm@berkeley.edu\>    |
| Stability    | alpha                                   |
| Portability  | portable                                |
| Safe Haskell | Safe-Inferred                           |
| Language     | GHC2021                                 |

P2PRC

</div>

<div id="description">

Description

<div class="doc">

This helper module exports the main writers, readers, and data structure
definitions from the Pandoc libraries.

A typical application will chain together a reader and a writer to
convert strings from one format to another. For example, the following
simple program will act as a filter converting markdown fragments to
reStructuredText, using reference-style links instead of inline links:

    module Main where
    import Text.Pandoc
    import Data.Text (Text)
    import qualified Data.Text.IO as T

    mdToRST :: Text -> IO Text
    mdToRST txt = runIOorExplode $
      readMarkdown def txt
      >>= writeRST def{ writerReferenceLinks = True }

    main :: IO ()
    main = do
      T.getContents >>= mdToRST >>= T.putStrLn

</div>

</div>

<div id="synopsis">

Synopsis

- [runP2PRC](#v:runP2PRC) ::
  [MapPortRequest](P2PRC.html#t:MapPortRequest "P2PRC") -\>
  [IO]($%7Bpkgroot%7D/../../../../dcnyq1a8qi8x59n5p53d0dx42cl8hf8x-ghc-9.6.5-doc/share/doc/ghc/html/libraries/base-4.18.2.1/System-IO.html#t:IO "System.IO")
  ()
- <span class="keyword">data</span> [MapPortRequest](#t:MapPortRequest)
  = [MkMapPortRequest](#v:MkMapPortRequest)
  [Int]($%7Bpkgroot%7D/../../../../dcnyq1a8qi8x59n5p53d0dx42cl8hf8x-ghc-9.6.5-doc/share/doc/ghc/html/libraries/base-4.18.2.1/Data-Int.html#t:Int "Data.Int")
  [String]($%7Bpkgroot%7D/../../../../dcnyq1a8qi8x59n5p53d0dx42cl8hf8x-ghc-9.6.5-doc/share/doc/ghc/html/libraries/base-4.18.2.1/Data-String.html#t:String "Data.String")

</div>

<div id="interface">

# Documentation

<div class="top">

<span id="v:runP2PRC" class="def">runP2PRC</span> ::
[MapPortRequest](P2PRC.html#t:MapPortRequest "P2PRC") -\>
[IO]($%7Bpkgroot%7D/../../../../dcnyq1a8qi8x59n5p53d0dx42cl8hf8x-ghc-9.6.5-doc/share/doc/ghc/html/libraries/base-4.18.2.1/System-IO.html#t:IO "System.IO")
() <a href="#v:runP2PRC" class="selflink">#</a>

<div class="doc">

Hello World

</div>

</div>

<div class="top">

<span class="keyword">data</span> <span id="t:MapPortRequest"
class="def">MapPortRequest</span>
<a href="#t:MapPortRequest" class="selflink">#</a>

<div class="subs constructors">

Constructors

|                                                                                                                                                                                                                                                                                                                                                                                                     |     |
|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-----|
| <span id="v:MkMapPortRequest" class="def">MkMapPortRequest</span> [Int]($%7Bpkgroot%7D/../../../../dcnyq1a8qi8x59n5p53d0dx42cl8hf8x-ghc-9.6.5-doc/share/doc/ghc/html/libraries/base-4.18.2.1/Data-Int.html#t:Int "Data.Int") [String]($%7Bpkgroot%7D/../../../../dcnyq1a8qi8x59n5p53d0dx42cl8hf8x-ghc-9.6.5-doc/share/doc/ghc/html/libraries/base-4.18.2.1/Data-String.html#t:String "Data.String") |     |

</div>

</div>

</div>

</div>

<div id="footer">

Produced by [Haddock](http://www.haskell.org/haddock/) version 2.29.2

</div>
