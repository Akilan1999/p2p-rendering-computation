::: {#package-header}
[p2prc-0.1.0.0: P2PRC haskell library]{.caption}

-   [Contents](index.html)
-   [Index](doc-index.html)
:::

::: {#content}
::: {#module-header}
  -------------- -----------------------------------------
  Copyright      Copyright (C) 2006-2024 John MacFarlane
  License        GNU GPL, version 2 or above
  Maintainer     John MacFarlane \<jgm@berkeley.edu\>
  Stability      alpha
  Portability    portable
  Safe Haskell   Safe-Inferred
  Language       GHC2021
  -------------- -----------------------------------------

P2PRC
:::

::: {#description}
Description

::: doc
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
:::
:::

::: {#synopsis}
Synopsis

-   [runP2PRC](#v:runP2PRC) ::
    [MapPortRequest](P2PRC.html#t:MapPortRequest "P2PRC") -\>
    [IO]($%7Bpkgroot%7D/../../../../dcnyq1a8qi8x59n5p53d0dx42cl8hf8x-ghc-9.6.5-doc/share/doc/ghc/html/libraries/base-4.18.2.1/System-IO.html#t:IO "System.IO")
    ()
-   [data]{.keyword} [MapPortRequest](#t:MapPortRequest) =
    [MkMapPortRequest](#v:MkMapPortRequest)
    [Int]($%7Bpkgroot%7D/../../../../dcnyq1a8qi8x59n5p53d0dx42cl8hf8x-ghc-9.6.5-doc/share/doc/ghc/html/libraries/base-4.18.2.1/Data-Int.html#t:Int "Data.Int")
    [String]($%7Bpkgroot%7D/../../../../dcnyq1a8qi8x59n5p53d0dx42cl8hf8x-ghc-9.6.5-doc/share/doc/ghc/html/libraries/base-4.18.2.1/Data-String.html#t:String "Data.String")
:::

::: {#interface}
# Documentation

::: top
[runP2PRC]{#v:runP2PRC .def} ::
[MapPortRequest](P2PRC.html#t:MapPortRequest "P2PRC") -\>
[IO]($%7Bpkgroot%7D/../../../../dcnyq1a8qi8x59n5p53d0dx42cl8hf8x-ghc-9.6.5-doc/share/doc/ghc/html/libraries/base-4.18.2.1/System-IO.html#t:IO "System.IO")
() [\#](#v:runP2PRC){.selflink}

::: doc
Hello World
:::
:::

::: top
[data]{.keyword} [MapPortRequest]{#t:MapPortRequest .def}
[\#](#t:MapPortRequest){.selflink}

::: {.subs .constructors}
Constructors

  -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- ---
  [MkMapPortRequest]{#v:MkMapPortRequest .def} [Int]($%7Bpkgroot%7D/../../../../dcnyq1a8qi8x59n5p53d0dx42cl8hf8x-ghc-9.6.5-doc/share/doc/ghc/html/libraries/base-4.18.2.1/Data-Int.html#t:Int "Data.Int") [String]($%7Bpkgroot%7D/../../../../dcnyq1a8qi8x59n5p53d0dx42cl8hf8x-ghc-9.6.5-doc/share/doc/ghc/html/libraries/base-4.18.2.1/Data-String.html#t:String "Data.String")    
  -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- ---
:::
:::
:::
:::

::: {#footer}
Produced by [Haddock](http://www.haskell.org/haddock/) version 2.29.2
:::
