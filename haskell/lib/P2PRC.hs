{- |
   Module      : Text.Pandoc
   Copyright   : Copyright (C) 2006-2024 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

This helper module exports the main writers, readers, and data
structure definitions from the Pandoc libraries.

A typical application will chain together a reader and a writer
to convert strings from one format to another.  For example, the
following simple program will act as a filter converting markdown
fragments to reStructuredText, using reference-style links instead of
inline links:

> module Main where
> import Text.Pandoc
> import Data.Text (Text)
> import qualified Data.Text.IO as T
>
> mdToRST :: Text -> IO Text
> mdToRST txt = runIOorExplode $
>   readMarkdown def txt
>   >>= writeRST def{ writerReferenceLinks = True }
>
> main :: IO ()
> main = do
>   T.getContents >>= mdToRST >>= T.putStrLn

-}

module P2PRC
  ( runP2PRC
  , MapPortRequest(..)
  )
  where


import Engine
  ( runP2PRC
  )


import API
  ( MapPortRequest(MkMapPortRequest)
  )
