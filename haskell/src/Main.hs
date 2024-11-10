
module Main where

import P2PRC
  ( runP2PRC
  )


main :: IO ()
main =
  runP2PRC 8080 "jose.akilan.io"
