module Main where

import P2PRC
  ( runP2PRC
  , MapPortRequest(MkMapPortRequest)
  , P2PRCommands(MapPort)
  )

main :: IO ()
main =let
  in 
  runP2PRC
    [
      ( MapPort $ MkMapPortRequest 8080 "jose.akilan.io")
    ]
    
