{-# OPTIONS_HADDOCK show-extensions #-}


{- |
   Module      : P2PRC
   Copyright   : Copyright (C) 2024-2024 Jose Fernandes
   License     : GNU GPL, version 2 or above

   Maintainer  : Jose Fernandes <jf94.uk@gmail.com>
   Stability   : beta
   Portability : portable

This helper module exports the main functions and data type definitions necessary to get started with the P2PRC api.

A minimal application will require the import of "runP2PRC" function that accepts a "MapPortRequest" value that exposes a specific port number and associates it with a domain name in the internet.

This is a small template to get quickly get started with this interface. We assume the user has already an application listening on the tcp socket "8080".

> module Main where
>
> import P2PRC
>   ( runP2PRC
>   , MapPortRequest(MkMapPortRequest)
>   )
>
>
>
> main :: IO ()
> main =
>   runP2PRC
>     ( MkMapPortRequest 8080 "jose.akilan.io"
>     )

-}


module P2PRC
  ( MapPortRequest(..)
  , P2PRCapi
  , IPAdressTable
  , MapPortResponse
  , P2prcConfig
  , IOEitherError
  , getP2prcAPI
  , runP2PRC
  )
  where


import Engine
  ( runP2PRC
  )

import JSON
  ( IPAdressTable
  , MapPortResponse
  , P2prcConfig
  )

import API
  ( MapPortRequest(MkMapPortRequest)
  , P2PRCapi
  , getP2prcAPI
  )


import Error (IOEitherError)
