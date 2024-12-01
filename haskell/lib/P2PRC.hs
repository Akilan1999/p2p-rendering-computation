{-# OPTIONS_HADDOCK show-extensions #-}


{- |
   Module      : P2PRC
   Copyright   : Copyright (C) 2024-2024 Jose Fernandes
   License     : GNU GPL, version 2 or above
   Maintainer  : Jose Fernandes <jf94.uk@gmail.com>
   Stability   : beta
   Portability : portable

This library provides an interface to the P2Prc runtime.

This Module intends to export the main functions and data type definitions necessary to get started with the P2PRC api.

A minimal application will require the import of 'runP2PRC' function that accepts a 'MapPortRequest' value that exposes a specific port number and associates it with a domain name in the internet.

This is a small template to get quickly get started with this interface. We assume the user has already an application listening on the tcp socket "8080".

> module Main where
>
> import P2PRC
>   ( runP2PRC
>   , MapPortRequest(MkMapPortRequest)
>   )
>
> main :: IO ()
> main =
>   runP2PRC
>     ( MkMapPortRequest 8080 "jose.akilan.io"
>     )

-}


module P2PRC
  (
    -- * Functions
    {- | These are the available functions available to interact with the P2Prc environment, at a lower level of abstraction.
     It is intended this way to give freedom to the developer to implement their own orchestration strategies.
    -}
    runP2PRC
  , getP2prcAPI

    -- * Data Types
    {- | This section describes and explains the library's type system, more specifically, the interfaces and primitive types.
    -}

    -- ** Interface data types
    -- | This section gives an overview on the runtime and host machine interfaces.
  , P2PRCapi
  , P2prcConfig

    -- ** Primitive data types
    -- | These types represent the core data that is communicated between requests and the runtime.
  , IPAdressTable
  , MapPortRequest(..)
  , MapPortResponse
  , Error(..)

    -- ** Type Synonyms
    -- | This section is reserved to some useful type synonyms that add significant ergonomics.
  , IOEitherError
  )
  where


import Engine
  ( runP2PRC
  )

import JSON
  ( IPAdressTable
  , MapPortResponse(..)
  , P2prcConfig
  )

import API
  ( MapPortRequest(MkMapPortRequest)
  , P2PRCapi
  , getP2prcAPI
  )


import Error
  ( Error(..)
  , IOEitherError
  )


