
module API
  ( P2PRCapi(..)
  , MapPortRequest(..)
  , p2prcAPI
  )
  where


import System.Directory ( getCurrentDirectory )

import System.Process ( ProcessHandle )

import Data.Aeson ( FromJSON )

import Error
  ( IOEitherError
  )

import JSON
  ( IPAddressTable(..)
  , MapPortResponse(..)
  , P2prcConfig
  )

import CLI
  ( StdInput(..)
  , CLIOpt(..)
  , eitherErrDecode
  , p2PrcCmdName
  , eitherExecProcess
  , eitherExecProcessParser
  , spawnProcP2Prc
  )



-- | Lower level P2PRC Haskell api that exposes basic functionality necessary to joint the network.

data P2PRCapi
  = MkP2PRCapi
    { startServer       :: IOEitherError ProcessHandle
      -- ^ Start server
    , execInitConfig    :: IOEitherError P2prcConfig
      -- ^ Instantiate server configuration
    , execListServers   :: IOEitherError IPAddressTable
      -- ^ List servers in network
    , execMapPort       :: MapPortRequest -> IOEitherError MapPortResponse
      -- ^ Exposes and associates a local TCP port with a remote DNS address
    }


-- | This defines the request required to create an association between a TCP socket port and a DNS server in the network. If successful, it makes a resource available in the network.
data MapPortRequest =
  MkMapPortRequest    -- ^ P2PRC's port allocation request value
    Int                 -- ^ TCP socket number
    String              -- ^ Network domain name


{-|
  This function intiates a pure P2PRC runtime state and builds up a 'P2PRCapi' API instance.

  ==== __Example__

  The following example show how this function can be used to expose the runtime functionalities:

  @
  example :: IOEitherError P2PRCapi
  example = do

    eitherP2prcAPI <- getP2prcAPI

    case eitherP2prcAPI of
      ( Right
        ( MkP2PRCapi
          { startServer     = startServer
          , execInitConfig  = execInitConfig
          , execListServers = execListServers
          , execMapPort     = execMapPort
          }
        )) -> do

        -- Your code logic

      errValue -> errValue
  @
-}

{-# WARNING p2prcAPI "This function is currently unstable because the configuration reading is dependent on the following issue: https://github.com/Akilan1999/p2p-rendering-computation/issues/120" #-}
p2prcAPI :: P2PRCapi
p2prcAPI =
  MkP2PRCapi
    { startServer = spawnProcP2Prc p2PrcCmdName [ MkOptAtomic "--s" ]

    , execListServers =
      execProcP2PrcParser [ MkOptAtomic "--ls" ] MkEmptyStdInput

    , execMapPort =
      \ (MkMapPortRequest portNumber domainName) ->
        execProcP2PrcParser
          [ MkOptTuple
            ( "--mp"
            , show portNumber
            )
            , MkOptTuple
            ( "--dn"
            , domainName
            )
          ]
          MkEmptyStdInput

    , execInitConfig = do

      confInitRes <- execProcP2Prc [ MkOptAtomic "--dc" ] MkEmptyStdInput

      case confInitRes of
        (Right _) -> do

          -- TODO: get config file name dynamically
          --
          currDirectory <- getCurrentDirectory

          -- TODO: change values before loading file
          let fname = currDirectory ++ "/config.json" :: FilePath


          -- TODO: read config check if file exists
          configContent <- readFile fname

          pure $ eitherErrDecode configContent

        (Left err) -> pure $ Left err

    }


  where

  execProcP2PrcParser ::
    FromJSON a =>
      [CLIOpt] -> StdInput -> IOEitherError a
  execProcP2PrcParser = eitherExecProcessParser p2PrcCmdName
  -- TODO: GHC question, why does it scope down instead staying generic

  execProcP2Prc = eitherExecProcess p2PrcCmdName

