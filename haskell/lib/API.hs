
module API
  ( P2PRCapi(..)
  , MapPortRequest(..)
  , getP2prcAPI
  )
  where

import System.Process ( ProcessHandle )

import Data.Aeson ( FromJSON )

import Error
  ( IOEitherError
  )

import JSON
  ( IPAdressTable(..)
  , MapPortResponse(..)
  , P2prcConfig
  )

import CLI
  ( StdInput(..)
  , CLIOpt(..)
  , eitherErrDecode
  , getP2PrcCmd
  , eitherExecProcess
  , eitherExecProcessParser
  , spawnProcP2Prc
  )

import Environment  ( cleanEnvironment )


data P2PRCapi   -- ^ Haskell API
  = MkP2PRCapi  -- ^ Main Constructor
    { startServer       :: IOEitherError ProcessHandle  -- ^ start server
    , execInitConfig    :: IOEitherError P2prcConfig
    , execListServers   :: IOEitherError IPAdressTable
    , execMapPort       :: MapPortRequest -> IOEitherError MapPortResponse
    }


-- | This defines the request required to create an association between a TCP socket port and a DNS server in the network. If successful, it makes a resource available in the network.
data MapPortRequest =
  MkMapPortRequest    -- ^ P2PRC's port allocation request value
    Int                 -- ^ TCP socket number
    String              -- ^ Network domain name


{-|
  This function cleans the previous running state (ensuring a pure P2PRC runtime state) and builds up a conditional 'P2PRCapi' instance.

  __The following example show how this function can be used to expose the runtime functionalities:__

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
{-# WARNING getP2prcAPI "This function is currently unstable since it assumes that the Haskell program is executed from the P2PRC \"haskell\" subfolder and the \"p2prc\" executable in the the root folder." #-}
getP2prcAPI :: IOEitherError P2PRCapi
getP2prcAPI = do

  cleanEnvironment

  eitherP2prcCmd <- getP2PrcCmd

  pure $ case eitherP2prcCmd of
    (Right p2prcCmd) -> let

      execProcP2PrcParser ::
        FromJSON a =>
          [CLIOpt] -> StdInput -> IOEitherError a
      execProcP2PrcParser = eitherExecProcessParser p2prcCmd
      -- TODO: GHC question, why does it scope down instead staying generic

      execProcP2Prc = eitherExecProcess p2prcCmd

      in do

      Right $ MkP2PRCapi
        { startServer = spawnProcP2Prc p2prcCmd [ MkOptAtomic "--s" ]

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

              -- TODO: change values before loading file
              let fname = "/home/xecarlox/Desktop/p2p-rendering-computation/haskell/config.json" :: FilePath


              -- TODO: read config check if file exists
              configContent <- readFile fname

              pure $ eitherErrDecode configContent

            (Left err) -> pure $ Left err

        }

    (Left err) -> Left err


