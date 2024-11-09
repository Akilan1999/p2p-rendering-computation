
module API
  ( P2prAPI(..)
  , MapPortRequest(..)
  , getP2prcAPI
  )
  where

import System.Process ( ProcessHandle )

import Data.Aeson ( FromJSON )

import Error (IOEitherError)

import JSON
  ( IPAdressTable
  , MapPortResponse
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



data P2prAPI = MkP2prAPI
  { startServer       :: IOEitherError ProcessHandle
  , execInitConfig    :: IOEitherError P2prcConfig
  , execListServers   :: IOEitherError IPAdressTable
  , execMapPort       :: MapPortRequest -> IOEitherError MapPortResponse
  }


data MapPortRequest
  = MkMapPortRequest Int String


getP2prcAPI :: IOEitherError P2prAPI
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

      Right $ MkP2prAPI
        { startServer = spawnProcP2Prc p2prcCmd [ MkOptAtomic "--s" ]

        , execListServers =
          execProcP2PrcParser [ MkOptAtomic "--ls" ] MkEmptyStdInput

        , execMapPort =
          \ (MkMapPortRequest portNumber _) ->
            execProcP2PrcParser
              [ MkOptTuple
                ( "--mp"
                , show portNumber
                )
              -- , MkOptTuple -- TODO: add domain parameter
              --   ( "--dm"
              --   , domainName
              --   )
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


