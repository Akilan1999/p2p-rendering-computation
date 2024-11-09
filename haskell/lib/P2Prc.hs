{-# LANGUAGE OverloadedStrings #-}

module P2Prc ( runP2Prc ) where

import System.Process
  ( ProcessHandle
  , terminateProcess
  )

import Control.Concurrent ( threadDelay )

import Data.Aeson ( FromJSON )


import Environment  ( cleanEnvironment )

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


-- URGENT TASKS
--
--
-- TODO: splitting code on different files and directories
--
-- TODO: add Haddock documentation
--
-- TODO: P2PRC setup
  -- p2prc runtime packaging
    -- Perhaps create internal script to run P2PRC from nix flake
      -- "nix flake run ..."
      -- simplify packaging
  -- check version P2PRC: only run if version if above a certain value
  -- setup p2prc command
    -- check if p2prc command is available in environment first
    -- otherwise check folder above
--
-- TODO: publish haskell library
--
-- TODO: add use case examples (extra-source_files)


runP2Prc :: IO ()
runP2Prc = do


  -- important but not urgent yet
  --
  -- TODO: Change Standard Library
  -- TODO: add GDTA syntax to data types
  -- TODO: need monad transformers to refactor the code
  --
  -- TODO: parse IO arguments;
    -- TODO: create DSL from the standard input
  --
  -- TODO: setup nix flake package
  -- TODO: add quickcheck testing (quickchecking-dynamic)
  --
  -- Extra:
  --
  -- TODO: Error
    -- assign error: should parse other error



  eitherP2prcAPI <- getP2prcAPI

  case eitherP2prcAPI of
    (Right p2prcAPI) -> do
      let
        ( MkP2prAPI
          { startServer     = startServer
          , execInitConfig  = execInitConfig
          , execListServers = execListServers
          , execMapPort     = execMapPort
          }
          ) = p2prcAPI


      configValue <- execInitConfig

      -- TODO: get name of host server from config json

      print configValue
      putStrLn "\n\n\n"

      eitherStartProcessHandle <- startServer


      case eitherStartProcessHandle of
        (Right startProcessHandle) -> do

          let sleepNSecs i = threadDelay (i * 1000000)

          sleepNSecs 5

          outputStr <- execListServers
          print outputStr

          mapPortOut <- execMapPort $ MkMapPortRequest 3333 "domain"


          case mapPortOut of
            (Right v) -> print v
            (Left e)  -> print e


          -- TODO: work on looping function
          --
          -- Loop (Run replica of haskell program on different $NODES)
          --    - Start server
          --    - wait 4 seconds
          --    - Identify new node running p2prc with SSH external port exposed
          --    - SSH into machine
          --    - Use simple File transfer to setup files
          --    - Run server
          --    - Use remote machine p2prc cmd to map a port using --mp
          --    - Return back the exposed public IP and port number back to stdout


          terminateProcess startProcessHandle

        (Left err) -> print err

    (Left err) -> print err


-- Module: API

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




