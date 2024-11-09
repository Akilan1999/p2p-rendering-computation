{-# LANGUAGE OverloadedStrings #-}

module P2Prc ( runP2Prc ) where


import System.Process ( terminateProcess )
import Control.Concurrent ( threadDelay )

import API
  ( P2prAPI(..)
  , MapPortRequest(..)
  , getP2prcAPI
  )


-- URGENT TASKS
--
--
-- TODO: add Haddock documentation
--
-- TODO: P2PRC setup
  -- check version P2PRC: only run if version if above a certain value
  -- setup p2prc command
    -- check if p2prc command is available in environment first
    -- otherwise check folder above
--
-- TODO: Fix API TODOS
--
-- TODO: Fix JSON TODOS
--
-- create DSL to start and orchestrate network
--
-- TODO: publish haskell library
--


runP2Prc :: IO ()
runP2Prc = do


  --
  -- TODO: Change Standard Library
  -- TODO: add GDTA syntax to data types
  -- TODO: need monad transformers to refactor the code
  --
  -- TODO: add quickcheck testing (quickchecking-dynamic)
  --
  -- TODO: parse IO arguments;
    -- TODO: create DSL from the standard input
  --
  -- TODO: add use case examples (extra-source_files)
  --
  -- TODO: setup nix flake package
    -- Nix p2prc runtime packaging
      -- Perhaps create internal script to run P2PRC from nix flake
        -- "nix flake run ..."
        -- simplify packaging
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


