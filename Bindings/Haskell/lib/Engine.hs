{-# LANGUAGE OverloadedStrings #-}

module Engine
  ( runP2PRC
  )
  where


import Control.Concurrent
import Data.Char (toLower)
import System.IO
import Control.Monad (when)


import System.Process ( terminateProcess )


import API
  ( P2PRCapi(..)
  , MapPortRequest(..)
  , p2prcAPI
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

{-|
  This function starts and bootstraps the P2PRC runtime that associates the a specific host's machine port to a DNS address to expose a certain application to the P2PRC network. You will only need to also import the 'MkMapPortRequest' data constructor to represent the this port request.

  ==== __Example__

  This example demonstrates how it can be ran on the IO context:

  @
  example :: IO ()
  example = do
    runP2PRC
      ( MkMapPortRequest 8080 "jose.akilan.io"
      )
  @
-}
runP2PRC
  :: MapPortRequest   -- ^ TCP Port Request
  -> IO ()
runP2PRC
  ( MkMapPortRequest
      portNumber
      domainName
  )
  =
  let

  --
  -- TODO: add quickcheck testing (quickchecking-dynamic)
  --
  -- TODO: Change Standard Library
  -- TODO: add GDTA syntax to data types
  --
  -- TODO: need monad transformers to refactor the code
  --
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

  ( MkP2PRCapi
    { startServer     = startServer
    -- , execInitConfig  = execInitConfig
    , execListServers = execListServers
    , execMapPort     = execMapPort
    }
    ) = p2prcAPI

  in do
      let


      -- configValue <- execInitConfig

      -- TODO: get name of host server from config json

      -- print configValue
      -- putStrLn "\n\n\n"

      eitherStartProcessHandle <- startServer


      case eitherStartProcessHandle of
        (Right startProcessHandle) -> do

          let sleepNSecs i = threadDelay (i * 1000000)

          sleepNSecs 5

          outputStr <- execListServers
          print outputStr

          mapPortOut <- execMapPort $ MkMapPortRequest portNumber domainName

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

          exitOnQ $ terminateProcess startProcessHandle


        (Left err) -> print err


    where

    exitOnQ :: IO () -> IO ()
    exitOnQ exitF = do
      hSetBuffering stdin NoBuffering
      c <- getChar
      when (toLower c /= 'q') $ exitOnQ exitF
      exitF

