{-# LANGUAGE OverloadedStrings #-}

module Main where


import System.Exit (ExitCode(ExitFailure))


import System.Process
  ( readProcessWithExitCode
  , readProcess
  , terminateProcess
  , spawnProcess
  , ProcessHandle
  )

import System.Directory
  ( doesDirectoryExist
  , doesFileExist
  , removeDirectoryRecursive
  , removeFile
  )

import Control.Concurrent (threadDelay)
import Control.Monad (when)

-- TODO: Change Standard Library

import qualified Data.Text as T

import qualified Data.ByteString.Lazy.Char8 as LBC8

import Data.Aeson



main :: IO ()
main = do

  -- TODO: add IO arguments; flag to optionally cleanup environment
  -- TODO: create DSL from the standard input
  --
  -- TODO: add GDTA syntax to data types

  p2prcAPI <- getP2prcAPI


  let
    ( MkP2prAPI
      { startServer     = startServer
      , execInitConfig  = execInitConfig
      , execListServers = execListServers
      , execMapPort     = execMapPort
      }
      ) = p2prcAPI

  _ <- execInitConfig


  startProcessHandle <- startServer
  -- TODO: get name of host server from config json
  -- TODO: add option to change some default config attributes
  -- TODO: parse config file
  --
  -- TODO: abstract init and server running logic into its function
      -- work on looping function

  sleepNSecs 5

  outputStr <- execListServers
  print outputStr

  mapPortOut <- execMapPort 3333 -- TODO: add domain name
  --
  -- TODO: Add loop to print servers list
  --
  putStrLn mapPortOut

  terminateProcess startProcessHandle


  -- Loop (Run replica of haskell program on different $NODES)
  --    - Start server
  --    - wait 4 seconds
  --    - Identify new node running p2prc with SSH external port exposed
  --    - SSH into machine
  --    - Use simple File transfer to setup files
  --    - Run server
  --    - Use remote machine p2prc cmd to map a port using --mp
  --    - Return back the exposed public IP and port number back to stdout



data P2prAPI = MkP2prAPI
  { startServer       :: IO ProcessHandle
  , execInitConfig    :: IO String
  , execListServers   :: IOEitherError IPAdressTable
  , execMapPort       :: Int -> IO String -- TODO: Parse JSON data
  }


getP2prcAPI :: IO P2prAPI
getP2prcAPI = do

  cleanEnvironment

  p2prcCmd <- getP2PrcCmd

  let execProcP2PrcParser = execProcP2PrcParser_ p2prcCmd
  let execProcP2Prc       = execProcP2Prc_ p2prcCmd


  return $
    MkP2prAPI
      { startServer = spawnProcP2Prc p2prcCmd [MkOptAtomic "--s"]

      , execInitConfig =
        execProcP2Prc
          [MkOptAtomic "--dc"]
          MkEmptyStdInput

      , execListServers =
        execProcP2PrcParser
          [MkOptAtomic "--ls"]
          MkEmptyStdInput

      , execMapPort =
        \portNumber ->
          execProcP2Prc
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
      }



newtype IPAdressTable
  = MkIPAdressTable [ServerInfo]
  deriving Show

instance FromJSON IPAdressTable where
  parseJSON = withObject "IPAdressTable" $
    \v ->
      MkIPAdressTable <$> v .: "ip_address"


data ServerInfo = MkServerInfo
  { name                  :: T.Text
  , ip                    :: IPAddress
  , latency               :: Int
  , download              :: Int
  , upload                :: Int
  , serverPort            :: Int
  , bareMetalSSHPort      :: Maybe Int
  , nat                   :: Bool
  , escapeImplementation  :: Maybe T.Text
  , customInformation     :: Maybe T.Text
  }
  deriving Show


data IPAddress        -- TODO: add extra type information to data type
  = MkIPv4 String     -- TODO: list of numbers
  | MkIPv6 String     -- TODO: list of numbers
  deriving Show


instance FromJSON ServerInfo where
  parseJSON = withObject "ServerInfo" $
    \o -> do

      name        <- o .: "Name"
      ip4str      <- o .: "IPV4"
      ip6str      <- o .: "IPV6"
      latency     <- o .: "Latency"
      download    <- o .: "Download"
      upload      <- o .: "Upload"
      serverPort  <- o .: "ServerPort"
      bmSshPort   <- o .: "BareMetalSSHPort"
      nat         <- o .: "NAT"
      mEscImpl    <- o .: "EscapeImplementation"
      custInfo    <- o .: "CustomInformation"

      pure $
        MkServerInfo
          { name                  = name
          , ip                    = getIPAddress ip4str ip6str
          , latency               = latency
          , download              = download
          , upload                = upload
          , serverPort            = getPortUNSAFE serverPort
          , bareMetalSSHPort      = getBMShhPort bmSshPort    -- TODO: add perhaps null placeholder??
          , nat                   = getNat nat
          , escapeImplementation  = mEscImpl
          , customInformation     = custInfo                  -- TODO: deal with null value
          }

    where

    getNat :: String -> Bool                -- TODO: Change it to normal JSON
    getNat ('T':_)  = True
    getNat _        = False

    getBMShhPort :: String -> Maybe Int     -- TODO: Dangerous partial function call !!!!!!!!!!!!!!!!!!!
    getBMShhPort []         = Nothing
    getBMShhPort bmSshPort  = Just $ getPortUNSAFE bmSshPort

    getPortUNSAFE :: String -> Int          -- TODO: Dangerous partial function call !!!!!!!!!!!!!!!!!!!
    getPortUNSAFE = read

    getIPAddress :: String -> String -> IPAddress
    getIPAddress []   ip6 = MkIPv6 ip6
    getIPAddress ip4  _   = MkIPv4 ip4



data StdInput
  = MkEmptyStdInput
  | MkStdInputVal String

instance Show StdInput where
  show MkEmptyStdInput    = ""
  show (MkStdInputVal v)  = v


type IOEitherError a = IO (Either Error a)

execProcP2PrcParser_ ::
  FromJSON a =>
    CLICmd -> [CLIOpt] -> StdInput -> IOEitherError a
execProcP2PrcParser_ p2prcCmd opts stdInput
    =
      eitherErrDecode
        <$> execProcP2Prc_ p2prcCmd opts stdInput

  where

  eitherErrDecode ::
    FromJSON a =>
      String -> Either Error a
  eitherErrDecode = eitherErrorDecode . eitherDecode . LBC8.pack


data CLIOpt
  = MkEmptyOpts
  | MkOptAtomic String
  | MkOptTuple (String, String)


type CLIOptsInput = [String]
type CLICmd       = String


execProcP2Prc_ :: CLICmd -> [CLIOpt] -> StdInput -> IO String
execProcP2Prc_ p2prcCmd ops stdi =
  --
  -- TODO: improve readProcess to return (Either Error String)
      -- TODO: improve error handling at type level
      -- TODO: use the "withReadProcess"
  --
  readProcess
    p2prcCmd
      (optsToCLI ops)
      (show stdi)


eitherExecProcess :: CLICmd -> [CLIOpt] -> StdInput -> IO (Either Error String)
eitherExecProcess cmd opts input =
  do
    (code, out, err) <- readProcessWithExitCode
        cmd
        (optsToCLI opts)
        (show input)

    case code of
      ExitFailure i -> pure (Left (MkSystemError i err))
      _             -> pure (Right out)



optsToCLI :: [CLIOpt] -> CLIOptsInput
optsToCLI = concatMap _optToCLI
  where

  _optToCLI :: CLIOpt -> CLIOptsInput
  _optToCLI MkEmptyOpts         = []
  _optToCLI (MkOptAtomic o)     = [o]
  _optToCLI (MkOptTuple (o, v)) = [o, v]


spawnProcP2Prc :: CLICmd -> [CLIOpt] -> IO ProcessHandle
spawnProcP2Prc p2prcCmd =
  --
  -- TODO: improve spawnProcess to return (Either Error String)
      -- TODO: improve error handling at type level
  --
  spawnProcess p2prcCmd . optsToCLI


eitherErrorDecode :: Either String a -> Either Error a
eitherErrorDecode esa =
  case esa of
    (Left s)  -> Left $ assignError s
    (Right v) -> Right v


data Error
  = MkUnknownError String
  | MkSystemError Int String
  deriving Show


assignError :: String -> Error
assignError = MkUnknownError
-- TODO: add megaparsec to parse Error Messages
--
-- TODO: add error when internet connection is off


getP2PrcCmd :: IO String
getP2PrcCmd = do
  -- assumes the program is ran inside the haskell module in p2prc's repo
  -- assumes that last path segment is "haskell" and that p2prc binary's name is "p2p-rendering-computation"

  -- TODO: change to safe type of function
  T.unpack . T.strip . T.pack <$> readProcess "pwd" [] "" >>=
    \pwdOut ->

      -- TODO: change to safe type of function
      readProcess "sed" ["s/haskell/p2p-rendering-computation/"] pwdOut



cleanEnvironment :: IO ()
cleanEnvironment = do

  mapM_ removeFileIfExists
    [ "cert.pem"
    , "config.json"
    , "key.pem"
    , "p2prc.privateKey"
    , "p2prc.PublicKeyBareMetal"
    ]

  mapM_ removeDirRIfExists
    [ "client"
    , "p2p"
    , "plugin"
    , "server"
    ]

  where

  removeIfExists
    :: (FilePath -> IO Bool)
    -> (FilePath -> IO ())
    -> FilePath
    -> IO ()
  removeIfExists doesItExists rmIt filePath =
    do
      res <- doesItExists filePath
      when res $ rmIt filePath

  removeDirRIfExists =
    removeIfExists
      doesDirectoryExist
      removeDirectoryRecursive

  removeFileIfExists =
    removeIfExists
      doesFileExist
      removeFile


sleepNSecs :: Int -> IO ()
sleepNSecs i = threadDelay (i * 1000000)
