{-# LANGUAGE OverloadedStrings #-}

module Main where


import System.Exit ( ExitCode(ExitFailure) )


import System.Process
  ( readProcessWithExitCode
  , proc
  , createProcess
  , terminateProcess
  , readProcess
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

  -- TODO: add IO arguments;
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


  eitherStartProcessHandle <- startServer
  --
  -- TODO: get name of host server from config json
  -- TODO: add option to change some default config attributes
  -- TODO: parse config file
  --
  -- TODO: abstract init and server running logic into its function
      -- work on looping function

  case eitherStartProcessHandle of
    (Right startProcessHandle) -> do

      sleepNSecs 5

      outputStr <- execListServers
      print outputStr

      mapPortOut <- execMapPort 3333 -- TODO: add domain name
      --
      -- TODO: Add loop to print servers list
      --

      case mapPortOut of
        (Right v) -> putStrLn v
        (Left e)  -> print e

      terminateProcess startProcessHandle

    (Left err) -> print err



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
  { startServer       :: IOEitherError ProcessHandle
  , execInitConfig    :: IOEitherError String
  , execListServers   :: IOEitherError IPAdressTable
  , execMapPort       :: Int -> IOEitherError String -- TODO: Parse JSON data
  }


getP2prcAPI :: IO P2prAPI
getP2prcAPI = do

  cleanEnvironment

  p2prcCmd <- getP2PrcCmd

  let execProcP2PrcParser = execProcP2PrcParser_ p2prcCmd
  let execProcP2Prc       = eitherExecProcess p2prcCmd


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
execProcP2PrcParser_ p2prcCmd opts stdInput =
  do
    val <- eitherExecProcess p2prcCmd opts stdInput

    pure $ case val of
      (Right v) -> eitherErrDecode v
      (Left e)  -> Left e


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


eitherExecProcess :: CLICmd -> [CLIOpt] -> StdInput -> IOEitherError String
eitherExecProcess cmd opts input =
  do
    (code, out, err) <-
      readProcessWithExitCode
        cmd
        (optsToCLI opts)
        (show input)

    pure $
      case code of
        ExitFailure i -> Left $ MkSystemError i err
        _             -> Right out



optsToCLI :: [CLIOpt] -> CLIOptsInput
optsToCLI = concatMap _optToCLI
  where

  _optToCLI :: CLIOpt -> CLIOptsInput
  _optToCLI MkEmptyOpts         = []
  _optToCLI (MkOptAtomic o)     = [o]
  _optToCLI (MkOptTuple (o, v)) = [o, v]


spawnProcP2Prc :: CLICmd -> [CLIOpt] -> IOEitherError ProcessHandle
spawnProcP2Prc cmd opts =
  do
    let prc = proc cmd $ optsToCLI opts

    creationResult <- createProcess prc

    let (_, _, _, ph) = creationResult

    case creationResult of
      (_, _, Just _, _) ->
        do
          terminateProcess ph
          pure $ Left
            $ MkErrorSpawningProcess $ "Error executing: " ++ cmd

      _-> pure $ Right ph


eitherErrorDecode :: Either String a -> Either Error a
eitherErrorDecode esa =
  case esa of
    (Left s)  -> Left $ assignError s
    (Right v) -> Right v


data Error
  = MkUnknownError String
  | MkErrorSpawningProcess String
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

  let trimString = T.unpack . T.strip . T.pack

  -- TODO: change to "eitherExecProcess"

  trimString <$> readProcess "pwd" [] "" >>=
    \pwdOut ->

      -- TODO: change to "eitherExecProcess"
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
