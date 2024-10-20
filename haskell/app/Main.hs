{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson

import System.Process (readProcess, terminateProcess, ProcessHandle, spawnProcess)

import Control.Concurrent (threadDelay)

import qualified Data.ByteString.Lazy.Char8 as LBC8

-- TODO: Change Standard Library


-- TODO: Use it
import qualified Data.Text as T


-- TODO: create library to abstract shell and go-level logic
main :: IO ()
main = do
  -- TODO: add IO arguments; flag to optionally cleanup environment
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

  confOut <- execInitConfig
  putStrLn confOut

  -- TODO: create record with all functions needed
  --
  -- TODO: Add loop to print servers list

  startProcessHandle <- startServer

  sleepNSecs 5

  outputStr <- execListServers
  print outputStr

  mapPortOut <- execMapPort 3333
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
  , execMapPort       :: Int -> IO String
  }


getP2prcAPI :: IO P2prAPI
getP2prcAPI = do

  p2prcCmd <- cleanEnvAndgetP2PrcCmd


  let execProcP2PrcParser = execProcP2PrcParser_ p2prcCmd
  let execProcP2Prc       = execProcP2Prc_ p2prcCmd

  -- TODO: initialise environment; perhaps cleanup state files
  --
  -- TODO: make it pure environment by default
  let execInitConfig = execProcP2Prc [MkOptAtomic "--dc"] MkEmptyStdInput
  -- TODO: get name of host server from config json
  -- TODO: change some default config attributes
  -- TODO: parse config file


  return $
    MkP2prAPI
      { startServer     = spawnProcP2Prc p2prcCmd [MkOptAtomic "--s"]
      , execInitConfig  = execInitConfig
      , execListServers = execProcP2PrcParser     [MkOptAtomic "--ls"] MkEmptyStdInput
      , execMapPort =
        \portNumber ->
          execProcP2Prc
            [ MkOptTuple
              ( "--mp"
              , show portNumber
              )
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


data IPAddress
  = MkIPv4 String
  | MkIPv6 String
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
          , bareMetalSSHPort      = getBMShhPort bmSshPort
          , nat                   = getNat nat
          , escapeImplementation  = mEscImpl
          , customInformation     = custInfo
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
  show (MkStdInputVal v)  = show v


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



execProcP2Prc_ :: CLICmd -> [CLIOpt] -> StdInput -> IO String
execProcP2Prc_ p2prcCmd ops stdi =
  readProcess
    p2prcCmd
      (optsToCLI ops)
      (show stdi)


data CLIOpt
  = MkEmptyOpts
  | MkOptAtomic String
  | MkOptTuple (String, String)


type CLIOptsInput = [String]
type CLICmd = String


optsToCLI :: [CLIOpt] -> CLIOptsInput
optsToCLI = concatMap _optToCLI
  where

  _optToCLI :: CLIOpt -> CLIOptsInput
  _optToCLI MkEmptyOpts         = []
  _optToCLI (MkOptAtomic o)     = [o]
  _optToCLI (MkOptTuple (o, v)) = [o, v]


spawnProcP2Prc :: CLICmd -> [CLIOpt] -> IO ProcessHandle
spawnProcP2Prc p2prcCmd =
  spawnProcess p2prcCmd . optsToCLI



eitherErrorDecode :: Either String a -> Either Error a
eitherErrorDecode esa =
  case esa of
    (Left s) -> Left $ assignError s
    (Right v) -> Right v


data Error = MkError String
  deriving Show

assignError :: String -> Error
assignError = MkError
-- TODO: add megaparsec to parse Error Messages


cleanEnvAndgetP2PrcCmd :: IO String
cleanEnvAndgetP2PrcCmd = do

  -- assumes the program is ran inside the haskell module in p2prc's repo
  readProcess "pwd" [] "" >>=
    \pOut -> do

      -- need to strip the newline and return a String again
      let pwdOut = T.unpack . T.strip . T.pack $ pOut

      putStrLn pOut

      -- cleanEnvironment

      -- assumes that last path segment is "haskell" and that p2prc binary's name is "p2p-rendering-computation"
      readProcess "sed" ["s/haskell/p2p-rendering-computation/"] pwdOut


cleanEnvironment :: IO ()
cleanEnvironment = do

  lsOut <- readProcess "ls" [] []
  putStrLn lsOut

  -- rmOut <- readProcess "rm" ["config.json"] [] -- client/ p2prc.* plugin/ server/"
  -- putStrLn rmOut

  -- rmOut <- readProcess "rm" [ "config.json" , "-r", "plugin/" , "-r", "client/" ] []
    -- p2prc.* plugin/ server/"
  -- putStrLn rmOut

  putStrLn "\n\ncleaning environment..."

  lsOut <- readProcess "ls" [] []
  putStrLn lsOut


sleepNSecs :: Int -> IO ()
sleepNSecs i = threadDelay (i * 1000000)
