{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson

import System.Process (readProcess, terminateProcess, ProcessHandle, spawnProcess)

import Control.Concurrent (threadDelay)

import qualified Data.ByteString.Lazy.Char8 as LBC8


-- TODO: Use it
import qualified Data.Text as T


-- TODO: create library to abstract shell and go-level logic
main :: IO ()
main = do
  -- TODO: add IO arguments; flag to optionally cleanup environment
  -- TODO: add GDTA syntax to data types

  -- -- TODO: initialise environment; perhaps cleanup state files
  -- _ <- execProcP2Prc [MkOptAtomic "-dc"] MkEmpty


  -- TODO: create record with all functions needed
  --
  -- TODO: Add loop to print servers list

  p2prcCmd <- getP2PrcCmd

  startProcessHandle <- spawnProcP2Prc p2prcCmd [MkOptAtomic "-s"]

  sleepNSecs 5

  let execProcP2PrcParser = execProcP2PrcParser_ p2prcCmd

  outputStr <- execProcP2PrcParser [MkOptAtomic "-ls"] MkEmpty :: IOEitherError IPAdressTable
  print outputStr

  terminateProcess startProcessHandle


{-
data P2PRCapi =
  MkP2PRCapi
    { startServer :: IO ProcessHandle
    , defineConfiguration :: IO String
    , getListServers :: IOEitherError IPAdressTable
    }

getP2PRCapi :: IO P2PRCapi
getP2PRCapi = do
  p2prcCmd <- getP2PrcCmd

  startProcessHandle <- spawnProcP2Prc p2prcCmd [MkOptAtomic "-s"]

  sleepNSecs 5

  let execProcP2PrcParser = execProcP2PrcParser_ p2prcCmd

  outputStr <- execProcP2PrcParser [MkOptAtomic "-ls"] MkEmpty :: IOEitherError IPAdressTable
-}


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
  , serverPort            :: Int                      -- TODO: verify if it is Maybe value
  , bareMetalSSHPort      :: Maybe Int                -- TODO: verify if it is Maybe value
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
  parseJSON = withObject "ServerInfo" $ \o -> do
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

    getNat :: String -> Bool                -- TODO: Dangerous partial function call !!!!!!!!!!!!!!!!!!!
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
  = MkEmpty

instance Show StdInput where
  show MkEmpty = ""



type IOEitherError a = IO (Either Error a)

execProcP2PrcParser_ ::
  FromJSON a =>
    CLICmd -> [CLIOpt] -> StdInput -> IOEitherError a
execProcP2PrcParser_ p2prcCmd opts stdInput
    =
      eitherErrDecode
        <$> execProcP2Prc_ p2prcCmd opts stdInput

  where

  eitherErrDecode = eitherErrorDecode . eitherDecode . LBC8.pack


execProcP2Prc_ :: CLICmd -> [CLIOpt] -> StdInput -> IO String
execProcP2Prc_ p2prcCmd ops stdi =
  readProcess
    p2prcCmd
      (optsToCLI ops)
      (show stdi)


data CLIOpt
  = MkOptAtomic String
  | MkOptTuple (String, String)

type CLIOptsInput = [String]
type CLICmd = String

optsToCLI :: [CLIOpt] -> CLIOptsInput
optsToCLI = concatMap _optToCLI
  where

  _optToCLI :: CLIOpt -> CLIOptsInput
  _optToCLI (MkOptAtomic o) = [o]
  _optToCLI (MkOptTuple (o, v)) = [o, show v]


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


sleepNSecs :: Int -> IO ()
sleepNSecs i = threadDelay (i * 1000000)



getP2PrcCmd :: IO String
getP2PrcCmd = do

  -- assumes the program is ran inside the haskell module in p2prc's repo
  readProcess "pwd" [] "" >>=
    \pwdOut -> do

      -- need to strip the newline and return a String again
      let strip = T.unpack . T.strip . T.pack

      -- assumes that last path segment is "haskell" and that p2prc binary's name is "p2p-rendering-computation"
      strip <$> readProcess "sed" ["s/haskell/p2p-rendering-computation/"] pwdOut

