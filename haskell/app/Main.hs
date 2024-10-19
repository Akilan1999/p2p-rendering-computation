{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson

import System.Process (readProcess, terminateProcess, ProcessHandle, spawnProcess)

import Control.Concurrent (threadDelay)

import qualified Data.ByteString.Lazy.Char8 as LBC8


-- TODO: Use it
-- import qualified Data.Text as T


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

  cmdOut <- getP2PrcCmd

  startProcessHandle <- spawnProcP2Prc [MkOptAtomic "-s"]

  sleepNSecs 5

  outputStr <- (execProcP2PrcParser [MkOptAtomic "-ls"] MkEmpty :: IOEitherError IPAdressTable)
  print outputStr

  terminateProcess startProcessHandle

  putStrLn cmdOut


newtype IPAdressTable
  = MkIPAdressTable [ServerInfo]
  deriving Show

instance FromJSON IPAdressTable where
  parseJSON = withObject "IPAdressTable" $
    \v ->
      MkIPAdressTable <$> v .: "ip_address"


data ServerInfo = MkServerInfo
  { name                  :: String
  , ip                    :: IPAddress
  , latency               :: Int
  , download              :: Int
  , upload                :: Int
  , serverPort            :: Int                      -- TODO: verify if it is Maybe value
  , bareMetalSSHPort      :: Maybe Int                -- TODO: verify if it is Maybe value
  , nat                   :: Bool
  , escapeImplementation  :: Maybe String
  , customInformation     :: Maybe String
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

    getNat :: String -> Bool
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


-- TODO: remove this variable
p2prcCmd = "/home/xecarlox/Desktop/p2p-rendering-computation/p2p-rendering-computation" :: String


data StdInput
  = MkEmpty

instance Show StdInput where
  show MkEmpty = ""



type IOEitherError a = IO (Either Error a)

execProcP2PrcParser ::
  FromJSON a =>
    [CLIOpt] -> StdInput -> IOEitherError a
execProcP2PrcParser opts stdInput
    =
      eitherErrDecode
        <$> execProcP2Prc opts stdInput

  where

  eitherErrDecode = eitherErrorDecode . eitherDecode . LBC8.pack


execProcP2Prc :: [CLIOpt] -> StdInput -> IO String
execProcP2Prc ops stdi =
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


spawnProcP2Prc :: [CLIOpt] -> IO ProcessHandle
spawnProcP2Prc =
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
    \pwdOut ->
      -- assumes that last path segment is "haskell" and that p2prc binary's name is "p2p-rendering-computation"
      readProcess "sed" ["s/haskell/p2p-rendering-computation/"] pwdOut
