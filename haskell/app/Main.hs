{-# LANGUAGE OverloadedStrings #-}

module Main where



import Data.Aeson

import System.Process (readProcess, terminateProcess, ProcessHandle, spawnProcess)

import Control.Monad (MonadPlus(mzero))

import Control.Concurrent (threadDelay)

import qualified Data.ByteString.Lazy.Char8 as LBC8


-- TODO: Use it
-- import qualified Data.Text as T



-- TODO: create library to abstract shell and go-level logic

main :: IO ()
main = do

  -- TODO: add IO arguments; flag to cleanup environment

  -- TODO create record with all functions needed

  -- TODO: initialise environment; perhaps cleanup state files
  _ <- execProcP2Prc [MkOptAtomic "-dc"] MkEmpty

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



-- TODO: refactor this datatype; better definition
data ServerInfo = MkServerInfo
  { serverInfoName                  :: String
  -- , serverInfoIp4                   :: String
  -- , serverInfoIp6                   :: String
  -- , serverInfoLatency               :: Int
  -- , serverInfoDownload              :: Int
  -- , serverInfoUpload                :: Int
  -- , serverInfoServerPort            :: Int
  -- , serverInfoBareMetalSSHPort      :: Maybe Int
  -- , serverInfoNat                   :: Bool
  -- , serverInfoEscapeImplementation  :: Maybe String
  -- , serverInfoCustomInformation     :: Maybe String
  }
  deriving Show


instance FromJSON ServerInfo where
  parseJSON (Object o) = MkServerInfo
    <$> o .: "Name"
    -- <*> o .: "IPV4"
    -- <*> o .: "IPV6"
    -- <*> o .: "Latency"
    -- <*> o .: "Download"
    -- <*> o .: "Upload"
    -- <*> o .: "ServerPort"
    -- <*> o .: "BareMetalSSHPort"
    -- <*> o .: "NAT"
    -- <*> o .: "EscapeImplementation"
    -- <*> o .: "CustomInformation"
  parseJSON _ = mzero


data StdInput
  = MkEmpty

instance Show StdInput where
  show MkEmpty = ""



type IOEitherError a = IO (Either Error a)

execProcP2PrcParser ::
  FromJSON a =>
    [CLIOpt] -> StdInput -> IOEitherError a
execProcP2PrcParser opts stdInput =
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

optsToCLI :: [CLIOpt] -> CLIOptsInput
optsToCLI = concatMap _optToCLI
  where

  _optToCLI :: CLIOpt -> CLIOptsInput
  _optToCLI (MkOptAtomic o) = [o]
  _optToCLI (MkOptTuple (o, v)) = [o, show v]


spawnProcP2Prc :: [CLIOpt] -> IO ProcessHandle
spawnProcP2Prc =
  spawnProcess p2prcCmd . optsToCLI


p2prcCmd = "/home/xecarlox/Desktop/p2p-rendering-computation/p2p-rendering-computation" :: String



getP2PrcCmd :: IO String
getP2PrcCmd = do
  -- assumes the program is ran inside the haskell module in p2prc's repo
  readProcess "pwd" [] "" >>=
    \pwdOut ->
      -- assumes that last path segment is "haskell" and that p2prc binary's name is "p2p-rendering-computation"
      readProcess "sed" ["s/haskell/p2p-rendering-computation/"] pwdOut


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


