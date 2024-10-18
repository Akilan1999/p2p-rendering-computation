{-# LANGUAGE OverloadedStrings #-}

module Main where


import Data.Aeson

import Data.List

import System.Process (readProcess, terminateProcess, ProcessHandle, spawnProcess)

import Control.Monad (MonadPlus(mzero))

import Control.Concurrent (threadDelay)

import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LBC8


import qualified Data.Text as T


main :: IO ()
main = do

  -- outputStr <- execProcP2Prc ["-dc"]

  cmdOut <- getP2PrcCmd
  putStrLn cmdOut

  -- startProcessHandle <- spawnProcP2Prc ["-s"]

  -- sleepNSecs 5

  -- outputStr <- (execProcP2PrcParser ["-ls"] :: IO (Either String IPAdressTable))
  -- print outputStr

  -- outputStr <- execProcP2Prc ["--help"]
  -- print outputStr

  -- terminateProcess startProcessHandle


newtype IPAdressTable
  = MkIPAdressTable [ServerInfo]
  deriving Show

instance FromJSON IPAdressTable where
  parseJSON = withObject "IPAdressTable" $
    \v ->
      MkIPAdressTable <$> v .: "ip_address"



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



sleepNSecs :: Int -> IO ()
sleepNSecs i = threadDelay (i * 1000000)



execProcP2PrcParser :: FromJSON a => [String] -> IO (Either String a)
execProcP2PrcParser opts = eitherDecode . LBC8.pack <$> execProcP2Prc opts


execProcP2Prc :: [String] -> IO String
execProcP2Prc opts = readProcess p2prcCmd opts ""

spawnProcP2Prc :: [String] -> IO ProcessHandle
spawnProcP2Prc = spawnProcess p2prcCmd


p2prcCmd = "/home/xecarlox/Desktop/p2p-rendering-computation/p2prc" :: String


getP2PrcCmd :: IO String
getP2PrcCmd = do
  pwdOut <- readProcess "pwd" [] ""
  readProcess "sed" ["s/haskell/p2prc/"] pwdOut

