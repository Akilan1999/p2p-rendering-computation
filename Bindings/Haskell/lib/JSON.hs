{-# LANGUAGE OverloadedStrings #-}

module JSON
  ( P2prcConfig(..)
  , IPAddressTable(..)
  , IPAddress(..)
  , ServerInfo(..)
  , MapPortResponse(..)
  )
  where


import Control.Monad ( mzero )

import qualified Data.Text as T

import Data.Aeson


{-# WARNING MapPortResponse "This newtype is unstable at the moment due to the P2PRC's library error handling bug. For more information visit: https://github.com/Akilan1999/p2p-rendering-computation/issues/114#issuecomment-2474737015" #-}

-- ^ This represents P2PRC's response to the TCP port and DNS address allocation. This value will confirm the successful allocation and return information about it.
newtype MapPortResponse
  = MkMapPortResponse   -- ^ Allocation information value
    String                -- ^ Column separated Host's IP address and Port String
  deriving Show



instance FromJSON MapPortResponse where
  parseJSON (Object o) = do

    ipAddress <- o .: "IPAddress"

    pure $ MkMapPortResponse ipAddress

  parseJSON _ = mzero



{-# WARNING P2prcConfig "This type is unstable at the moment due to the P2PRC's library error handling bug. For more information visit: https://github.com/Akilan1999/p2p-rendering-computation/issues/114#issuecomment-2474737015" #-}

-- | This represents the server configuration that defines its attributes and behaviours in the network, as well as, the location of the runtime persistence artifacts.
newtype P2prcConfig
  = MkP2prConfig
    { machineName               :: String     -- ^ Machine Name
    } deriving Show

    -- , hostServerPort            :: Int
    -- , iPV6Address               :: Maybe String
    -- , proxyPort                 :: Maybe Int
    -- , fRPServerPort             :: Bool
    -- , behindNAT                 :: Bool
    -- , iPTableKey                :: String
    -- , bareMetal                 :: Bool
    -- , customConfig              :: String
    --
    -- , iPTable                   :: String -- File
    -- , dockerContainers          :: String -- Directory
    -- , defaultDockerFile         :: String -- Directory
    -- , dockerRunLogs             :: String -- Directory
    -- , speedTestFile             :: String -- File
    -- , pluginPath                :: String -- Directory
    -- , trackContainersPath       :: String -- File
    -- , publicKeyFile             :: String -- File
    -- , privateKeyFile            :: String -- File
    -- , pemFile                   :: String -- File
    -- , keyFile                   :: String -- File
    -- , groupTrackContainersPath  :: String -- File


-- TODO: p2prc API
  --
  -- ListServers
    -- remove "ip_address" root field if not needed
    -- "Nat field" returning a JSON Boolean
    -- serverPort as a JSON number
    -- baremetalPort as a JSON number
    -- have either IPV4 or IPV6 field visible
    -- remove "customInformation" if not needed anymore
    -- remove "escapeImplementation" if not needed anymore
  --
  -- Config file
    --
    -- Fix JSON number: ServerPort
    -- Fix: IPV6Address dont show if value does not exist
    -- Fix JSON number: ProxyPort to number (dont show if it does not exist)
    -- Fix JSON number: fRPServerPort
    -- Fix JSON boolean: fRPServerPort
    -- Fix JSON boolean: behindNAT
    -- Fix JSON boolean: bareMetal
    -- remove "customConfig" if not needed
  --
  -- MapPort
    -- to have a dedicated ip address (with type either IPV6 or IPV4 fields)
    -- to have a dedicated port field


instance FromJSON P2prcConfig where
  parseJSON (Object o) = do

    machineName <- o .: "MachineName"

    pure
      $ MkP2prConfig
        { machineName=machineName
        }

  parseJSON _ = mzero


{-# WARNING IPAddressTable "This newtype is highly unstable due to undergoing work on improving P2PRC's server api. For more information, visit: https://github.com/Akilan1999/p2p-rendering-computation/issues/114" #-}

-- | This is a wrapper value that parses a json key value from the list of ip addresses in the network.
newtype IPAddressTable
  = MkIPAddressTable      -- ^ Wrapping constructor
    [ServerInfo]            -- ^ List current network servers
  deriving Show


instance FromJSON IPAddressTable where
  parseJSON = withObject "IPAdressTable" $
    \ v ->
      MkIPAddressTable <$> v .: "ip_address"


{-# WARNING ServerInfo "This type is highly unstable due to undergoing work on improving P2PRC's server api. For more information, visit: https://github.com/Akilan1999/p2p-rendering-computation/issues/114" #-}
{- |
  This is a record that keeps track of the current state of every node in the network. It is crucial information required for orchestration strategies.
-}
data ServerInfo =
  MkServerInfo
    { name                  :: T.Text         -- ^ Machine name
    , ip                    :: IPAddress      -- ^ Machine IP address
    , latency               :: Int            -- ^ Response latency
    , download              :: Int            -- ^ Download speed
    , upload                :: Int            -- ^ Upload speed
    , serverPort            :: Int            -- ^ Server port number
    , bareMetalSSHPort      :: Maybe Int      -- ^ SSH machine port number
    , nat                   :: Bool           -- ^ Checking if node is behind a NAT
    , escapeImplementation  :: Maybe T.Text   -- ^ Type of NAT trasversal technique
    , customInformation     :: Maybe T.Text   -- ^ Custom String information
    }
  deriving Show

-- | This is a simple representation of the IP address of nodes in the network.
data IPAddress
  = MkIPv4 String -- ^ IP version 4 address
  | MkIPv6 String -- ^ IP version 6 address
  deriving Show


instance FromJSON ServerInfo where
  parseJSON = withObject "ServerInfo" $
    \ o -> do

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



