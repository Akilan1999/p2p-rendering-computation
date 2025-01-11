module Environment
  ( cleanEnvironment
  )
  where


import System.Directory
  ( doesDirectoryExist
  , doesFileExist
  , removeDirectoryRecursive
  , removeFile
  )

import Control.Monad ( when )



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

