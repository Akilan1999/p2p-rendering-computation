module Error
  ( Error(..)
  , IOEitherError
  , assignError
  ) where


{- |
  Haskell-side Error value. This type is designed to parse and track System and P2PRC's error signals in a safe and effective manner.

  It does have an 'MkUnknownError' value which is meant to be warn about new kinds of error not yet accounted in this client. Github issues and pull requests are very welcome to improve error handling by parsing more types of errors.

-}
data Error
  = MkCLISystemError        -- ^ This is a CLI System Error
      Int                     -- ^ System error code
      String                  -- ^ Command name executed
      String                  -- ^ Error output
  | MkErrorSpawningProcess  -- ^ Spawing process error
      String                  -- ^ Spawning executable name
  | MkUnknownError          -- ^ This is an unparsed P2PRC's error
      String                  -- ^ Unparsed error message
  deriving Show

-- | Type synonym for an IO action with either returns an Error or a parsed value
type IOEitherError a = IO ( Either Error a)

assignError :: String -> Error
assignError = MkUnknownError
--
-- TODO: add megaparsec to parse Error Messages
--
-- TODO: add error when internet connection is off
--
-- Left (MkUnknownError "Unexpected end-of-input, expecting JSON value")
-- MkSystemError 1 "/home/xecarlox/Desktop/p2p-rendering-computation/p2p-rendering-computation" "2024/11/09 21:08:06 Get \"http://0.0.0.0:8088/MAPPort?port=3333&domain_name=\": dial tcp 0.0.0.0:8088: connect: connection refused\n"
