module Error
  ( Error(..)
  , IOEitherError
  , assignError
  ) where


-- ^ Error type for the application
data Error
  = MkUnknownError
      String -- ^ error message
  | MkErrorSpawningProcess
      String -- ^ error message
  | MkSystemError
      Int -- ^ System error code
      String -- ^ String1
      String -- ^ String2
  deriving Show

-- ^ Type synonym for an IO action with either returns an Error or a parsed value
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
