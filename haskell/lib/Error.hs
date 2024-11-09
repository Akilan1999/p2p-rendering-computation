module Error where


data Error
  = MkUnknownError String
  | MkErrorSpawningProcess String
  | MkSystemError Int String String
  deriving Show

type IOEitherError a = IO (Either Error a)

assignError :: String -> Error
assignError = MkUnknownError
--
-- TODO: add megaparsec to parse Error Messages
--
-- TODO: add error when internet connection is off
--
-- Left (MkUnknownError "Unexpected end-of-input, expecting JSON value")
-- MkSystemError 1 "/home/xecarlox/Desktop/p2p-rendering-computation/p2p-rendering-computation" "2024/11/09 21:08:06 Get \"http://0.0.0.0:8088/MAPPort?port=3333&domain_name=\": dial tcp 0.0.0.0:8088: connect: connection refused\n"
