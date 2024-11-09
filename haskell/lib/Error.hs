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
