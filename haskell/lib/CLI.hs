module CLI where

import System.Process
  ( readProcessWithExitCode
  , proc
  , createProcess
  , terminateProcess
  , ProcessHandle
  )

import System.Exit
  ( ExitCode(ExitFailure)
  )

import Error
  ( IOEitherError
  , Error(..)
  , assignError
  )

import Data.Aeson
  ( FromJSON
  , eitherDecode
  )

import qualified Data.Text as T

import qualified Data.ByteString.Lazy.Char8 as LBC8



data StdInput
  = MkEmptyStdInput
  | MkStdInputVal String


instance Show StdInput where
  show MkEmptyStdInput    = ""
  show (MkStdInputVal v)  = v


data CLIOpt
  = MkEmptyOpts
  | MkOptAtomic String
  | MkOptTuple (String, String)

type CLIOptsInput = [String]
type CLICmd       = String


eitherExecProcessParser ::
  FromJSON a =>
    CLICmd -> [CLIOpt] -> StdInput -> IOEitherError a
eitherExecProcessParser p2prcCmd opts stdInput =
  do
    val <- eitherExecProcess p2prcCmd opts stdInput

    pure $ case val of
      (Right v) -> eitherErrDecode v
      (Left e)  -> Left e


eitherErrDecode ::
  FromJSON a =>
    String -> Either Error a
eitherErrDecode = eitherErrorDecode . eitherDecode . LBC8.pack


eitherExecProcess :: CLICmd -> [CLIOpt] -> StdInput -> IOEitherError String
eitherExecProcess cmd opts input =
  do
    (code, out, err) <-
      readProcessWithExitCode
        cmd
        (optsToCLI opts)
        (show input)

    pure $ case code of
      ExitFailure i -> Left $ MkCLISystemError i cmd err
      _             -> Right out


optsToCLI :: [CLIOpt] -> CLIOptsInput
optsToCLI = concatMap _optToCLI
  where

  _optToCLI :: CLIOpt -> CLIOptsInput
  _optToCLI MkEmptyOpts         = []
  _optToCLI (MkOptAtomic o)     = [o]
  _optToCLI (MkOptTuple (o, v)) = [o, v]


spawnProcP2Prc :: CLICmd -> [CLIOpt] -> IOEitherError ProcessHandle
spawnProcP2Prc cmd opts =
  do
    let prc = proc cmd $ optsToCLI opts

    creationResult <- createProcess prc

    let (_, _, _, ph) = creationResult

    case creationResult of
      (_, _, Just _, _) -> do

        terminateProcess ph

        pure $ Left $ MkErrorSpawningProcess cmd

      _-> pure $ Right ph


eitherErrorDecode :: Either String a -> Either Error a
eitherErrorDecode esa =
  case esa of
    (Left s)  -> Left $ assignError s
    (Right v) -> Right v


getP2PrcCmd :: IOEitherError String
getP2PrcCmd = do

  -- assumes the program is ran inside the haskell module in p2prc's repo
  -- assumes that last path segment is "haskell" and that p2prc binary's name is "p2p-rendering-computation"

  let trimString = T.unpack . T.strip . T.pack

  eitherErrPwd <- eitherExecProcess "pwd" [MkEmptyOpts] MkEmptyStdInput

  case eitherErrPwd of

    (Right pwdOut) ->
      eitherExecProcess
        "sed"
        [ MkOptAtomic "s/haskell/p2p-rendering-computation/" ]
        $ MkStdInputVal
          $ trimString pwdOut

    err -> pure err


