module Main where

import System.Console.GetOpt
import System.Environment (getArgs)
import Interpreter

data Options = Options
  { optPrelude :: Bool
  , optHelp    :: Bool
  } deriving Show

defaultOptions :: Options
defaultOptions = Options { optPrelude = True
                         , optHelp    = False }

options :: [OptDescr (Options -> Options)]
options =
  [ Option [] ["no-prelude"]
    (NoArg (\opts -> opts { optPrelude = False }))
    "Skip loading 'stdlib/prelude.unlisp'",
    Option ['h'] ["help"]
    (NoArg (\opts -> opts { optHelp = True } ))
    "Show help"
  ]

parseOpts :: [String] -> IO (Options, [String])
parseOpts argv =
  case getOpt Permute options argv of
    (o, n, [])   -> return (foldl (flip id) defaultOptions o, n)
    (_, _, errs) -> ioError (userError $ concat errs ++ usageInfo header options)

header :: String
header = "Usage: underlisp [OPTION...] [FILE]"

main :: IO ()
main = do
  args <- getArgs
  (opts, restArgs) <- parseOpts args
  if optHelp opts
    then putStrLn $ usageInfo header options
    else case restArgs of
           (filename:args) -> interpreteProgram (optPrelude opts) filename restArgs
           []              -> repl (optPrelude opts)
