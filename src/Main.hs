{-# LANGUAGE OverloadedStrings #-}

module Main where

import Add (addPackage)
import Data.Version (showVersion)
import Options.Applicative (
  Parser,
  ParserInfo,
  command,
  customExecParser,
  flag,
  fullDesc,
  help,
  helper,
  info,
  long,
  metavar,
  option,
  prefs,
  progDesc,
  short,
  showDefault,
  showHelpOnError,
  str,
  strArgument,
  subparser,
  value,
 )
import Paths_nix_snow (version)
import Utils (succeedWith)

data Opts = Opts
  { optVersion :: Bool
  , optOutput :: FilePath
  , optCommand :: Command
  }

data Command
  = Add [String]
  | Remove [String]

optsParser :: ParserInfo Opts
optsParser =
  info (parser <**> helper) $
    mconcat
      [ fullDesc
      , progDesc "Manage nix packages"
      ]
  where
    parser :: Parser Opts
    parser =
      Opts
        <$> flag False True (long "version" <> short 'v' <> help "Print version information")
        <*> option str (long "output" <> short 'o' <> metavar "FILE" <> help "Output file" <> value "./snowpkgs.nix" <> showDefault)
        <*> subparser
          ( command "add" (info (Add <$> many (strArgument (metavar "PACKAGES"))) (progDesc "Add a package"))
              <> command "remove" (info (Remove <$> many (strArgument (metavar "PACKAGES"))) (progDesc "Remove a package"))
          )

main :: IO ()
main = do
  opts <- customExecParser (prefs showHelpOnError) optsParser

  when (optVersion opts) $ do
    succeedWith $ putStrLn ("nix-snow " <> showVersion version)

  case optCommand opts of
    Add packages -> Add.addPackage packages (optOutput opts)
    Remove packages -> putStrLn $ "Removing packages: " <> show packages
