{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.Version (showVersion)
import Paths_nix_snow (version)
import Text.ANSI (bold, underline)
import Text.InterpolatedString.QM (qmb)

-- Helper functions
succeedWith :: IO () -> IO ()
succeedWith action = action >> exitSuccess

failWith :: IO () -> IO ()
failWith action = action >> exitFailure

main :: IO ()
main = do
  let
    printHelp =
      putStrLn
        [qmb| 
          {bold $ underline "Usage:"} {bold "nix-snow"} [options] <command>

          {bold $ underline "Commands:"}
          \  {bold "add"}     Add a package
          \  {bold "remove"}  Remove a package
          \  {bold "help"}    Print this help and exit

          {bold $ underline "Options:"}
          \  {bold "-h, --help"}     Print this help and exit
          \  {bold "-v, --version"}  Print version information and exit
        |]

  args <- getArgs

  case () of
    _
      -- Help
      | "--help" `elem` args || "-h" `elem` args -> succeedWith printHelp
      -- Version
      | "--version" `elem` args || "-v" `elem` args -> succeedWith $ putStrLn ("nix-snow " <> showVersion version)
      -- Subcommands
      | otherwise -> case () of
          _
            -- Add package
            | Just "add" == viaNonEmpty head args ->
                let
                  packages = fromMaybe [] (viaNonEmpty tail args)
                in
                  if null packages
                    then failWith $ putStrLn "No packages specified, use 'nix-snow add <package(s)>'"
                    else succeedWith $ print packages
            -- Remove package
            | Just "remove" == viaNonEmpty head args ->
                let
                  packages = fromMaybe [] (viaNonEmpty tail args)
                in
                  if null packages
                    then failWith $ putStrLn "No packages specified, use 'nix-snow remove <package(s)>'"
                    else succeedWith $ print packages
            -- Alias for help
            | "help" `elem` args -> succeedWith printHelp
            -- Fail for anything invalid
            | otherwise -> failWith printHelp
