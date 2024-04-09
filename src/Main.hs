{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.Version (showVersion)
import Paths_nix_snow (version)
import Text.ANSI (bold, underline)
import Text.InterpolatedString.QM (qmb)

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
      | "--help" `elem` args || "-h" `elem` args -> printHelp
      | "--version" `elem` args || "-v" `elem` args -> putStrLn ("nix-snow " <> showVersion version)
      | "add" `elem` args -> putStrLn "add"
      | "remove" `elem` args -> putStrLn "remove"
      | otherwise -> printHelp
