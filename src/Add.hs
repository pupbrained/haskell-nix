{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Add where

import System.Process.Typed
import Text.InterpolatedString.QM (qmb)
import Utils (failWith, succeedWith, toNixArray)

validatePackage :: String -> IO Bool
validatePackage package = do
  -- FIXME: this breaks nix
  exitCode <- runProcess $ setStdout closed (proc "nix" ["search", package, "--json"])
  case exitCode of
    ExitSuccess -> return True
    _ -> return False

addPackage :: [String] -> FilePath -> IO ()
addPackage packages output = do
  validatePackages <- mapM validatePackage packages

  -- if any of the packages don't exist, fail
  unless (or validatePackages) $ do
    failWith $ putStrLn "One or more packages don't exist"

  let strToWrite =
        [qmb|
          \{pkgs, ...}: \{
          \  snowPkgs = with pkgs; {toNixArray $ sort packages};
          }
        |]
   in succeedWith $ writeFile output strToWrite
