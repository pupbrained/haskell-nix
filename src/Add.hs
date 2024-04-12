{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Add where

import System.Process.Typed (
  ExitCode (ExitSuccess),
  closed,
  proc,
  runProcess,
  setStderr,
  setStdout,
 )
import Text.InterpolatedString.QM (qmb)
import Utils (failWith, succeedWith, toNixArray)

validatePackage :: String -> IO (Bool, Maybe String)
validatePackage package = do
  exitCode <-
    runProcess $
      setStdout closed $
        setStderr closed (proc "nix-instantiate" ["--eval", "-E", "(import <nixpkgs> {})." <> package <> ".pname"])

  case exitCode of
    ExitSuccess -> return (True, Nothing)
    _ -> return (False, Just package)

addPackage :: [String] -> FilePath -> IO ()
addPackage packages output = do
  validatePackages <- mapM validatePackage packages

  case filter (not . fst) validatePackages of
    [] -> do
      let strToWrite =
            [qmb|
              \{pkgs, ...}: \{
              \  snowPkgs = with pkgs; {toNixArray $ sort packages};
              }
            |]
       in succeedWith $ writeFile output strToWrite
    invalidPackages -> failWith $ do
      putStrLn "Invalid packages:"
      mapM_ (putStrLn . fromMaybe "" . snd) invalidPackages
