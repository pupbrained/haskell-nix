module Utils where

import Data.Text (pack, unpack)

succeedWith :: IO () -> IO ()
succeedWith action = action >> exitSuccess

failWith :: IO () -> IO ()
failWith action = action >> exitFailure

toNixArray :: [String] -> String
toNixArray packages = "[" <> unpack (unwords (map pack packages)) <> "]"
