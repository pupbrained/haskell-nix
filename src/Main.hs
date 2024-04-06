module Main where

import Relude (getArgs)

main :: IO ()
main = do
    args <- getArgs
    print args
