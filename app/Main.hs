module Main (main) where

import System.Environment (getArgs)
import Run(_main)

-- Main
main :: IO ()
main = do
  args <- getArgs
  putStrLn "args:"
  mapM_ putStrLn args
  _main args


