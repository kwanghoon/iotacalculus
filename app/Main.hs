module Main (main) where

import CommonParserUtil

import Lexer
import Parser

import Control.Monad (when)

import System.Environment (getArgs)

import TokenInterface

-- Main
main :: IO ()
main = do
  args <- getArgs
  putStrLn "args:"
  mapM_ putStrLn args
  _main args

-- Todo: Can I fix to have "test" as a command in stack exec?

_main :: [FilePath] -> IO ()
_main [] = return ()
_main (fileName:args) = 
   do _ <- doProcess True fileName
      _main args

doProcess :: Bool -> FilePath -> IO ()
doProcess verbose fileName = do
  text <- readFile fileName
  -- when (verbose) $ putStrLn "Lexing..."
  -- (_,_,terminalList) <- lexingWithLineColumn lexerSpec 1 1 text
  -- when (verbose) $ mapM_ putStrLn $ map terminalToString terminalList
  when (verbose) $ putStrLn "Parsing..."
  _ <- parsing False parserSpec ((),1,1,text)
            (aLexer lexerSpec) (fromToken (endOfToken lexerSpec))
  -- when (verbose) $ putStrLn (show expr)
  when (verbose) $ putStrLn "Done."