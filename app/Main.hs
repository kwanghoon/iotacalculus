{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

module Main (main) where

import CommonParserUtil

import AST
import Expr
import Lexer
import Parser

import Control.Monad (when)

import System.Environment (getArgs)

import TokenInterface

import Data.Aeson
import GHC.Generics

-- Pretty printing JSON
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson.Encode.Pretty (encodePretty)

import qualified Data.Text.IO as DTIO


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
  astRule <- parsing False parserSpec ((),1,1,text)
            (aLexer lexerSpec) (fromToken (endOfToken lexerSpec))
  -- when (verbose) $ putStrLn (show (fromASTRule astRule))
  -- when (verbose) $ putStrLn (show (toJSON (fromASTRule astRule)))
  when (verbose) $ DTIO.putStrLn (jsonToText (fromASTRule astRule))
  when (verbose) $ putStrLn "Done."

-- Pretty printing JSON
lsbToText :: LBS.ByteString -> T.Text
lsbToText = E.decodeUtf8 . LBS.toStrict

jsonToText :: Rule -> T.Text
jsonToText = lsbToText . encodePretty
