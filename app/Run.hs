{-# LANGUAGE DeriveGeneric #-}
module Run where

import CommonParserUtil

import AST
import Expr
import Lexer
import Parser

import TokenInterface

import Control.Monad (when)

------------------------------------------------------------------------
-- | Pretty printing JSON
------------------------------------------------------------------------

import qualified Data.ByteString.Lazy.Char8 as B

------------------------------------------------------------------------

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
  let rule = fromASTRule astRule	    
  -- when (verbose) $ putStrLn (show rule)
  -- when (verbose) $ putStrLn (show (toJSON rule))
  when (verbose) $ ruleTojson rule
  when (verbose) $ putStrLn "Done."

-- | Utilities

load :: FilePath -> IO Rule
load fileName =
  do text <- readFile fileName
     astRule <- parsing False parserSpec ((),1,1,text)
                  (aLexer lexerSpec) (fromToken (endOfToken lexerSpec))
     return $ fromASTRule astRule

ruleTojson :: Rule -> IO ()
ruleTojson rule = B.putStrLn $ toJson $ rule