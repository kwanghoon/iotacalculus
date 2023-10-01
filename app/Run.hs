{-# LANGUAGE DeriveGeneric #-}
module Run where

import CommonParserUtil

import AST
import Expr
import Lexer
import Parser

import Interp

import TokenInterface

import Control.Monad (when)

import qualified Data.Map as Map

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
  when (verbose) $ ruleToJson rule
  when (verbose) $ putStrLn "Done."

-- | Utilities

load :: FilePath -> IO Rule
load fileName =
  do text <- readFile fileName
     astRule <- parsing False parserSpec ((),1,1,text)
                  (aLexer lexerSpec) (fromToken (endOfToken lexerSpec))
     return $ fromASTRule astRule

ruleToJson :: Rule -> IO ()
ruleToJson rule = B.putStrLn $ toJson $ rule

-- | Testing
test1 :: IO ()
test1 = do
  rule <- load ".\\examples\\turn-on-hallway-light-when-the-front-door-unlocks.iota"
  ruleToJson rule

env1 :: Environment
env1 = 
  Map.fromList [
    ("front_door", "front_door@myhome"),
    ("hallway_light", "hallway_light@myhome")
  ]

iot1 :: IoT
iot1 = (dev1, input1, output1, timer1)

dev1 :: Map.Map Name (Capability, Map.Map AttributeName Literal)
dev1 = 
  Map.fromList [
    ("front_door@myhome", 
      ( "lock"
      , Map.fromList [("lock", ConstantLiteral "locked")])),
    ("hallway_light@myhome", 
      ( "switch"
      ,  Map.fromList [("switch", ConstantLiteral "off")]))
  ]

input1 :: Map.Map Name (ValueType, Literal)
input1 =
  Map.fromList [
  ]

output1 :: Map.Map Name ([ ValueType], [ Literal ])
output1 =
  Map.fromList [
  ]

timer1 :: Map.Map Name Literal
timer1 = 
  Map.fromList [
  ]  

