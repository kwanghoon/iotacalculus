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
import qualified Data.Set as Set

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


------------------------------------------------------------------------------------------
-- | Testing
------------------------------------------------------------------------------------------

test1 :: IO ()
test1 = do
  ruleText <- load "./examples/turn-on-hallway-light-when-the-front-door-is-unlocked.iota"
  ruleToJson ruleText
  rs <- installRule env1 ruleText
  (event2, iot2) <- driverECA event1 iot1 rs
  print "iot1:"
  print iot1
  print "iot2:"
  print iot2
  return ()

env1 :: Environment
env1 = 
  Map.fromList [
    ("front_door", "front_door@myhome"),
    ("hallway_light", "hallway_light@myhome")
  ]

event1 :: Set.Set Event
event1 =
  Set.fromList [
    EventField "front_door@myhome" "lock" (ConstantLiteral "locked") (ConstantLiteral "unlocked")
  ]

iot1 :: IoT
iot1 = (dev1, input1, output1, timer1)

dev1 :: Map.Map Name (Capability, Map.Map AttributeName Literal)
dev1 = 
  Map.fromList [
    ("front_door@myhome", 
      ( "lock"
      , Map.fromList [("lock", ConstantLiteral "unlocked")])),
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

------------------------------------------------------------------------------------------

test2 :: IO ()
test2 = do
  ruleText <- load "./examples/turn-off-hallway-light-five-minutes-after-the-front-door-is-locked.iota"
  ruleToJson ruleText
  rs <- installRule env2 ruleText
  (event2', iot2') <- driverECA event2 iot2 rs
  print "iot2:"
  print iot2
  print "event2:"
  print event2
  print "iot2':"
  print iot2'
  print "event2':"
  print event2'
  return ()

env2 :: Environment
env2 = 
  Map.fromList [
    ("front_door", "front_door@myhome"),
    ("hallway_light", "hallway_light@myhome"),
    ("light_timer", "hallway_light_timer@myhome")
  ]

event2 :: Set.Set Event
event2 =
  Set.fromList [
    EventField "front_door@myhome" "lock" (ConstantLiteral "unlocked") (ConstantLiteral "locked")
  ]

iot2 :: IoT
iot2 = (dev2, input2, output2, timer2)

dev2 :: Map.Map Name (Capability, Map.Map AttributeName Literal)
dev2 = 
  Map.fromList [
    ("front_door@myhome", 
      ( "lock"
      , Map.fromList [("lock", ConstantLiteral "locked")])),
    ("hallway_light@myhome", 
      ( "switch"
      ,  Map.fromList [("switch", ConstantLiteral "on")]))
  ]

input2 :: Map.Map Name (ValueType, Literal)
input2 =
  Map.fromList [
  ]

output2 :: Map.Map Name ([ ValueType], [ Literal ])
output2 =
  Map.fromList [
  ]

timer2 :: Map.Map Name Literal
timer2 = 
  Map.fromList [
  ]
