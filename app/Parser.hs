module Parser where

import CommonParserUtil
import Token
import Expr

import ParserTime

-- | Utility
rule prodRule action              = (prodRule, action, Nothing  )
ruleWithPrec prodRule action prec = (prodRule, action, Just prec)
ruleWithNoAction prodRule         = (prodRule, noAction, Nothing)

noAction rhs = return NoExpr

--
parserSpec :: ParserSpec Token Expr IO ()
parserSpec = ParserSpec
  {
    startSymbol = "IotaProg'",
    
    tokenPrecAssoc = [],

    parserSpecList =
    [
      ruleWithNoAction "IotaProg' -> IotaProg",
      
      ruleWithNoAction "IotaProg -> Rule",
      
      ruleWithNoAction "Rule -> EventHandler ; Predicate ; Actions",
      
      ruleWithNoAction "EventHandler -> ",
    ],
    
    baseDir = "./",
    actionTblFile = "action_table.txt",  
    gotoTblFile = "goto_table.txt",
    grammarFile = "prod_rules.txt",
    parserSpecFile = "mygrammar.grm",
    genparserexe = "yapb-exe",

    synCompSpec = Nothing,
    parserTime = ParserTime {
                   pa_startTime=startTime,
                   pa_finishTime=finishTime
                 }
  }
