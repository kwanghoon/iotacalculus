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
      
      ruleWithNoAction "IotaProg -> Rules",
      
      ruleWithNoAction "IotaProg -> Rule",
      
      ruleWithNoAction "Rules -> rules string_literal OneOrMoreRules end",

      ruleWithNoAction "OneOrMoreRules -> Rule",
      
      ruleWithNoAction "OneOrMoreRules -> Rule OneOrMoreRules",

      ruleWithNoAction "Rule -> rule string_literal EventHandler ; Predicate ; Actions end",

      ruleWithNoAction "EventHandler -> FieldOrTimer [ . ~> ]",
      
      ruleWithNoAction "EventHandler -> FieldOrTimer [ . ~> Constant ]",
      
      ruleWithNoAction "EventHandler -> FieldOrTimer [ Constant ~> ]",
      
      ruleWithNoAction "EventHandler -> FieldOrTimer [ Constant ~> Constant ]",
      
      ruleWithNoAction "Predicate -> OrPred",
      
      ruleWithNoAction "OrPred -> OrPred \/ AndPred",
      
      ruleWithNoAction "OrPred -> AndPred",
      
      ruleWithNoAction "AndPred -> AndPred /\ EqNeqPred",
      
      ruleWithNoAction "AndPred -> EqNeqPred",
      
      ruleWithNoAction "EqNeqPred -> EqNeqPred = CompPred",
      
      ruleWithNoAction "EqNeqPred -> EqNeqPred != CompPred",
      
      ruleWithNoAction "EqneqPred -> CompExpr",
      
      ruleWithNoAction "CompExpr -> CompExpr < AdditiveExpr",
      
      ruleWithNoAction "CompExpr -> CompExpr <= AdditiveExpr",
      
      ruleWithNoAction "CompExpr -> CompExpr > AdditiveExpr",
      
      ruleWithNoAction "CompExpr -> CompExpr >= AdditiveExpr",
      
      ruleWithNoAction "CompExpr -> AdditiveExpr",
      
      ruleWithNoAction "AdditiveExpr -> AdditiveExpr + MultiplicativeExpr",
      
      ruleWithNoAction "AdditiveExpr -> AdditiveExpr - MultiplicativeExpr",
      
      ruleWithNoAction "AdditiveExpr -> MultiplicativeExpr",
      
      ruleWithNoAction "MultiplicativeExpr -> MultiplicativeExpr * UnaryExpr",
      
      ruleWithNoAction "MultiplicativeExpr -> MultiplicativeExpr / UnaryExpr",
      
      ruleWithNoAction "MultiplicativeExpr -> UnaryExpr",
      
      ruleWithNoAction "UnaryExpr -> - PrimaryExpr",
      
      ruleWithNoAction "UnaryExpr -> PrimaryExpr",
      
      ruleWithNoAction "UnaryExpr -> ~ PrimaryExpr",
      
      ruleWithNoAction "PrimaryExpr -> true",
      
      ruleWithNoAction "PrimaryExpr -> false",
      
      ruleWithNoAction "PrimaryExpr -> Constant",
      
      ruleWithNoAction "PrimaryExpr -> FieldOrTimer",
      
      ruleWithNoAction "PrimaryExpr -> ( Predicate )",

      ruleWithNoAction "Actions -> Action",
      
      ruleWithNoAction "Actions -> Action Actions",

      ruleWithNoAction "Action -> FieldOrTimer := AdditiveExpr",
      
      ruleWithNoAction "Action -> start Timer at AdditiveExpr",
      
      ruleWithNoAction "Action -> stop Timer",
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
