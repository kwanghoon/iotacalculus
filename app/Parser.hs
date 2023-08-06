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
      -- | The IoTa calculus (base syntax)
      
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
      
      ruleWithNoAction "EventHandler -> any Group ( identifier -> Predicate )",
      
      ruleWithNoAction "Predicate -> OrPred",
      
      ruleWithNoAction "Predicate -> all Group ( x -> Predicate )",
      
      ruleWithNoAction "Predicate -> exists Group ( x -> Predicate )",
      
      -- ruleWithNoAction "Predicate -> FieldOrTimer is in Group",
      
      ruleWithNoAction "OrPred -> OrPred || AndPred",
      
      ruleWithNoAction "OrPred -> AndPred",
      
      ruleWithNoAction "AndPred -> AndPred && EqNeqPred",
      
      ruleWithNoAction "AndPred -> EqNeqPred",
      
      ruleWithNoAction "EqNeqPred -> EqNeqPred == CompPred",
      
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
      
      ruleWithNoAction "PrimaryExpr -> number_literal",
      
      ruleWithNoAction "PrimaryExpr -> FieldOrTimer",
      
      ruleWithNoAction "PrimaryExpr -> ( Predicate )",

      ruleWithNoAction "Constant -> identifier",
      
      ruleWithNoAction "Constant -> number_literal",
      
      ruleWithNoAction "Actions -> ",
      
      ruleWithNoAction "Actions -> Action Actions",

      ruleWithNoAction "Action -> FieldOrTimer := AdditiveExpr",
      
      ruleWithNoAction "Action -> start Timer at AdditiveExpr",
      
      ruleWithNoAction "Action -> stop Timer",

      ruleWithNoAction "Action -> map Group ( x -> Action )",

      ruleWithNoAction "FieldOrTimer -> identifier",

      ruleWithNoAction "FieldOrTimer -> identifier . identifier",

      ruleWithNoAction "Group -> devices",

      ruleWithNoAction "Group -> { zero_or_more_device_identifiers }",

      ruleWithNoAction "zero_or_more_device_identifiers -> ",

      ruleWithNoAction "zero_or_more_device_identifiers -> one_or_more_device_identifiers",

      ruleWithNoAction "one_or_more_device_identifiers -> identifier",

      ruleWithNoAction "one_or_more_device_identifiers -> identifier , one_or_more_device_identifiers"

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
