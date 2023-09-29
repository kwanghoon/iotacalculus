module Parser where

import CommonParserUtil
import Token
import AST
import Expr

import ParserTime

-- | Utility
rule prodRule action              = (prodRule, action, Nothing  )
ruleWithPrec prodRule action prec = (prodRule, action, Just prec)
ruleWithNoAction prodRule         = (prodRule, noAction, Nothing)

noAction rhs = return undefined -- NoExpr

--
parserSpec :: ParserSpec Token AST IO ()
parserSpec = ParserSpec
  {
    startSymbol = "IotaProg'",
    
    tokenPrecAssoc = [],

    parserSpecList =
    [
      -- | The IoTa calculus (base syntax)

      -- | IoTaProg' : Rule
      rule "IotaProg' -> IotaProg" ( \rhs -> return $ get rhs 1 ),
      
      -- | IoTaProg : Rule
      rule "IotaProg -> Rules" ( \rhs -> return $ get rhs 1 ),
      
      rule "IotaProg -> Rule" ( \rhs -> return $ get rhs 1 ),
      
      -- | Rules : Rule
      rule "Rules -> rules string_literal ZeroOrMoreDecls OneOrMoreRules end"
        ( \rhs -> let desc  = getText rhs 2
                      decls = fromASTDecls (get rhs 3)
		      rules = fromASTRules (get rhs 4)
                  in  return $ toASTRule $ NodeRule desc decls rules ),

      -- | OneOrMoreRules : [ Rule ]
      rule "OneOrMoreRules -> Rule" ( \rhs -> return $ toASTRules [ fromASTRule $ get rhs 1 ] ),
      
      rule "OneOrMoreRules -> Rules"
        ( \rhs -> return $ toASTRules [ fromASTRule $ get rhs 1 ] ),
      
      rule "OneOrMoreRules -> Rule OneOrMoreRules"
        ( \rhs -> let rule  = fromASTRule (get rhs 1)
	              rules = fromASTRules (get rhs 2)
	          in  return $ toASTRules (rule : rules) ),

      rule "OneOrMoreRules -> Rules OneOrMoreRules"
        ( \rhs -> let rule  = fromASTRule (get rhs 1)
	              rules = fromASTRules (get rhs 2)
	          in  return $ toASTRules (rule : rules) ),

      -- | Rule : Rule
      rule "Rule -> rule string_literal ZeroOrMoreDecls EventHandler ; OneOrMorePredicateActions end"
        ( \rhs -> let desc = getText rhs 2
	              decls = fromASTDecls (get rhs 3)
		      evhandler = fromASTEventHandler (get rhs 4)
		      multiplePredAction = fromASTMultiplePredicateActions (get rhs 6)
		  in return $ toASTRule $ LeafRule desc decls ( EMCA evhandler multiplePredAction )
        ),

      -- | ZeroOrMoreDecls : [ Decl ]
      rule "ZeroOrMoreDecls -> " ( \rhs -> return $ toASTDecls [] ),

      rule "ZeroOrMoreDecls -> Decl ZeroOrMoreDecls"
        ( \rhs -> let decl  = fromASTDecl (get rhs 1)
                      decls = fromASTDecls (get rhs 2)
                  in  return $ toASTDecls (decl : decls) ),
		  
      -- | Decl : Decl
      rule "Decl -> device identifier : identifier ;"  -- device name : capability
        ( \rhs -> let name = getText rhs 2
	              capability = getText rhs 4
		  in  return $ toASTDecl ( DeviceDecl name capability )
	),

      rule "Decl -> input identifier : identifier ;"  -- input name : value type
        ( \rhs -> let name = getText rhs 2
	              valuetype = getText rhs 4
		  in  return $ toASTDecl ( InputDecl name valuetype )
	),

      rule "Decl -> output identifier : identifier ;"  -- input name : value type
        ( \rhs -> let name = getText rhs 2
	              valuetype = getText rhs 4
		  in  return $ toASTDecl ( OutputDecl name [ valuetype ] )
        ),

      rule "Decl -> output identifier : ( OneOrMoreIdentifiers ) ;"  -- output name : value types
        ( \rhs -> let name = getText rhs 2
	              valuetypes = fromASTValueTypes ( get rhs 5 )
		  in  return $ toASTDecl ( OutputDecl name valuetypes )
        ),

      -- | OneOrMoreIdentifiers : [ ValueType ]
      rule "OneOrMoreIdentifiers -> identifier"
        ( \rhs -> let valueType = getText rhs 1
                  in  return $ toASTValueTypes [ valueType ] ),

      rule "OneOrMoreIdentifiers -> identifier , OneOrMoreIdentifiers"
        ( \rhs -> let valueType = getText rhs 1
	              valueTypes = fromASTValueTypes (get rhs 3)
                  in  return $ toASTValueTypes (valueType : valueTypes)
        ),

      -- | EventHandler : EventHandler
      rule "EventHandler -> FieldOrTimer [ . ~> ]"
        ( \rhs -> let fieldOrTimer = fromASTExpression (get rhs 1)
	          in  return $ toASTEventHandler $ JustEvent fieldOrTimer
	),
      
      rule "EventHandler -> FieldOrTimer [ . ~> Constant ]"
        ( \rhs -> let fieldOrTimer = fromASTExpression (get rhs 1)
	              constant = fromASTLiteral (get rhs 5)
		  in  return $ toASTEventHandler $ EventTo fieldOrTimer constant
	),
      
      rule "EventHandler -> FieldOrTimer [ Constant ~> ]"
        ( \rhs -> let fieldOrTimer = fromASTExpression (get rhs 1)
	              constant = fromASTLiteral (get rhs 3)
		  in  return $ toASTEventHandler $ EventFrom fieldOrTimer constant
	),
      
      rule "EventHandler -> FieldOrTimer [ Constant ~> Constant ]"
        ( \rhs -> let fieldOrTimer = fromASTExpression (get rhs 1)
	              constantFrom = fromASTLiteral (get rhs 3)
	              constantTo = fromASTLiteral (get rhs 5)
		  in  return $ toASTEventHandler $ EventFromTo fieldOrTimer constantFrom constantTo
	),
      
      rule "EventHandler -> any Group ( identifier -> EventHandler )"
        ( \rhs -> let group = fromASTGroup (get rhs 2)
	              varName = getText rhs 4
		      eventHandler = fromASTEventHandler (get rhs 6)
		  in  return $ toASTEventHandler $ GroupEvent group varName eventHandler
	),

      -- | OneOrMorePredicateActions : [ ( Predicate, Actions ) ]
      rule "OneOrMorePredicateActions -> Predicate ; Actions"      -- Extension
        ( \rhs -> let pred    = fromASTPredicate (get rhs 1)
	              actions = fromASTActions (get rhs 3)
		  in  return $ toASTMultiplePredicateActions $ [ ( pred, actions ) ]
	),

      rule "OneOrMorePredicateActions -> Predicate ; Actions | OneOrMorePredicateActions"
        ( \rhs -> let pred = fromASTPredicate ( get rhs 1 )
	              actions = fromASTActions ( get rhs 3 )
		      theRest = fromASTMultiplePredicateActions ( get rhs 5 )
		  in  return $ toASTMultiplePredicateActions $ ( pred, actions) : theRest
	),
      
      -- | Predicate : Predicate
      rule "Predicate -> OrPred"
        ( \rhs -> return $ get rhs 1 ),
      
      rule "Predicate -> all Group ( identifier -> Predicate )"
        ( \rhs -> let group = fromASTGroup (get rhs 2)
	              varName = getText rhs 4
		      pred = fromASTPredicate (get rhs 6)
		  in  return $ toASTPredicate $ Forall group varName pred
	),
      
      rule "Predicate -> exists Group ( identifier -> Predicate )"
        ( \rhs -> let group = fromASTGroup (get rhs 2)
	              varName = getText rhs 4
		      pred = fromASTPredicate (get rhs 6)
		  in  return $ toASTPredicate $ Exists group varName pred
	),
      
      -- ruleWithNoAction "Predicate -> FieldOrTimer is in Group",
      
      -- | OrPred : Predicate
      rule "OrPred -> OrPred || AndPred"
        ( \rhs -> let pred1 = fromASTPredicate (get rhs 1)
	              pred2 = fromASTPredicate (get rhs 3)
		  in  return $ toASTPredicate $ LogicalOr pred1 pred2
	),
      
      rule "OrPred -> AndPred"
        ( \rhs -> return $ get rhs 1 ),
      
      -- | AndPred : Predicate
      rule "AndPred -> AndPred && EqNeqPred"
        ( \rhs -> let pred1 = fromASTPredicate (get rhs 1)
	              pred2 = fromASTPredicate (get rhs 3)
		  in  return $ toASTPredicate $ LogicalAnd pred1 pred2
	),
      
      rule "AndPred -> EqNeqPred"
        ( \rhs -> return $ get rhs 1 ),
      
      -- | EqNeqPred : Predicate
      rule "EqNeqPred -> EqNeqPred == CompExpr"
        ( \rhs -> let pred1 = fromASTPredicate (get rhs 1)
	              pred2 = fromASTPredicate (get rhs 3)
		  in  return $ toASTPredicate $ IsEqual pred1 pred2
	),
      
      rule "EqNeqPred -> EqNeqPred != CompExpr"
        ( \rhs -> let expr1 = fromASTPredicate (get rhs 1)
	              expr2 = fromASTPredicate (get rhs 3)
		  in  return $ toASTPredicate $ IsInequal expr1 expr2
	),
      
      rule "EqNeqPred -> CompExpr"
        ( \rhs -> return $ get rhs 1 ),
      
      -- | CompExpr : Predicate
      rule "CompExpr -> CompExpr < AdditiveExpr"
        ( \rhs -> let expr1 = fromASTPredicate $ get rhs 1
	              expr2 = fromASTExpression $ get rhs 3
		  in  return $ toASTPredicate $ LessThan expr1 expr2
	),
      
      rule "CompExpr -> CompExpr <= AdditiveExpr"
        ( \rhs -> let expr1 = fromASTPredicate $ get rhs 1
	              expr2 = fromASTExpression $ get rhs 3
		  in  return $ toASTPredicate $ LessThanOrEqualTo expr1 expr2
	),
      
      rule "CompExpr -> CompExpr > AdditiveExpr"
        ( \rhs -> let expr1 = fromASTPredicate $ get rhs 1
	              expr2 = fromASTExpression $ get rhs 3
		  in  return $ toASTPredicate $ GreaterThan expr1 expr2
	),
      
      rule "CompExpr -> CompExpr >= AdditiveExpr"
        ( \rhs -> let expr1 = fromASTPredicate $ get rhs 1
	              expr2 = fromASTExpression $ get rhs 3
		  in  return $ toASTPredicate $ GreaterThanOrEqualTo expr1 expr2
	),
      
      rule "CompExpr -> AdditiveExpr"
        ( \rhs -> let expr = fromASTExpression $ get rhs 1
	          in  return $ toASTPredicate $ ExpressionPredicate expr
        ), 
      
      -- | AdditiveExpr : Expression
      rule "AdditiveExpr -> AdditiveExpr + MultiplicativeExpr"
        ( \rhs -> let expr1 = fromASTExpression $ get rhs 1
	              expr2 = fromASTExpression $ get rhs 3
		  in  return $ toASTExpression $ Addition expr1 expr2
	),
      
      rule "AdditiveExpr -> AdditiveExpr - MultiplicativeExpr"
        ( \rhs -> let expr1 = fromASTExpression $ get rhs 1
	              expr2 = fromASTExpression $ get rhs 3
		  in  return $ toASTExpression $ Subtraction expr1 expr2
	),
      
      rule "AdditiveExpr -> MultiplicativeExpr"
        ( \rhs -> return $ get rhs 1 ),
      
      -- | MultiplicativeExpr : Expression
      rule "MultiplicativeExpr -> MultiplicativeExpr * UnaryExpr"
        ( \rhs -> let expr1 = fromASTExpression $ get rhs 1
	              expr2 = fromASTExpression $ get rhs 3
		  in  return $ toASTExpression $ Multiplication expr1 expr2
	),
      
      rule "MultiplicativeExpr -> MultiplicativeExpr / UnaryExpr"
        ( \rhs -> let expr1 = fromASTExpression $ get rhs 1
	              expr2 = fromASTExpression $ get rhs 3
		  in  return $ toASTExpression $ Division expr1 expr2
	),
      
      rule "MultiplicativeExpr -> UnaryExpr"
        ( \rhs -> return $ get rhs 1 ),
      
      -- | UnaryExpr : Expression
      rule "UnaryExpr -> - PrimaryExpr"
        ( \rhs -> let expr = fromASTExpression $ get rhs 2
	          in  return $ toASTExpression $ MinusSign expr
	),
      
      rule "UnaryExpr -> ~ PrimaryExpr"
        ( \rhs -> let expr = fromASTExpression $ get rhs 2
	          in  return $ toASTExpression $ Negate expr
	),
      
      rule "UnaryExpr -> PrimaryExpr"
        ( \rhs -> return $ get rhs 1 ),
      
      -- | PrimayExpr : Expression
      rule "PrimaryExpr -> true"
        ( \rhs -> return $ toASTExpression $ LiteralExpression $ BoolLiteral True ),
      
      rule "PrimaryExpr -> false"
        ( \rhs -> return $ toASTExpression $ LiteralExpression $ BoolLiteral False ),
      
      rule "PrimaryExpr -> number_literal"
        ( \rhs -> let number = (read $ getText rhs $1) :: Integer
	          in  return $ toASTExpression $ LiteralExpression $ NumberLiteral $ number ),

      rule "PrimaryExpr -> string_literal"       -- extension
        ( \rhs -> let str = getText rhs $1
	          in  return $ toASTExpression $ LiteralExpression $ StringLiteral $ str ),

      -------------------------------------------------------------------------------------
      -- Note: It could be a constant, a timer, or an input variable (identifier).
      --       A typechecker could discern it.
      --       In the parser, it pretends to be an identifier.
      -------------------------------------------------------------------------------------
      rule "PrimaryExpr -> identifier"  
        ( \rhs -> return $ toASTExpression $ IdentifierExpression $ getText rhs 1 ),

      rule "PrimaryExpr -> identifier . identifier"
        ( \rhs -> let device = getText rhs 1
	              field = getText rhs 3
		  in  return $ toASTExpression $ Field device field ),

      rule "PrimaryExpr -> ( Predicate )"
        ( \rhs -> let pred = fromASTPredicate $ get rhs 2
	          in  return $ toASTExpression $ PredicateExpression $ pred ),

      -- | Constant : String
      rule "Constant -> identifier"
        ( \rhs -> return $ toASTLiteral (ConstantLiteral (getText rhs 1)) ),
      
      rule "Constant -> number_literal"
        ( \rhs -> return $ toASTLiteral (NumberLiteral (read (getText rhs 1) :: Integer)) ),
      
      -- | Actions : [ Action ]
      rule "Actions -> "
        ( \rhs -> return $ toASTActions [] ),
      
      rule "Actions -> OneOrMoreActions"
        ( \rhs -> return $ get rhs 1 ),
      
      rule "OneOrMoreActions -> Action"
        ( \rhs -> return $ toASTActions $ [ fromASTAction $ get rhs 1 ] ),

      rule "OneOrMoreActions -> Action , OneOrMoreActions"
        ( \rhs -> let action = fromASTAction $ get rhs 1
	              actions = fromASTActions $ get rhs 3
		  in  return $ toASTActions (action : actions) ),

      -- | Action : Action
      rule "Action -> FieldOrTimer := AdditiveExpr"
        ( \rhs -> let fieldOrTimer = fromASTExpression (get rhs 1)
	              expr = fromASTExpression (get rhs 3)
		  in  return $ toASTAction $ CommandAction fieldOrTimer expr
	),

      rule "Action -> identifier ( OneOrMoreAdditiveExprs )"
        ( \rhs -> let name  = getText rhs 1
	              exprs = fromASTExpressions (get rhs 3)
		  in  return $ toASTAction $ OutputAction name exprs
	),
      
      rule "Action -> start identifier at AdditiveExpr"
        ( \rhs -> let name = getText rhs 2
	              expr = fromASTExpression $ get rhs 4
	          in  return $ toASTAction $ StartTimer name expr
	), 
      
      rule "Action -> stop identifier"
        ( \rhs -> let name = getText rhs 2
	          in  return $ toASTAction $ StopTimer name
	), 

      rule "Action -> map Group ( identifier -> Action )"
        ( \rhs -> let group = fromASTGroup $ get rhs 2
	              varName = getText rhs 4
		      action = fromASTAction $ get rhs 6
		  in  return $ toASTAction $ MapAction group varName action
	),

      -- | OneOrMoreAdditiveExprs : [ Expressions ]
      rule "OneOrMoreAdditiveExprs -> AdditiveExpr"
        ( \rhs -> let expr = fromASTExpression $ get rhs 1
	          in  return $ toASTExpressions $ [ expr ]
	),

      rule "OneOrMoreAdditiveExprs -> AdditiveExpr , OneOrMoreAdditiveExprs"
        ( \rhs -> let expr = fromASTExpression $ get rhs 1
	              exprs = fromASTExpressions $ get rhs 3
	          in  return $ toASTExpressions $ expr : exprs
	),

      -- | FieldOrTimer :: Expression
      rule "FieldOrTimer -> identifier"  -- timer
        ( \rhs -> return $ toASTExpression $ Timer $ getText rhs 1	),

      rule "FieldOrTimer -> identifier . identifier"  -- field
        ( \rhs -> let device = getText rhs 1
	              field = getText rhs 3
		  in  return $ toASTExpression $ Field device field ),

      -- | Group :  [ DeviceName ]
      rule "Group -> identifier"
        ( \rhs -> return $ toASTGroup $ [ getText rhs 1 ] ),

      rule "Group -> { zero_or_more_device_identifiers }"
        ( \rhs -> return $ get rhs 2 ),

      -- | zero_or_more_device_identifiers  :  [ DeviceName ]
      rule "zero_or_more_device_identifiers -> "
        ( \rhs -> return $ toASTGroup $ [ ] ),

      rule "zero_or_more_device_identifiers -> one_or_more_device_identifiers"
        ( \rhs -> return $ get rhs 1 ),

      rule "one_or_more_device_identifiers -> identifier"
        ( \rhs -> return $ toASTGroup $ [ getText rhs 1 ] ),

      rule "one_or_more_device_identifiers -> identifier , one_or_more_device_identifiers"
        ( \rhs -> let name = getText rhs 1
	              devNames = fromASTGroup $ get rhs 3
		  in  return $ toASTGroup $ name : devNames
        )

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
