module AST where

import Expr

instance Show AST where
  showsPrec p _ = (++) "AST ..."

data AST =
   ASTRule { fromASTRule :: Rule }
 | ASTRules { fromASTRules :: [ Rule ] }
 | ASTDecl { fromASTDecl :: Decl }
 | ASTDecls { fromASTDecls :: [ Decl ] }
 | ASTValueTypes { fromASTValueTypes :: [ ValueType ] }
 | ASTEMCA { fromASTEMCA :: [ EMCA ] }
 | ASTEventHandler { fromASTEventHandler :: EventHandler }
 | ASTMultiplePredicateActions { fromASTMultiplePredicateActions :: MultiplePredicateActions }
 | ASTPredicate { fromASTPredicate :: Predicate }
 | ASTAction { fromASTAction :: Action }
 | ASTActions { fromASTActions :: Actions }
 | ASTExpression { fromASTExpression :: Expression }
 | ASTExpressions { fromASTExpressions :: [ Expression ] }
 | ASTLiteral { fromASTLiteral :: Literal }
 | ASTConstant { fromASTConstant :: Literal }
 | ASTGroup { fromASTGroup :: Group }

toASTRule rule = ASTRule rule
toASTRules rules = ASTRules rules
toASTDecl decl = ASTDecl decl
toASTDecls decls = ASTDecls decls
toASTValueTypes valueTypes = ASTValueTypes valueTypes
toASTEMCA emcas = ASTEMCA emcas
toASTEventHandler eventhandler = ASTEventHandler eventhandler
toASTMultiplePredicateActions mpas = ASTMultiplePredicateActions mpas
toASTPredicate p = ASTPredicate p
toASTActions actions = ASTActions actions
toASTExpression expression = ASTExpression expression
toASTExpressions expressions = ASTExpressions expressions
toASTLiteral literal = ASTLiteral literal
toASTConstant constant = ASTConstant constant
toASTAction action = ASTAction action
toASTGroup group = ASTGroup group

