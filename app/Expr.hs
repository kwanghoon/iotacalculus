{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

module Expr where

import Data.Char
import Data.List (lookup)
import Data.Text.Prettyprint.Doc hiding (Pretty)
import Data.Text.Prettyprint.Doc.Util

import Text.JSON.Generic


--
data Expr = NoExpr
  deriving (Show, Typeable, Data)

type Description = String

type Name = String

type DeviceName = Name

type AttributeName = Name

type TimerName = Name

type Capability = String

type ValueType = String

data Rule =
    NodeRule Description Decls Rules
  | LeafRule Description Decls EMCA
  deriving (Show, Typeable, Data)

type Rules = [ Rule ]

data Decl =
    DeviceDecl Name Capability
  | InputDecl Name ValueType
  | OutputDecl Name [ ValueType ]
  deriving (Show, Typeable, Data)

type Decls = [ Decl ]

data EMCA = 
    EMCA EventHandler MultiplePredicateActions
  deriving (Show, Typeable, Data)

type BoundVariable = String

type EventConstant = String

data EventHandler =
    JustEvent FieldOrTimer                          -- fOrM [ . ~> ]
  | EventTo FieldOrTimer EventConstant              -- fOrM [ . ~> Constant ]
  | EventFrom FieldOrTimer EventConstant            -- fOrM [ Constant ~> ]
  | Event FieldOrTimer EventConstant EventConstant  -- fOrM [ Constant ~> Constant' ]
  | GroupEvent Group BoundVariable EventHandler     -- any group ( x -> predicate )
  deriving (Show, Typeable, Data)

type MultiplePredicateActions = [ ( Predicate, Actions ) ]

data Predicate =
    Forall Group BoundVariable Predicate
  | Exists Group BoundVariable Predicate
  | LogicalOr Predicate Predicate
  | LogicalAnd Predicate Predicate
  | IsEqual Predicate Predicate  -- may look strange
  | IsInequal Predicate Predicate  -- may look strange
  | LessThan Predicate Expression -- may look strange
  | LessThanOrEqualTo Predicate Expression -- may look strange
  | GreaterThan Predicate Expression -- may look strange
  | GreaterThanOrEqualTo Predicate Expression -- may look strange
  | ExpressionPredicate Expression
  deriving (Show, Typeable, Data)

data Expression =
    Addition Expression Expression
  | Subtraction Expression Expression
  | Multiplication Expression Expression
  | Division Expression Expression
  | MinusSign Expression
  | Negate Expression
  | LiteralExpression Literal
  | Field DeviceName AttributeName
  | Timer TimerName
  | PredicateExpression Predicate
  deriving (Show, Typeable, Data)

data Literal =
    BoolLiteral Bool
  | NumberLiteral Integer
  | StringLiteral String
  | ConstantLiteral String
  deriving (Show, Typeable, Data)

type Actions = [ Action ]

type FieldOrTimer = Expression  --   Field DeviceName AttributeName 
                                -- | Timer TimeName

data Action =
    CommandAction FieldOrTimer Expression
  | OutputAction Name [ Expression ]
  | StartTimer TimerName Expression
  | StopTimer TimerName
  | MapAction Group BoundVariable Action
  deriving (Show, Typeable, Data)

type Group = [ DeviceName ]

