module Expr where

data Expr = NoExpr
  deriving Show

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

type Rules = [ Rule ]

data Decl =
    DeviceDecl Name Capability
  | InputDecl Name ValueType
  | OutputDecl Name [ ValueType ]

type Decls = [ Decl ]

data EMCA = 
    EMCA EventHandler MultiplePredicateActions

type BoundVariable = String

type EventConstant = String

data EventHandler =
    JustEvent                                      -- [ . ~> ]
  | EventTo EventConstant                          -- [ . ~> Constant ]
  | EventFrom EventConstant                        -- [ Constant ~> ]
  | Event EventConstant EventConstant                 -- [ Constant ~> Constant' ]
  | GroupEvent Group BoundVariable EventHandler    -- any group ( x -> predicate )

type MultiplePredicateActions = [ ( Predicate, Actions ) ]

data Predicate =
    Forall Group BoundVariable Predicate
  | Exists Group BoundVariable Predicate
  | LogicalOr Predicate Predicate
  | LogicalAnd Predicate Predicate
  | IsEqual Expression Expression
  | IsInequal Expression Expression
  | Negate Predicate
  | LessThan Expression Expression
  | LessThanOrEqal Expression Expression
  | GreaterThan Expression Expression
  | GreaterThanOrEqualTo Expression Expression
  | ExpressionPredicate Expression

data Expression =
    Addition Expression Expression
  | Subtraction Expression Expression
  | Multiplication Expression Expression
  | Division Expression Expression
  | MinusSign Expression
  | LiteralExpression Literal
  | Field DeviceName AttributeName
  | Timer TimerName

data Literal =
    BoolLiteral Bool
  | NumberLiteral Integer
  | StringLiteral String

type Actions = [ Action ]

type FieldOrTimer = Expression  --   Field DeviceName AttributeName 
                                -- | Timer TimeName

data Action =
    CommandAction FieldOrTimer Expression
  | OutputAction FieldOrTimer
  | StartTimer TimerName Expression
  | StopTimer TimerName
  | MapAction Group BoundVariable Action

type Group = [ DeviceName ]

