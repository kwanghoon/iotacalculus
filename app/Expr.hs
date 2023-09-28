{-# LANGUAGE DeriveGeneric #-}

module Expr where

import Data.Aeson
import Data.Aeson.Key (fromString)
import Data.Aeson.Encode.Pretty (encodePretty)

import GHC.Generics ( Generic )
import qualified Data.ByteString.Lazy.Char8 as B


--
type Description = String

type Name = String

type DeviceName = Name

type AttributeName = Name

type TimerName = Name

type Capability = String

type ValueType = String

toJson :: Rule -> B.ByteString
toJson = encodePretty

data Rule =
    NodeRule Description Decls Rules
  | LeafRule Description Decls EMCA
  deriving (Show, Generic)

instance FromJSON Rule

instance ToJSON Rule where
  toJSON (NodeRule d ds rs) = object [fromString "NodeRule" .= (d, ds, rs)]
  toJSON (LeafRule d ds e) = object [fromString "LeafRule" .= (d, ds, e)]

type Rules = [ Rule ]

data Decl =
    DeviceDecl Name Capability
  | InputDecl Name ValueType
  | OutputDecl Name [ ValueType ]
  deriving (Show, Generic)

instance FromJSON Decl

instance ToJSON Decl where
  toJSON (DeviceDecl n c) = object [fromString "DeviceDecl" .= (n, c)]
  toJSON (InputDecl n vt) = object [fromString "InputDecl" .= (n, vt)]
  toJSON (OutputDecl n vts) = object [fromString "OutputDecl" .= (n, vts)]

type Decls = [ Decl ]

data EMCA = 
    EMCA EventHandler MultiplePredicateActions
  deriving (Show, Generic, Ord, Eq)

eventHandler (EMCA evH _) = evH  

instance FromJSON EMCA

instance ToJSON EMCA where
  toJSON (EMCA e mpa) = object [fromString "EMCA" .= (e, mpa)]

type BoundVariable = String

type EventConstant = String

data EventHandler =
    JustEvent FieldOrTimer                          -- fOrM [ . ~> ]
  | EventTo FieldOrTimer EventConstant              -- fOrM [ . ~> Constant ]
  | EventFrom FieldOrTimer EventConstant            -- fOrM [ Constant ~> ]
  | EventFromTo FieldOrTimer EventConstant EventConstant  -- fOrM [ Constant ~> Constant' ]
  | GroupEvent Group BoundVariable EventHandler     -- any group ( x -> predicate )
  deriving (Show, Generic, Ord, Eq)

instance FromJSON EventHandler

instance ToJSON EventHandler where
  toJSON (JustEvent f) = object [fromString "JustEvent" .= f]
  toJSON (EventTo f c) = object [fromString "EventTo" .= (f, c)]
  toJSON (EventFrom f c) = object [fromString "EventFrom" .= (f, c)]
  toJSON (EventFromTo f c c') = object [fromString "EventFromTo" .= (f, c, c')]
  toJSON (GroupEvent g b e) = object [fromString "GroupEvent" .= (g, b, e)]

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
  deriving (Show, Generic, Ord, Eq)

instance FromJSON Predicate

instance ToJSON Predicate where
  toJSON (Forall g b p) = object [fromString "Forall" .= (g, b, p)]
  toJSON (Exists g b p) = object [fromString "Exists" .= (g, b, p)]
  toJSON (LogicalOr p1 p2) = object [fromString "LogicalOr" .= (p1, p2)]
  toJSON (LogicalAnd p1 p2) = object [fromString "LogicalAnd" .= (p1, p2)]
  toJSON (IsEqual p1 p2) = object [fromString "IsEqual" .= (p1, p2)]
  toJSON (IsInequal p1 p2) = object [fromString "IsInequal" .= (p1, p2)]
  toJSON (LessThan p e) = object [fromString "LessThan" .= (p, e)]
  toJSON (LessThanOrEqualTo p e) = object [fromString "LessThanOrEqualTo" .= (p, e)]
  toJSON (GreaterThan p e) = object [fromString "GreaterThan" .=(p, e)]
  toJSON (GreaterThanOrEqualTo p e) = object [fromString "GreaterThanOrEqualTo" .= (p, e)]
  toJSON (ExpressionPredicate e) = object [fromString "ExpressionPredicate" .= e]

data Expression =
    Addition Expression Expression
  | Subtraction Expression Expression
  | Multiplication Expression Expression
  | Division Expression Expression
  | MinusSign Expression
  | Negate Expression
  | LiteralExpression Literal
  | IdentifierExpression String
  | Field DeviceName AttributeName
  | Timer TimerName
  | PredicateExpression Predicate
  deriving (Show, Generic, Ord, Eq)

instance FromJSON Expression

instance ToJSON Expression where
  toJSON (Addition e1 e2) = object [fromString "Addition" .= (e1, e2)]
  toJSON (Subtraction e1 e2) = object [fromString "Subtraction" .= (e1, e2)]
  toJSON (Multiplication e1 e2) = object [fromString "Multiplication" .= (e1, e2)]
  toJSON (Division e1 e2) = object [fromString "Division" .= (e1, e2)]
  toJSON (MinusSign e) = object [fromString "MinusSign" .= e]
  toJSON (Negate e) = object [fromString "Negate" .= e]
  toJSON (LiteralExpression l) = object [fromString "LiteralExpression" .= l]
  toJSON (IdentifierExpression s) = object [fromString "IdentifierExpression" .= s]
  toJSON (Field d a) = object [fromString "Field" .= (d, a)]
  toJSON (Timer t) = object [fromString "Timer" .= t]
  toJSON (PredicateExpression p) = object [fromString "PredicateExpression" .= p]

data Literal =
    BoolLiteral Bool
  | NumberLiteral Integer
  | StringLiteral String
  | ConstantLiteral String
  deriving (Show, Generic, Ord, Eq)

instance FromJSON Literal

instance ToJSON Literal where
  toJSON (BoolLiteral b) = object [fromString "BoolLiteral" .= b]
  toJSON (NumberLiteral n) = object [fromString "NumberLiteral" .= n]
  toJSON (StringLiteral s) = object [fromString "StringLiteral" .= s]
  toJSON (ConstantLiteral c) = object [fromString "ConstantLiteral" .= c]

isTrueLiteral :: Literal -> Bool
isTrueLiteral (BoolLiteral True) = True
isTrueLiteral _ = False

-- data Value =  -- | Runtime value
--     BoolValue Bool
--   | NumberValue Integer
--   | StringValue String
--   | ConstantValue String
--   deriving (Show, Generic)

type Actions = [ Action ]

type FieldOrTimer = Expression  --   Field DeviceName AttributeName 
                                -- | Timer TimeName

data Action =
    CommandAction FieldOrTimer Expression
  | OutputAction Name [ Expression ]
  | StartTimer TimerName Expression
  | StopTimer TimerName
  | MapAction Group BoundVariable Action
  deriving (Show, Generic, Ord, Eq)

instance FromJSON Action

instance ToJSON Action where
  toJSON (CommandAction f e) = object [fromString "CommandAction" .= (f, e)]
  toJSON (OutputAction n es) = object [fromString "OutputAction" .= (n, es)]
  toJSON (StartTimer t e) = object [fromString "StartTimer" .= (t, e)]
  toJSON (StopTimer t) = object [fromString "StopTimer" .= t]
  toJSON (MapAction g b a) = object [fromString "MapAction" .= (g, b, a)]

type Group = [ DeviceName ]

