module Interp where

import qualified Data.Set   as Set
import qualified Data.Map   as Map
import qualified Data.Maybe as Maybe

import Expr
import Data.Functor.Contravariant (Predicate(Predicate))

data Event =
    EventField DeviceName AttributeName EventConstant EventConstant
  | EventTimer TimerName EventConstant EventConstant
  deriving (Eq, Ord)

data State = Map Name Literal

-- | Environments
type Environment = Map.Map Name DeclValue

data DeclValue =
   Device Capability
 | Input  ValueType
 | Output [ ValueType ]

type RuleClosure = (Environment, EMCA)

eventHandler :: RuleClosure -> EventHandler
eventHandler (_, emca) = Expr.eventHandler emca

-- | Rule sets
type Ruleset = Map.Map Integer RuleClosure

data EvalState =
   NoneES  Ruleset
 | EventES Ruleset Ruleset  -- (In, Out)
 | PredES  Ruleset Ruleset  -- (In, Out)
 | ActES   Ruleset Ruleset  -- (In, Out)

{-

-- | (R-E)

evalREvent :: Set.Set Event -> Set.Set State -> Ruleset ->
 IO (Maybe.Maybe Event, Set.Set Event, Set.Set State, EvalState)

-- | NoneES ruleset
evalREvent eventSet stateSet ruleset
 | Map.null ruleset = 
    return (Maybe.Nothing, eventSet, stateSet, NoneES ruleset)

 | otherwise =
    do eEsList <- chooseOne (Set.toList eventSet)
       let (e, esList) = head eEsList -- Just pick the first one!
       let es = Set.fromList esList
       return (Maybe.Just e, es, stateSet, EventES ruleset Map.empty)


-- | (R-HP) and (R-H) with EventES rulesetIn rulsetOut

evalRHandleEvent 
  :: Event -> Set.Set Event -> Set.Set State -> Ruleset -> Ruleset 
      -> IO (Event, Set.Set Event, Set.Set State, EvalState)
evalRHandleEvent event eventSet stateSet rulesetIn rulesetOut

 | Map.null rulesetIn =
     return (event, eventSet, stateSet, PredES rulesetOut Map.empty)

 | otherwise =
     do rRsList <- chooseOne (Map.assocs rulesetIn)
        let ((idx,r), rsList) = head rRsList -- Just pick the first one!
        let rs1 = Map.fromList rsList
        rs <- handleRule idx r (eventHandler r) event
        return (event, eventSet, stateSet, EventES rs1 (Map.union rs rulesetOut))

-- | (R-P) and (R-PA) with PredES rulesetIn rulesetOut

evalRPredicate 
  :: Event -> Set.Set Event -> Set.Set State -> Map.Map Integer RuleClosure -> Ruleset 
      -> IO (Event, Set.Set Evnet, State, EvalState)
evalRPredicate event eventSet stateSet rulesetIn rulesetOut

  | Map.null rulesetIn =
      return (event, eventSet, stateSet, ActES rulesetOut Map.empty)

  | otherwise =
      do rRsList <- chooseOne (Map.assocs rulesetIn)
         let ((idx,r), rsList) = head rRsList
         let rs1 = Map.fromList rsList
         rs <- evalPredicate idx r stateSet
         return (event, eventSet, stateSet, PredES rs1 (Map.union rs rulesetOut))

-- | (R-A) and (R-AE) with PredES rulesetIn rulesetOut

evalRAction 
  :: Event -> Set.Set Event -> State -> Map.Map Integer RuleClosure -> Ruleset 
      -> IO (Maybe a, Set.Set Event, State, EvalState)
evalRAction event eventSet stateSet rulesetIn rulesetOut

   | Map.null rulesetIn =
       return (Maybe.Nothing, eventSet, stateSet, NoneES rulesetOut)

   | otherwise =
       do rRsList <- chooseOne (Map.assocs rulesetIn)
          let ((i,r), rsList) = head rRsList
          let rs0 = Map.fromList [(i,r)]
          let rs1 = Map.fromList rsList
          (stateSet', eventSet') <- evalAction r stateSet
          return (Maybe.Just event, Set.union eventSet eventSet', stateSet',
                                          ActES rs1 (Map.union rs0 rulesetOut))


-- | [[ r ]]_h e = rs
-- |   Note that the 3rd argument is one extracted from the 2nd argument.
handleRule :: Integer -> RuleClosure -> EventHandler -> Event -> IO Ruleset

-- for JustEvent
handleRule idx rc (JustEvent (Field devName1 fieldName1)) 
                  (EventField devName2 fieldName2 _ _)
  | devName1 == devName2 && fieldName1 == fieldName2
    = return $ Map.fromList [ (idx, rc)]
  | otherwise = return Map.empty

handleRule idx rc (JustEvent (Timer timerName)) 
                  (EventTimer timerName' _ _)
  | timerName == timerName' = return $ Map.fromList [ (idx, rc)]
  | otherwise = return Map.empty

-- for EventTo
handleRule idx rc (EventTo (Field devName1 fieldName1) eventConstant1)
                  (EventField devName2 fieldName2 _ eventConstant2)
  | devName1 == devName2 && fieldName1 == fieldName2 && eventConstant1 == eventConstant2
    = return $ Map.fromList [ (idx, rc)]
  | otherwise = return Map.empty

handleRule idx rc (EventTo (Timer timerName1) eventConstant1)
                  (EventTimer timerName2 _ eventConstant2)
  | timerName1 == timerName2 && eventConstant1 == eventConstant2
    = return $ Map.fromList [ (idx, rc)]
  | otherwise = return Map.empty

-- for EventFrom
handleRule idx rc (EventFrom (Field devName1 fieldName1) eventConstant1)
                  (EventField devName2 fieldName2 eventConstant2 _)
  | devName1 == devName2 && fieldName1 == fieldName2 && eventConstant1 == eventConstant2
    = return $ Map.fromList [ (idx, rc)]
  | otherwise = return Map.empty

handleRule idx rc (EventFrom (Timer timerName1) eventConstant1)
                  (EventTimer timerName2 eventConstant2 _)
  | timerName1 == timerName2 && eventConstant1 == eventConstant2
    = return $ Map.fromList [ (idx, rc)]
  | otherwise = return Map.empty

-- for EventFromTo
handleRule idx rc (EventFromTo (Field devName1 fieldName1) eventConstant1 eventConstant1')
                  (EventField devName2 fieldName2 eventConstant2 eventConstant2')
  | devName1 == devName2 && fieldName1 == fieldName2 
    && eventConstant1 == eventConstant2 && eventConstant1' == eventConstant2'
    = return $ Map.fromList [ (idx, rc)]
  | otherwise = return Map.empty

handleRule idx rc (EventFromTo (Timer timerName1) eventConstant1 eventConstant1')
                  (EventTimer timerName2 eventConstant2 eventConstant2')
  | timerName1 == timerName2 && eventConstant1 == eventConstant2 
    && eventConstant1' == eventConstant2'
    = return $ Map.fromList [ (idx, rc)]
  | otherwise = return Map.empty

-- forGroupEvent
handleRule _ (_, EMCA (GroupEvent {}) _) _ _ = error "handleRule over GroupEvent: not implemented"

handleRule _ _ evH event = error $ "handleRule: " ++ show evH ++ " over " ++ show event


-- | [[ r ]]_p sigma = rs
evalPredicate :: Integer -> RuleClosure -> State -> IO Ruleset
evalPredicate idx rc@(env, EMCA eh mpas) state = 
  case map (\ (p, _) -> satisfied p) mpas of
    [] -> return Map.empty
    bList -> case [ pas | (b,pas) <- zip bList mpas, b] of
                [] -> return Map.empty
                (pas:_) -> return (Map.fromList [(idx, (env, EMCA eh [pas]))])
  where
    satisfied :: Expr.Predicate -> Expr.Literal
    satisfied (LogicalOr p1 p2) = satisfied p1 || satisfied p2
    satisfied (LogicalAnd p1 p2) = satisfied p1 && satisfied p2
    satisfied (IsEqual p1 p2) = satisfied p1 == satisfied p2
    satisfied (IsInequal p1 p2) = satisfied p1 /= satisfied p2
    satisfied (LessThan p e) = satisfied p < eval e
    satisfied (LessThanOrEqualTo p e) = satisfied p <= eval e
    satisfied (GreaterThan p e) = satisfied p > eval e
    satisfied (GreaterThanOrEqualTo p e) = satisfied p >= eval e
    satisfied (ExpressionPredicate e) = eval e
    satisfied _ = error "Unexpected predicate in satisfied"
    -- satisfied (Forall g b p) = all (\ x -> satisfied (substitute b x p)) (group g) 

    eval :: Expr.Expression -> Expr.Literal
    eval (Addition e1 e2) = eval e1 `addition` eval e2
    eval (Subtraction e1 e2) = eval e1 `subtraction` eval e2
    eval (Multiplication e1 e2) = eval e1 `multiplication` eval e2
    eval (Division e1 e2) = eval e1 `division` eval e2
    eval (MinusSign e) = minusSign (eval e)
    eval (Negate e) = Interp.negate (eval e)
    eval (LiteralExpression l) = l
    eval (IdentifierExpression s) = evalIdentifier s
    eval (Field devName fieldName) = evalField devName fieldName
    eval (Timer timerName) = evalTimer timerName
    eval (PredicateExpression p) = if satisfied p then 1 else 0


addition :: Expr.Literal -> Expr.Literal -> Expr.Literal
addition (NumberLiteral n1) (NumberLiteral n2) = NumberLiteral (n1 + n2)
addition e1 e2 = error $ "Unexpected operands in addition: " ++ show e1 ++ " " ++ show e2

subtraction :: Expr.Literal -> Expr.Literal -> Expr.Literal
subtraction (NumberLiteral n1) (NumberLiteral n2) = NumberLiteral (n1 - n2)
subtraction e1 e2 = error $ "Unexpected operands in subtraction: " ++ show e1 ++ " " ++ show e2

multiplication :: Expr.Literal -> Expr.Literal -> Expr.Literal
multiplication (NumberLiteral n1) (NumberLiteral n2) = NumberLiteral (n1 * n2)
multiplication e1 e2 = error $ "Unexpected operands in multiplication: " ++ show e1 ++ " " ++ show e2

division :: Expr.Literal -> Expr.Literal -> Expr.Literal
division (NumberLiteral n1) (NumberLiteral n2) = NumberLiteral (n1 `div` n2)
division e1 e2 = error $ "Unexpected operands in division: " ++ show e1 ++ " " ++ show e2

minusSign :: Expr.Literal -> Expr.Literal
minusSign (NumberLiteral n) = NumberLiteral (-n)
minusSign e = error $ "Unexpected operand in minusSign: " ++ show e

negate :: Expr.Literal -> Expr.Literal
negate (BoolLiteral b) = BoolLiteral (not b)
negate e = error $ "Unexpected operand in negate: " ++ show e





-- | [[ r ]]_a sigma = 
evalAction :: RuleClosure -> State -> IO (State, Set.Set Event)
evalAction = undefined



-- | Utilities

-- Assume the list is not empty.
chooseOne :: [a] -> IO [ (a, [a]) ]
chooseOne list =
 case list of
   []    -> error "chooseOne: the assumption is broken"
   list' -> return $ f list' []

 where
   f [e] prev        = [ (e, prev) ]
   f (e1:e2:es) prev = (e1, e2:es++prev) : f (e2:es) (e1:prev)


-}