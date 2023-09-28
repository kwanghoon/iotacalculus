module Interp where

import qualified Data.Set   as Set
import qualified Data.Map   as Map
import qualified Data.Maybe as Maybe

import Expr
import Data.Functor.Contravariant (Predicate(Predicate))
import Data.ByteString (index)

data Event =
    EventField DeviceName AttributeName EventConstant EventConstant
  | EventTimer TimerName EventConstant EventConstant
  deriving (Eq, Ord, Show)

type State = Map.Map DeviceName (Map.Map Expr.AttributeName Expr.Literal)

-- | Environments
type Environment = Map.Map Name (Expr.Literal, ValueType)  -- Input declaration environments!!

data DeclValue =
   Device Capability
 | Input  ValueType
 | Output [ ValueType ]

type RuleClosure = (Environment, EMCA)

eventHandler :: RuleClosure -> EventHandler
eventHandler (_, emca) = Expr.eventHandler emca

-- | Rule sets
type Ruleset = Map.Map Integer RuleClosure

type IndexedRuleClosure = (Integer, RuleClosure) -- (index, rule closure)

data EvalState =
   NoneEvalState  Ruleset
 | EventEvalState Ruleset Ruleset  -- (In, Out)
 | PredEvalState  Ruleset Ruleset  -- (In, Out)
 | ActEvalState   Ruleset Ruleset  -- (In, Out)

------------------------------------------------------------------------------------------
-- | (R-E) rule event
------------------------------------------------------------------------------------------

evalREvent :: Set.Set Event -> Set.Set State -> Ruleset ->
 IO (Maybe.Maybe Event, Set.Set Event, Set.Set State, EvalState)

evalREvent eventSet stateSet ruleset
 | Set.null eventSet = 
    return (Maybe.Nothing, eventSet, stateSet, NoneEvalState ruleset)

 | otherwise =
    do (e, es) <- revDisjointUnion eventSet    -- eventSet = { e } U es
       return (Maybe.Just e, es, stateSet, EventEvalState ruleset Map.empty)

------------------------------------------------------------------------------------------
-- | (R-H) and (R-HP) with EventEvalState rulesetIn rulsetOut
------------------------------------------------------------------------------------------

evalRHandleEvent 
  :: Event -> Set.Set Event -> State -> Ruleset -> Ruleset 
      -> IO (Event, Set.Set Event, State, EvalState)
evalRHandleEvent event eventSet stateSet rulesetIn rulesetOut

 | Map.null rulesetIn =
     return (event, eventSet, stateSet, PredEvalState rulesetOut Map.empty)

 | otherwise =
     do (indexedRule, rs1) <- revDisjointUnionMap rulesetIn
        let (idx, rule) = indexedRule
        rs <- evalEvent idx rule (Interp.eventHandler rule) event
        return (event, eventSet, stateSet, EventEvalState rs1 (Map.union rs rulesetOut))

------------------------------------------------------------------------------------------
-- | (R-P) and (R-PA) with PredES rulesetIn rulesetOut
------------------------------------------------------------------------------------------

evalRPredicate 
  :: Event -> Set.Set Event -> State -> Map.Map Integer RuleClosure -> Ruleset 
      -> IO (Event, Set.Set Event, State, EvalState)
evalRPredicate event eventSet stateSet rulesetIn rulesetOut

  | Map.null rulesetIn =
      return (event, eventSet, stateSet, ActEvalState rulesetOut Map.empty)

  | otherwise =
      do  (indexedRule, rs1) <- revDisjointUnionMap rulesetIn
          let (idx, rule) = indexedRule
          rs <- evalPredicate idx rule stateSet
          return (event, eventSet, stateSet, PredEvalState rs1 (Map.union rs rulesetOut))

------------------------------------------------------------------------------------------
-- | (R-A) and (R-AE) with PredES rulesetIn rulesetOut
------------------------------------------------------------------------------------------

evalRAction 
  :: Event -> Set.Set Event -> State -> Map.Map Integer RuleClosure -> Ruleset 
      -> IO (Maybe Event, Set.Set Event, State, EvalState)
evalRAction event eventSet stateSet rulesetIn rulesetOut

   | Map.null rulesetIn =
       return (Maybe.Nothing, eventSet, stateSet, NoneEvalState rulesetOut)

   | otherwise =
       do (indexedRule, rs1) <- revDisjointUnionMap rulesetIn
          let (_, rule) = indexedRule
          (stateSet', eventSet') <- evalAction rule stateSet
          return (Maybe.Just event, 
                    Set.union eventSet eventSet', stateSet', 
                      ActEvalState rs1 (Map.union (Map.fromList [indexedRule]) rulesetOut))

------------------------------------------------------------------------------------------
-- | evalEvent:
-- | [[ r ]]_h e = rs
-- |   Note that the 3rd argument is one extracted from the 2nd argument.
------------------------------------------------------------------------------------------

evalEvent :: Integer -> RuleClosure -> EventHandler -> Event -> IO Ruleset

-- for JustEvent
evalEvent idx rc (JustEvent (Field devName1 fieldName1)) 
                  (EventField devName2 fieldName2 _ _)
  | devName1 == devName2 && fieldName1 == fieldName2
    = return $ Map.fromList [ (idx, rc)]
  | otherwise = return Map.empty

evalEvent idx rc (JustEvent (Timer timerName)) 
                  (EventTimer timerName' _ _)
  | timerName == timerName' = return $ Map.fromList [ (idx, rc)]
  | otherwise = return Map.empty

-- for EventTo
evalEvent idx rc (EventTo (Field devName1 fieldName1) eventConstant1)
                  (EventField devName2 fieldName2 _ eventConstant2)
  | devName1 == devName2 && fieldName1 == fieldName2 && eventConstant1 == eventConstant2
    = return $ Map.fromList [ (idx, rc)]
  | otherwise = return Map.empty

evalEvent idx rc (EventTo (Timer timerName1) eventConstant1)
                  (EventTimer timerName2 _ eventConstant2)
  | timerName1 == timerName2 && eventConstant1 == eventConstant2
    = return $ Map.fromList [ (idx, rc)]
  | otherwise = return Map.empty

-- for EventFrom
evalEvent idx rc (EventFrom (Field devName1 fieldName1) eventConstant1)
                  (EventField devName2 fieldName2 eventConstant2 _)
  | devName1 == devName2 && fieldName1 == fieldName2 && eventConstant1 == eventConstant2
    = return $ Map.fromList [ (idx, rc)]
  | otherwise = return Map.empty

evalEvent idx rc (EventFrom (Timer timerName1) eventConstant1)
                  (EventTimer timerName2 eventConstant2 _)
  | timerName1 == timerName2 && eventConstant1 == eventConstant2
    = return $ Map.fromList [ (idx, rc)]
  | otherwise = return Map.empty

-- for EventFromTo
evalEvent idx rc (EventFromTo (Field devName1 fieldName1) eventConstant1 eventConstant1')
                  (EventField devName2 fieldName2 eventConstant2 eventConstant2')
  | devName1 == devName2 && fieldName1 == fieldName2 
    && eventConstant1 == eventConstant2 && eventConstant1' == eventConstant2'
    = return $ Map.fromList [ (idx, rc)]
  | otherwise = return Map.empty

evalEvent idx rc (EventFromTo (Timer timerName1) eventConstant1 eventConstant1')
                  (EventTimer timerName2 eventConstant2 eventConstant2')
  | timerName1 == timerName2 && eventConstant1 == eventConstant2 
    && eventConstant1' == eventConstant2'
    = return $ Map.fromList [ (idx, rc)]
  | otherwise = return Map.empty

-- forGroupEvent
evalEvent _ (_, EMCA (GroupEvent {}) _) _ _ = error "evalEvent over GroupEvent: not implemented"

evalEvent _ _ evH event = error $ "evalEvent: " ++ show evH ++ " over " ++ show event


------------------------------------------------------------------------------------------
-- | evalPredicate:
-- | [[ r ]]_p sigma = rs
------------------------------------------------------------------------------------------
evalPredicate :: Integer -> RuleClosure -> State -> IO Ruleset
evalPredicate idx (env, EMCA eh mpas) state = 
  do  sats <- mapM (\ (p, _) -> satisfied p) mpas
      case sats of
        [] -> return Map.empty
        bList -> case [ pas | (b,pas) <- zip bList mpas, isTrueLiteral b] of
                    [] -> return Map.empty
                    (pas:_) -> return (Map.fromList [(idx, (env, EMCA eh [pas]))])
  where
    satisfied :: Expr.Predicate -> IO Expr.Literal
    satisfied (LogicalAnd p1 p2) = logicalAnd <$> satisfied p1 <*> satisfied p2
    satisfied (LogicalOr p1 p2) = logicalOr <$> satisfied p1 <*> satisfied p2
    satisfied (IsEqual p1 p2) = isEqual <$> satisfied p1 <*> satisfied p2
    satisfied (IsInequal p1 p2) = isInequal <$> satisfied p1 <*> satisfied p2
    satisfied (LessThan p e) = lessThan <$> satisfied p <*> eval e
    satisfied (LessThanOrEqualTo p e) = lessThanOrEqualTo <$> satisfied p <*> eval e
    satisfied (GreaterThan p e) = greaterThan <$> satisfied p <*> eval e
    satisfied (GreaterThanOrEqualTo p e) = greaterThanOrEqualTo <$> satisfied p <*> eval e
    satisfied (ExpressionPredicate e) = eval e
    satisfied _ = error "Unexpected predicate in satisfied"

    -- satisfied (Forall g b p) = all (\ x -> satisfied (substitute b x p)) (group g) 
    -- satisfied (Exists g b p) = all (\ x -> satisfied (substitute b x p)) (group g) 

    eval :: Expr.Expression -> IO Expr.Literal
    eval (Addition e1 e2) = addition <$> eval e1 <*> eval e2
    eval (Subtraction e1 e2) = subtraction <$> eval e1 <*> eval e2
    eval (Multiplication e1 e2) = multiplication <$> eval e1 <*> eval e2
    eval (Division e1 e2) = division <$> eval e1 <*> eval e2
    eval (MinusSign e) = minusSign <$> eval e
    eval (Negate e) = Interp.negate <$> eval e
    eval (LiteralExpression l) = return l
    eval (IdentifierExpression s) = evalIdentifier env s
    eval (Field devName fieldName) = evalField state devName fieldName
    eval (Timer timerName) = evalTimer state timerName
    eval (PredicateExpression p) = satisfied p
    eval _ = error "Unexpected expression in eval"

    evalIdentifier :: Environment -> String -> IO Expr.Literal
    evalIdentifier environment s = 
      case Map.lookup s environment of
        Just (lit, _) -> return lit
        Nothing ->  error $ "evalIdentifier: " ++ s    

    evalField :: State -> DeviceName -> AttributeName -> IO Expr.Literal
    evalField deviceState devName fieldName = 
      case Map.lookup devName deviceState of
        Just attrs -> 
          case Map.lookup fieldName attrs of
            Just lit -> return lit
            Nothing -> error $ "evalField: " ++ fieldName
        Nothing -> error $ "evalField: " ++ devName

    evalTimer :: State -> TimerName -> IO Expr.Literal
    evalTimer deviceState timerName = 
      evalField deviceState timerName "timer"  -- Todo: fix this hard coding!

------------------------------------------------------------------------------------------
-- | Operations over literals (values)
------------------------------------------------------------------------------------------

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

logicalOr :: Expr.Literal -> Expr.Literal -> Expr.Literal
logicalOr (BoolLiteral b1) (BoolLiteral b2) = BoolLiteral (b1 || b2)
logicalOr e1 e2 = error $ "Unexpected operands in logicalOr: " ++ show e1 ++ " " ++ show e2

logicalAnd :: Expr.Literal -> Expr.Literal -> Expr.Literal
logicalAnd (BoolLiteral b1) (BoolLiteral b2) = BoolLiteral (b1 && b2)
logicalAnd e1 e2 = error $ "Unexpected operands in logicalAnd: " ++ show e1 ++ " " ++ show e2

isEqual :: Expr.Literal -> Expr.Literal -> Expr.Literal
isEqual (BoolLiteral b1) (BoolLiteral b2) = BoolLiteral (b1 == b2)
isEqual (NumberLiteral n1) (NumberLiteral n2) = BoolLiteral (n1 == n2)
isEqual (StringLiteral s1) (StringLiteral s2) = BoolLiteral (s1 == s2)
isEqual (ConstantLiteral c1) (ConstantLiteral c2) = BoolLiteral (c1 == c2)
isEqual e1 e2 = error $ "Unexpected operands in isEqual: " ++ show e1 ++ " " ++ show e2

isInequal :: Expr.Literal -> Expr.Literal -> Expr.Literal
isInequal (BoolLiteral b1) (BoolLiteral b2) = BoolLiteral (b1 /= b2)
isInequal (NumberLiteral n1) (NumberLiteral n2) = BoolLiteral (n1 /= n2)
isInequal (StringLiteral s1) (StringLiteral s2) = BoolLiteral (s1 /= s2)
isInequal (ConstantLiteral c1) (ConstantLiteral c2) = BoolLiteral (c1 /= c2)
isInequal e1 e2 = error $ "Unexpected operands in isInequal: " ++ show e1 ++ " " ++ show e2

lessThan :: Expr.Literal -> Expr.Literal -> Expr.Literal
lessThan (NumberLiteral n1) (NumberLiteral n2) = BoolLiteral (n1 < n2)
lessThan e1 e2 = error $ "Unexpected operands in lessThan: " ++ show e1 ++ " " ++ show e2

lessThanOrEqualTo :: Expr.Literal -> Expr.Literal -> Expr.Literal
lessThanOrEqualTo (NumberLiteral n1) (NumberLiteral n2) = BoolLiteral (n1 <= n2)
lessThanOrEqualTo e1 e2 = error $ "Unexpected operands in lessThanOrEqualTo: " ++ show e1 ++ " " ++ show e2

greaterThan :: Expr.Literal -> Expr.Literal -> Expr.Literal
greaterThan (NumberLiteral n1) (NumberLiteral n2) = BoolLiteral (n1 > n2)
greaterThan e1 e2 = error $ "Unexpected operands in greaterThan: " ++ show e1 ++ " " ++ show e2

greaterThanOrEqualTo :: Expr.Literal -> Expr.Literal -> Expr.Literal
greaterThanOrEqualTo (NumberLiteral n1) (NumberLiteral n2) = BoolLiteral (n1 >= n2)
greaterThanOrEqualTo e1 e2 = error $ "Unexpected operands in greaterThanOrEqualTo: " ++ show e1 ++ " " ++ show e2


------------------------------------------------------------------------------------------
-- | evalAction:
-- | [[ r ]]_a sigma = 
------------------------------------------------------------------------------------------
evalAction :: RuleClosure -> State -> IO (State, Set.Set Event)
evalAction = undefined

------------------------------------------------------------------------------------------


-- | Utilities

-- Reverse of disjoint union for map
--    * Assume the map is not empty.
revDisjointUnionMap :: (Ord k, Ord a) => Map.Map k a -> IO ((k,a), Map.Map k a)
revDisjointUnionMap mapmap = 
  do  let list = Map.assocs mapmap
      list' <- revDisjointUnionForList list
      let (elm, list1) = head list' -- Just pick the first one!
      return (elm, Map.fromList list1)

-- Reverse of disjoint union for set
--    * Assume the set is not empty.
revDisjointUnion :: Ord a => Set.Set a -> IO (a, Set.Set a)
revDisjointUnion set = 
  do  list <- revDisjointUnionForList (Set.toList set)
      let (elm, list1) = head list -- Just pick the first one!
      return (elm, Set.fromList list1)

-- revDisjointUnionForList [1, 2, 3]
-- ==> [ (1, [2, 3]), (2, [1, 3]), (3, [1, 2]) ]

-- Reverse of disjoint union for set
--    * Assume the list is not empty.
revDisjointUnionForList :: [a] -> IO [ (a, [a]) ]
revDisjointUnionForList list =
  case list of
    []    -> error "revDisjointUnion: the assumption is broken"
    list' -> return $ f list' []

  where
    f [] _         = []
    f (e:es) prev  = (e, prev ++ es) : f es (prev ++ [e])
