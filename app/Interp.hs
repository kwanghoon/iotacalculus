module Interp where

import qualified Data.Set   as Set
import qualified Data.Map   as Map
import qualified Data.Maybe as Maybe

import Control.Monad (foldM)

import Expr

data Event =
    EventField DeviceName AttributeName EventConstant EventConstant
  | EventTimer TimerName EventConstant EventConstant
  deriving (Eq, Ord, Show)

type State = Map.Map DeviceName (Map.Map Expr.AttributeName Expr.Literal)

-- | Environments
type Environment = Map.Map DeclName Name  -- Device, Input, and Output declaration environments!!

type RuleClosure = (Environment, EMCA)

eventHandler :: RuleClosure -> EventHandler
eventHandler (_, emca) = Expr.eventHandler emca

-- | Rule sets
type Ruleset = Map.Map Integer RuleClosure

type RulesetIn = Ruleset
type RulesetOut = Ruleset

type IndexedRuleClosure = (Integer, RuleClosure) -- (index, rule closure)

data EvalState =
   NoneEvalState  -- Note: Ruleset is remmoved from this state.
 | EventEvalState Ruleset Ruleset  -- (In, Out)
 | PredEvalState  Ruleset Ruleset  -- (In, Out)
 | ActEvalState   Ruleset Ruleset  -- (In, Out)

 ------------------------------------------------------------------------------------------
 -- | Internet of things
 ------------------------------------------------------------------------------------------

type DeclName = Name

type IoT = (DeviceIoT, InputIoT, OutputIoT)

type DeviceIoT = Map.Map DeviceName (Capability, Map.Map AttributeName Literal)
type InputIoT  = Map.Map Name (ValueType, Literal)
type OutputIoT = Map.Map Name ([ ValueType], [ Literal ])

{-
------------------------------------------------------------------------------------------
-- | Driver
------------------------------------------------------------------------------------------

installRule :: IoT -> Environment -> Rule -> IO Ruleset
installRule iot env (NodeRule _ decls rs) = 
  do env' <- foldM (installDecl iot) env decls
     rsList <- mapM (installRule iot env') rs
     return $ foldl Map.union Map.empty rsList

installRule iot env (LeafRule _ decls emca) =
  do env' <- foldM (installDecl iot) env decls
     return $ Map.fromList [ (0, (env', emca)) ]

installDecl :: IoT -> Environment -> Decl -> IO Environment
installDecl iot env (DeviceDecl n cap) =
  do putStrLn n
     putStrLn cap
     putStrLn (show iot)
     name <- getLine
     return $ Map.insert n name env
     
installDecl _ env _ = return env -- Todo: do something!

driverECA :: Set.Set Event -> State -> Ruleset -> IO (Set.Set Event, State)
driverECA eventSet state ruleset =
  do  (maybeEvent, eventSet, state, status) <- driverNoneEvalState eventSet state ruleset
      case status of
        NoneEvalState -> 
          if Set.null eventSet then  -- terminal condition! Note maybeEvent must be Nothing.
              return (eventSet, state)
          else driverECA eventSet state ruleset
        _ -> let event = Maybe.fromJust maybeEvent in
             do (_, _, _, rulesetIn1, rulesetOut1) <- 
                  driverEventEvalState event eventSet state ruleset Map.empty

                (_, _, _, rulesetIn2, rulesetOut2) <-
                  driverPredEvalState event eventSet state rulesetIn1 rulesetOut1

                (_, eventSet', state', _) <-
                  driverActEvalState event eventSet state rulesetIn2 rulesetOut2

                driverECA eventSet' state' ruleset

------------------------------------------------------------------------------------------
-- | (R-E) rule event
------------------------------------------------------------------------------------------
 
evalREvent :: Set.Set Event -> State -> Ruleset ->
 IO (Maybe.Maybe Event, Set.Set Event, State, EvalState)

evalREvent eventSet state ruleset
 | Set.null eventSet = 
    return (Maybe.Nothing, eventSet, state, NoneEvalState)

 | otherwise =
    do (e, es) <- revDisjointUnion eventSet    -- eventSet = { e } U es
       return (Maybe.Just e, es, state, EventEvalState ruleset Map.empty)

driverNoneEvalState eventSet state ruleset = evalREvent eventSet state ruleset

------------------------------------------------------------------------------------------
-- | (R-H) and (R-HP) with EventEvalState rulesetIn rulsetOut
------------------------------------------------------------------------------------------

evalRHandleEvent 
  :: Event -> Set.Set Event -> State -> RulesetIn -> RulesetOut
      -> IO (Event, Set.Set Event, State, EvalState)
evalRHandleEvent event eventSet state rulesetIn rulesetOut

 | Map.null rulesetIn =
     return (event, eventSet, state, PredEvalState rulesetOut Map.empty)

 | otherwise =
     do (indexedRule, rs1) <- revDisjointUnionMap rulesetIn
        let (idx, rule) = indexedRule
        rs <- evalEvent idx rule (Interp.eventHandler rule) event
        return (event, eventSet, state, EventEvalState rs1 (Map.union rs rulesetOut))


driverEventEvalState event eventSet state rulesetIn rulesetOut =
 do (_, _, _, status) <- evalRHandleEvent event eventSet state rulesetIn rulesetOut
    case status of
      PredEvalState rulesetIn' rulesetOut' -> return (event, eventSet, state, rulesetIn', rulesetOut')
      EventEvalState rulesetIn' rulesetOut' ->
        driverEventEvalState event eventSet state rulesetIn' rulesetOut'
      _ -> error $ "driverEventEvalState: unexpected status"


------------------------------------------------------------------------------------------
-- | (R-P) and (R-PA) with PredES rulesetIn rulesetOut
------------------------------------------------------------------------------------------

evalRPredicate 
  :: Event -> Set.Set Event -> State -> RulesetIn -> RulesetOut
      -> IO (Event, Set.Set Event, State, EvalState)
evalRPredicate event eventSet state rulesetIn rulesetOut

  | Map.null rulesetIn =
      return (event, eventSet, state, ActEvalState rulesetOut Map.empty)

  | otherwise =
      do  (indexedRule, rs1) <- revDisjointUnionMap rulesetIn
          let (idx, rule) = indexedRule
          rs <- evalPredicate idx rule state
          return (event, eventSet, state, PredEvalState rs1 (Map.union rs rulesetOut))

driverPredEvalState event eventSet state rulesetIn rulesetOut =
  do (_, _, _, status) <- evalRPredicate event eventSet state rulesetIn rulesetOut
     case status of
      ActEvalState rulesetIn' rulesetOut' -> return (event, eventSet, state, rulesetIn', rulesetOut')
      PredEvalState rulesetIn' rulesetOut' -> 
        driverPredEvalState event eventSet state rulesetIn' rulesetOut'
      _ -> error $ "driverPredEvalState: unexpected status"

------------------------------------------------------------------------------------------
-- | (R-A) and (R-AE) with PredES rulesetIn rulesetOut
-- |
-- |   Note that the emca in the rulesetIn has a single pair of a predicate and an action 
-- |   that are found by evalRPredicate or evalPredicate. 
-- |   The function evalPredicateion leaves only the matched pair and remove 
-- |   all the unmatched pairs from the original emca.
------------------------------------------------------------------------------------------

evalRAction 
  :: Event -> Set.Set Event -> State -> RulesetIn -> RulesetOut
      -> IO (Maybe Event, Set.Set Event, State, EvalState)
evalRAction event eventSet state rulesetIn rulesetOut

   | Map.null rulesetIn =
       return (Maybe.Nothing, eventSet, state, NoneEvalState) -- Note ruleset is removed from NoneEvalState.

   | otherwise =
       do (indexedRule, rs1) <- revDisjointUnionMap rulesetIn
          let (idx, rule) = indexedRule
          (state', eventSet') <- evalActionFrom idx rule state
          return (Maybe.Just event, 
                    Set.union eventSet eventSet', state', 
                      ActEvalState rs1 (Map.union (Map.fromList [indexedRule]) rulesetOut))

driverActEvalState event eventSet state rulesetIn rulesetOut =
  do (maybeEvent, eventSet', state', status) <- evalRAction event eventSet state rulesetIn rulesetOut
     case status of
      NoneEvalState -> return (maybeEvent, eventSet', state', status)
      ActEvalState rulesetIn' rulesetOut' -> 
        driverActEvalState event eventSet state rulesetIn' rulesetOut' 

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
-- |
-- |  Note that the returned rule set is the firstly matched E-C-A from the EMCA.
------------------------------------------------------------------------------------------
evalPredicate :: Integer -> RuleClosure -> State -> IO Ruleset
evalPredicate idx (env, EMCA eh mpas) state = 
  do  sats <- mapM (\ (p, _) -> satisfied env state p) mpas
      case sats of
        [] -> return Map.empty
        bList -> case [ pas | (b,pas) <- zip bList mpas, isTrueLiteral b] of
                    [] -> return Map.empty
                    (pas:_) -> return (Map.fromList [(idx, (env, EMCA eh [pas]))])
                          -- Todo: What is meant by idx here??
                          --       It seems that the idx is not used in the following evalAction.
                          --       This is because of the definition of Ruleset, which is 
                          --       Map.Map Integer RuleClosure.

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
evalActionFrom :: Integer -> RuleClosure -> State -> IO (State, Set.Set Event)
evalActionFrom _ (env, EMCA _ [(_, actions)]) state = evalActions env state actions
evalActionFrom _ _ _ = error $ "evalActionFrom: unexpected configuration of EMCA"


evalActions :: Environment -> State -> Actions -> IO (State, Set.Set Event)
evalActions _ state [] = return (state, Set.empty)
evalActions env state (action : actions) = 
  do (state1, eventSet1) <- evalAction env state action
     (state2, eventSet2) <- evalActions env state1 actions
     return (state2, Set.union eventSet1 eventSet2)

evalAction :: Environment -> State -> Action -> IO (State, Set.Set Event)
evalAction env state (CommandAction (Field dev attr) e) = 
  do  litFrom <- eval env state (Field dev attr)
      litTo <- eval env state e
      let event = EventField dev attr litFrom litTo
      let state' = Map.insert dev 
                    (Map.insert attr litTo 
                      (Maybe.fromMaybe Map.empty (Map.lookup dev state))) state
      return (state', Set.singleton event)

evalAction _ _ (CommandAction _ e) = error $ "evalAction: CommandAction is not implemented"

evalAction env state (OutputAction n es) =
  do  lits <- mapM (eval env state) es
      putStrLn $ "OutputAction: " ++ show n ++ " " ++ show lits
      return (state, Set.empty)

evalAction env state (StartTimer t e) = 
  do  lit <- eval env state e
      let timerTo = addition lit (NumberLiteral 1)
      let timerEvent = EventTimer t lit timerTo
      let timerState = Map.fromList [("timer", timerTo)]  -- Fix: hard coding!
      return (Map.insert t timerState state , Set.singleton timerEvent)

evalAction _ state (StopTimer t) = 
  do  let state' = Map.delete t state
      return (state', Set.empty)

evalAction _ _ _ = error $ "evalAction: MapAction is not implemented"


------------------------------------------------------------------------------------------
-- | eval expressions, predicates, identifiers, timers, and fields
------------------------------------------------------------------------------------------

satisfied :: Environment -> State -> Expr.Predicate -> IO Expr.Literal
satisfied env state (LogicalAnd p1 p2) = logicalAnd <$> satisfied env state p1 <*> satisfied env state p2
satisfied env state (LogicalOr p1 p2) = logicalOr <$> satisfied env state p1 <*> satisfied env state p2
satisfied env state (IsEqual p1 p2) = isEqual <$> satisfied env state p1 <*> satisfied env state p2
satisfied env state (IsInequal p1 p2) = isInequal <$> satisfied env state p1 <*> satisfied env state p2
satisfied env state (LessThan p e) = lessThan <$> satisfied env state p <*> eval env state e
satisfied env state (LessThanOrEqualTo p e) = lessThanOrEqualTo <$> satisfied env state p <*> eval env state e
satisfied env state (GreaterThan p e) = greaterThan <$> satisfied env state p <*> eval env state e
satisfied env state (GreaterThanOrEqualTo p e) = greaterThanOrEqualTo <$> satisfied env state p <*> eval env state e
satisfied env state (ExpressionPredicate e) = eval env state e
satisfied _ _ _ = error "Unexpected predicate in satisfied"

-- satisfied (Forall g b p) = all (\ x -> satisfied (substitute b x p)) (group g) 
-- satisfied (Exists g b p) = all (\ x -> satisfied (substitute b x p)) (group g) 

eval :: Environment -> State -> Expr.Expression -> IO Expr.Literal
eval env state (Addition e1 e2) = addition <$> eval env state e1 <*> eval env state e2
eval env state (Subtraction e1 e2) = subtraction <$> eval env state e1 <*> eval env state e2
eval env state (Multiplication e1 e2) = multiplication <$> eval env state e1 <*> eval env state e2
eval env state (Division e1 e2) = division <$> eval env state e1 <*> eval env state e2
eval env state (MinusSign e) = minusSign <$> eval env state e
eval env state (Negate e) = Interp.negate <$> eval env state e
eval env state (LiteralExpression l) = return l
eval env state (IdentifierExpression s) = evalIdentifier env s
eval env state (Field devName fieldName) = evalField state devName fieldName
eval env state (Timer timerName) = evalTimer state timerName
eval env state (PredicateExpression p) = satisfied env state p

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


-- | Utilities

-- Reverse of disjoint union for map
--    * Assume the map is not empty.
revDisjointUnionMap :: (Ord k) => Map.Map k a -> IO ((k,a), Map.Map k a)
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
-}