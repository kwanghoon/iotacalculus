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

type State = IoT -- Map.Map DeviceName (Map.Map Expr.AttributeName Expr.Literal)

-- | Environments
type Environment = Map.Map DeclName Name  -- Device, Input, and Output declaration environments!!

type RuleClosure = (Environment, EMCA)

envFrom :: RuleClosure -> Environment
envFrom = fst

eventHandler :: RuleClosure -> EventHandler
eventHandler (_, emca) = Expr.eventHandler emca

-- | Rule sets
type Ruleset = Set.Set RuleClosure

type RulesetIn = Ruleset
type RulesetOut = Ruleset

data EvalState =
   NoneEvalState  -- Note: Ruleset is remmoved from this state.
 | EventEvalState Ruleset Ruleset  -- (In, Out)
 | PredEvalState  Ruleset Ruleset  -- (In, Out)
 | ActEvalState   Ruleset Ruleset  -- (In, Out)

 ------------------------------------------------------------------------------------------
 -- | Internet of things
 ------------------------------------------------------------------------------------------

type DeclName = Name

type IoT = (DeviceIoT, InputIoT, OutputIoT, TimerIoT)

type DeviceIoT = Map.Map DeviceName (Capability, Map.Map AttributeName Literal)
type InputIoT  = Map.Map Name (ValueType, Literal)
type OutputIoT = Map.Map Name ([ ValueType], [ Literal ])
type TimerIoT = Map.Map Name Literal -- NumberLiteral

readDev :: DeviceName -> AttributeName -> IoT -> IO Literal
readDev dev attr (devIot, _, _,_) = 
  case Map.lookup dev devIot of
    Just (_, attrs) -> 
      case Map.lookup attr attrs of
        Just lit -> return lit
        Nothing -> error $ "readDev: " ++ attr
    Nothing -> error $ "readDev: " ++ dev

readInputOrTimer :: Name -> IoT -> IO Literal
readInputOrTimer name (_, inputIot, _, timerIot) = 
  case Map.lookup name inputIot of
    Just (_, lit) -> return lit
    Nothing -> case Map.lookup name timerIot of
                Just lit -> return lit
		Nothing -> error $ "readInputOrTimer: " ++ name

doCmd :: DeviceName -> AttributeName -> Literal -> IoT -> IO IoT
doCmd dev attr lit (devIot, inputIot, outputIot, timerIot) =     -- Implementation:   dev.attr := lit
  case Map.lookup dev devIot of
    Just (cap, attrs) -> 
      do let attrs' = Map.insert attr lit attrs
	 let devIot' = Map.insert dev (cap, attrs') devIot
	 return (devIot', inputIot, outputIot, timerIot)
    Nothing -> error $ "doCmd: not found in the state" ++ dev
     
startTimer :: TimerName -> Literal -> IoT -> IO IoT
startTimer timerName lit (devIot, inputIot, outputIot, timerIot) =     -- Implementation:   timerName := lit
  case Map.lookup timerName timerIot of
    Nothing -> 
      do let timerIot' = Map.insert timerName lit timerIot
         return (devIot, inputIot, outputIot, timerIot')
    Just lit1 -> error $ "startTimer: " ++ timerName ++ " is already started with " ++ show lit1

stopTimer :: TimerName -> IoT -> IO IoT
stopTimer timerName (devIot, inputIot, outputIot, timerIot) =     -- Implementation:   stop timerName 
  do let timerIot' = Map.delete timerName timerIot
     return (devIot, inputIot, outputIot, timerIot')

tickTimer :: TimerName -> IoT -> IO IoT
tickTimer timerName (devIot, inputIot, outputIot, timerIot) =     -- Implementation:   tick timerName
  case Map.lookup timerName timerIot of
    Just lit -> return (devIot, inputIot, outputIot, Map.insert timerName (addition lit (NumberLiteral 1)) timerIot)
    Nothing -> error $ "tickTimer: " ++ timerName

readTimer :: TimerName -> IoT -> IO Literal
readTimer timerName (_, _, _, timerIot) = 
  case Map.lookup timerName timerIot of
    Just lit -> return lit
    Nothing -> error $ "readTimer: " ++ timerName     

areNamesEqual :: Environment -> Name -> Name -> Bool
areNamesEqual env declName name2 =
  case Map.lookup declName env of
    Nothing -> error $ "areDevNamesEqual: not found " ++ declName
    Just name1 -> name1 == name2

------------------------------------------------------------------------------------------
-- | Driver
------------------------------------------------------------------------------------------

installRule :: Environment -> Rule -> IO Ruleset
installRule env (NodeRule _ _ rs) =
  do rsList <- mapM (installRule env) rs
     return $ foldl Set.union Set.empty rsList

installRule env (LeafRule _ _ emca) =
  do return $ Set.fromList [ (env, emca) ]

driverECA :: Set.Set Event -> State -> Ruleset -> IO (Set.Set Event, State)
driverECA eventSet state ruleset =
  do  (maybeEvent, eventSet', state', status) <- driverNoneEvalState eventSet state ruleset
      case status of
        NoneEvalState -> 
          if Set.null eventSet' then  -- terminal condition! Note maybeEvent must be Nothing.
              return (eventSet', state')
	      
          else callDriverECA eventSet' state' ruleset
        _ -> let event = Maybe.fromJust maybeEvent in
             do (_, _, _, rulesetIn1, rulesetOut1) <- 
                  driverEventEvalState event eventSet' state' ruleset Set.empty

                (_, _, _, rulesetIn2, rulesetOut2) <-
                  driverPredEvalState event eventSet' state' rulesetIn1 rulesetOut1

                (_, eventSet'', state'', _) <-
                  driverActEvalState event eventSet' state' rulesetIn2 rulesetOut2

                callDriverECA eventSet'' state'' ruleset

callDriverECA :: Set.Set Event -> State -> Ruleset -> IO (Set.Set Event, State)
callDriverECA eventSet state ruleset =
  do print "after a cycle of event-condition-action,"
     print state
     print eventSet
     putStrLn ""

     driverECA eventSet state ruleset

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
       return (Maybe.Just e, es, state, EventEvalState ruleset Set.empty)

driverNoneEvalState eventSet state ruleset = evalREvent eventSet state ruleset

------------------------------------------------------------------------------------------
-- | (R-H) and (R-HP) with EventEvalState rulesetIn rulsetOut
------------------------------------------------------------------------------------------

evalRHandleEvent 
  :: Event -> Set.Set Event -> State -> RulesetIn -> RulesetOut
      -> IO (Event, Set.Set Event, State, EvalState)
evalRHandleEvent event eventSet state rulesetIn rulesetOut

 | Set.null rulesetIn =
     return (event, eventSet, state, PredEvalState rulesetOut Set.empty)

 | otherwise =
     do (rule, rs1) <- revDisjointUnion rulesetIn
        rs <- evalEvent (envFrom rule) rule (Interp.eventHandler rule) event
        return (event, eventSet, state, EventEvalState rs1 (Set.union rs rulesetOut))


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

  | Set.null rulesetIn =
      return (event, eventSet, state, ActEvalState rulesetOut Set.empty)

  | otherwise =
      do  (rule, rs1) <- revDisjointUnion rulesetIn
          rs <- evalPredicate rule state
          return (event, eventSet, state, PredEvalState rs1 (Set.union rs rulesetOut))

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

   | Set.null rulesetIn =
       return (Maybe.Nothing, eventSet, state, NoneEvalState) -- Note ruleset is removed from NoneEvalState.

   | otherwise =
       do (rule, rs1) <- revDisjointUnion rulesetIn
          (state', eventSet') <- evalActionFrom rule state
          return (Maybe.Just event, 
                    Set.union eventSet eventSet', state', 
                      ActEvalState rs1 (Set.union (Set.fromList [rule]) rulesetOut))

driverActEvalState event eventSet state rulesetIn rulesetOut =
  do (maybeEvent, eventSet', state', status) <- evalRAction event eventSet state rulesetIn rulesetOut
     case status of
      NoneEvalState -> return (maybeEvent, eventSet', state', status)
      ActEvalState rulesetIn' rulesetOut' -> 
        driverActEvalState event eventSet' state' rulesetIn' rulesetOut' 

------------------------------------------------------------------------------------------
-- | evalEvent:
-- | [[ r ]]_h e = rs
-- |   Note that the 1st and 3rd arguments are extracted from the 2nd argument.
------------------------------------------------------------------------------------------

evalEvent :: Environment -> RuleClosure -> EventHandler -> Event -> IO Ruleset

-- for JustEvent
evalEvent env rc (JustEvent (Field devName1 fieldName1)) 
                  (EventField devName2 fieldName2 _ _)
  | areNamesEqual env devName1 devName2 && fieldName1 == fieldName2
    = return $ Set.fromList [ rc]
  | otherwise = return Set.empty

evalEvent env rc (JustEvent (Timer timerName)) 
                  (EventTimer timerName' _ _)
  | areNamesEqual env timerName timerName' =
     do return $ Set.fromList [ rc]
  | otherwise = return Set.empty

-- for EventTo
evalEvent env rc (EventTo (Field devName1 fieldName1) eventConstant1)
                  (EventField devName2 fieldName2 _ eventConstant2)
  | areNamesEqual env devName1 devName2
    && fieldName1 == fieldName2 && eventConstant1 == eventConstant2
    = return $ Set.fromList [ rc]
  | otherwise = return Set.empty

evalEvent env rc (EventTo (Timer timerName1) eventConstant1)
                  (EventTimer timerName2 _ eventConstant2)
  | areNamesEqual env timerName1 timerName2 && eventConstant1 == eventConstant2
    = return $ Set.fromList [ rc]
  | otherwise = return Set.empty

-- for EventFrom
evalEvent env rc (EventFrom (Field devName1 fieldName1) eventConstant1)
                  (EventField devName2 fieldName2 eventConstant2 _)
  | areNamesEqual env devName1 devName2
    && fieldName1 == fieldName2 && eventConstant1 == eventConstant2
    = return $ Set.fromList [ rc ]
  | otherwise = return Set.empty

evalEvent env rc (EventFrom (Timer timerName1) eventConstant1)
                  (EventTimer timerName2 eventConstant2 _)
  | areNamesEqual env timerName1 timerName2 && eventConstant1 == eventConstant2
    = return $ Set.fromList [ rc ]
  | otherwise = return Set.empty

-- for EventFromTo
evalEvent env rc (EventFromTo (Field devName1 fieldName1) eventConstant1 eventConstant1')
                  (EventField devName2 fieldName2 eventConstant2 eventConstant2')
  | areNamesEqual env devName1 devName2 && fieldName1 == fieldName2 
    && eventConstant1 == eventConstant2 && eventConstant1' == eventConstant2'
    = return $ Set.fromList [ rc ]
  | otherwise = return Set.empty

evalEvent env rc (EventFromTo (Timer timerName1) eventConstant1 eventConstant1')
                  (EventTimer timerName2 eventConstant2 eventConstant2')
  | areNamesEqual env timerName1 timerName2 && eventConstant1 == eventConstant2 
    && eventConstant1' == eventConstant2'
    = return $ Set.fromList [ rc]
  | otherwise = return Set.empty

-- forGroupEvent
evalEvent _ (_, EMCA (GroupEvent {}) _) _ _ = error "evalEvent over GroupEvent: not implemented"

evalEvent _ _ evH event = return Set.empty


------------------------------------------------------------------------------------------
-- | evalPredicate:
-- | [[ r ]]_p sigma = rs
-- |
-- |  Note that the returned rule set is the firstly matched E-C-A from the EMCA.
------------------------------------------------------------------------------------------
evalPredicate :: RuleClosure -> State -> IO Ruleset
evalPredicate (env, EMCA eh mpas) state = 
  do  sats <- mapM (\ (p, _) -> satisfied env state p) mpas
      case sats of
        [] -> return Set.empty
        bList -> case [ pas | (b,pas) <- zip bList mpas, isTrueLiteral b] of
                    [] -> return Set.empty
                    (pas:_) -> return (Set.fromList [(env, EMCA eh [pas])])
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
evalActionFrom :: RuleClosure -> State -> IO (State, Set.Set Event)
evalActionFrom (env, EMCA _ [(_, actions)]) state = evalActions env state actions
evalActionFrom _ _ = error $ "evalActionFrom: unexpected configuration of EMCA"


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
      
      case Map.lookup dev env of
	Nothing -> error $ "evalAction: not found in the environment " ++ dev
	Just dev' -> 
          do state' <- doCmd dev' attr litTo state
             return (state', Set.singleton event)

evalAction _ _ (CommandAction _ e) = error $ "evalAction: CommandAction is not implemented"

evalAction env state (OutputAction n es) =
  do  lits <- mapM (eval env state) es
      putStrLn $ "OutputAction: " ++ show n ++ " " ++ show lits
      return (state, Set.empty)

evalAction env state (StartTimer t e) = 
  case Map.lookup t env of
    Nothing -> error $ "evalAction timer: not found in the environment :" ++ t
    Just t' ->
      do lit <- eval env state e
	 let timerTo = addition lit (NumberLiteral 1)
	 let timerEvent = EventTimer t' lit timerTo
	 state' <- startTimer t' timerTo state
	 return (state', Set.singleton timerEvent)

evalAction env state (StopTimer t) = 
  case Map.lookup t env of
    Nothing -> error $ "evalAction timer: not found in the environment :" ++ t
    Just t' ->
      do  state' <- stopTimer t' state
	  return (state', Set.empty)

evalAction env state (TickTimer t) = 
  case Map.lookup t env of
    Nothing -> error $ "evalAction timer: not found in the environment :" ++ t
    Just t' ->
      do  litFrom <- readTimer t' state
          state' <- tickTimer t' state
	  litTo <- readTimer t' state'
	  let timerEvent = EventTimer t' litFrom litTo
	  return (state', Set.singleton timerEvent)

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
eval env state (IdentifierExpression s) = evalIdentifier env state s
eval env state (Field devName fieldName) = evalField env state devName fieldName
eval env state (Timer timerName) = evalTimer env state timerName
eval env state (PredicateExpression p) = satisfied env state p

evalIdentifier :: Environment -> State -> String -> IO Expr.Literal
evalIdentifier env state s = 
  case Map.lookup s env of
    Nothing ->  return $ ConstantLiteral s -- error $ "evalIdentifier: " ++ s  -- Todo: Should type check!
    Just s' -> readInputOrTimer s' state

evalField :: Environment -> State -> DeviceName -> AttributeName -> IO Expr.Literal
evalField env state devName fieldName =
  case Map.lookup devName env of
    Nothing -> error $ "evalField: " ++ devName
    Just devName' -> readDev devName' fieldName state

evalTimer :: Environment -> State -> TimerName -> IO Expr.Literal
evalTimer env state timerName =
  case Map.lookup timerName env of
    Nothing -> error $ "evalTimer: " ++ timerName
    Just timerName' -> readTimer timerName' state


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