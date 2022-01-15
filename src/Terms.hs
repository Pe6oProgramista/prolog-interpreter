{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
module Terms where

import Prelude hiding (head)
import Prelude (id)
import Data.Maybe (fromMaybe)

import Utils ( (>.>), uniq, getLine', maybeAndThen, second )

import Debug.Trace (trace)
import Control.Monad (foldM_, when, unless)
import System.IO (BufferMode(LineBuffering, NoBuffering), hSetBuffering, stdin)
import Data.Foldable (asum)
import Data.List (find)

debug :: c -> String -> c
debug = flip trace

-- some build-in predicates: https://homepage.cs.uiowa.edu/~fleck/runSWI.htm
-- https://www.cse.unsw.edu.au/~billw/dictionaries/prolog/fact.html
-- whole dictionary: https://www.cse.unsw.edu.au/~billw/dictionaries/prologdict.html

-- boolean: true | false

-- atom: start with LCase and contains LCase, UCase, Digits, "_"   //could be in '' and start with anything
-- identifier | constant = atom   // [] is atom
-- identifier = predicate = functor

-- variable: start with UCase or "_" and contains LCase, UCase, Digits, "_"
-- only "_" is used when var is used once(no matter its value)
-- "_" is never grounded (its always free var)

-- term: variable | atomic_term | compound_term   // lists, expressions are also terms
-- atomic_term: constant   // number is atomic term
-- compound_term: identifier(term{,term})

-- clause: unit that end with "."
-- clause = fact | rule

-- fact: compound_term. | identifier.  // = identifier().

-- goal = variable | compound_term | constant
-- query = goal  |  query {,/; query }  |  (query)
-- rule: compound_term :- compound_term {,/; compound_term }. | identifire :- ...  // = identifire() :- ...

-- Goal: identifier{({args})}.  // identifier must be in facts or rules with same arity 
-- args - terms  // could not contains variables

-- Query: identifier(arg1{,args}). // identifier must be in facts or rules with same arity
-- args - terms | variables // with at least one variable

class Callable a where
  -- returns array because some terms may map to Rule that has GoalList with more than one andQuery
  call :: Program -> Context -> a -> [QueryRes]

class IsTerm a where
  asTerm :: a -> Term

  callTerm :: Program -> Context -> a -> [QueryRes]
  callTerm prog ctxt = call prog ctxt . asTerm


type Atom = String

newtype Id = Id { idAtom :: Atom }
  deriving (Eq)

newtype Const = Const { constId :: Id }
  deriving (Eq)

newtype Var = Var { varName :: String }

instance Eq Var where
  (==) (Var v1) (Var v2)
    | varName anonymousVar `elem` [v1, v2] = True
    | otherwise = v1 == v2

data AtomicTerm = ConstT Const | IntT Int | FloatT Float
  deriving (Eq)

data CompoundTerm = CompoundTerm { functor :: Id, args :: [Term] }
  deriving (Eq)

data Term = VarT Var | AtomicT AtomicTerm | CompoundT CompoundTerm
  -- deriving (Eq)

instance Eq Term where
  (==) (VarT v1) (VarT v2) = v1 == v2
  (==) (VarT v) _ = varName anonymousVar == varName v
  (==) _ (VarT v) = varName anonymousVar == varName v
  (==) (AtomicT at1) (AtomicT at2) = at1 == at2
  (==) (CompoundT ct1) (CompoundT ct2) = ct1 == ct2
  (==) _ _ = False

instance Show Id where
  show i = idAtom i

instance Show Const where
  show c = show $ constId c

instance Show Var where
  show v = varName v

instance Show AtomicTerm where
  show (ConstT c) = show c
  show (IntT n) = show n
  show (FloatT x) = show x

instance Show CompoundTerm where
  show ct =
    idAtom (functor ct) ++
    "(" ++ foldr (\x acc -> show x ++ (if null acc then "" else ",") ++ acc) "" (args ct) ++ ")"

instance Show Term where
  show (VarT var) = varName var
  show (AtomicT at) = show at
  show (CompoundT ct) = show ct

-- Orders Terms by how deep they are (right is deeper if "left < right" returns True)
instance Ord Term where
  (<=) (VarT _) _ = True
  (<=) (AtomicT _) (AtomicT _) = True
  (<=) (AtomicT _) (CompoundT _) = True
  (<=) (CompoundT _) (AtomicT _) = True
  (<=) (CompoundT (CompoundTerm _ args1)) (CompoundT (CompoundTerm _ args2)) = case args1 of
    [] -> True
    args1' -> case args2 of
      [] -> False
      args2' -> a1 <= a2
        where
          a1 = foldr1 (\x acc -> if x <= acc then acc else x) args1'
          a2 = foldr1 (\x acc -> if x <= acc then acc else x) args2'

  (<=) _ _ = False


data ClauseHead
  = CompoundHead CompoundTerm
  | ConstHead Id
  deriving (Show, Eq)

data Goal
  = ClauseGoal ClauseHead -- goal  goal(..)
  | VarGoal Var           -- Goal - this will work if the Var contains boolean value
  | EnclosedGoal GoalList -- (goal1, goal2 ; goal3, goal4)
  deriving (Show, Eq)

-- Query
type GoalList = [[Goal]] -- map every list w/ Data.Monoid.All and the outer list is mapped w/ Data.Monoid.Any

data Info
  = Error String
  | Ok
  | Cut (Context, [QueryRes])
  | EndInterpreter 
  deriving (Show, Eq)

type QueryRes = Either Info (Context, Bool)

data PredefClause = PredefClause {
    predefCH :: ClauseHead,
    callPredef :: Program -> Context -> [QueryRes] }

instance Show PredefClause where
  show (PredefClause h _) = show h

instance Eq PredefClause where
  (==) (PredefClause h1 _) (PredefClause h2 _) = h1 == h2

data Clause
  = Fact ClauseHead
  | Rule ClauseHead GoalList
  | PredefC PredefClause
  deriving (Show, Eq)

-- NOTE: No support for string, lists
-- TODO: Predefine true false ! not(X) \+ halt clauses and (.) list constructor
type Program = [Clause]
-- BoundVars (any other var is free var)
type Context = [(Var, Term)]

-- All types that act like a term
instance IsTerm Const where
  asTerm c = AtomicT $ ConstT c

instance IsTerm Var where
  asTerm v = VarT v

instance IsTerm AtomicTerm where
  asTerm at = AtomicT at

instance IsTerm CompoundTerm where
  asTerm ct = CompoundT ct

instance IsTerm ClauseHead where
  asTerm ch = case ch of
    CompoundHead ct -> asTerm ct
    ConstHead ident -> asTerm $ Const ident

anonymousVar :: Var
anonymousVar = Var "_"

freeToAnonymousVars :: Term -> Term
freeToAnonymousVars t = case t of
  (VarT _) -> VarT anonymousVar
  (AtomicT _) -> t
  (CompoundT ct) -> CompoundT $ CompoundTerm (functor ct) $ map freeToAnonymousVars $ args ct

-- Bind free vars in the term with values from context
bindTermVars :: Term -> Context -> Term
bindTermVars t ctxt = case t of
  VarT var -> fromMaybe t (lookupVar var ctxt)
  CompoundT CompoundTerm {functor, args} -> CompoundT $ CompoundTerm {
    functor = functor,
    args = map (`bindTermVars` ctxt) args
  }
  _ -> t -- AtomicTerms dont contain Vars

lookupVar :: Var -> Context -> Maybe Term
lookupVar v = foldr (\(x, term) acc -> if v == x
    then Just term
    else acc
  ) Nothing

lookupClause :: Term -> Program -> [Clause]
lookupClause t = filter validClause
  where
    validClause c = case c of
      Fact ch -> validTerm t $ asTerm ch
      Rule ch _ -> validTerm t $ asTerm ch
      PredefC pc -> validTerm t $ asTerm $ predefCH pc

    validTerm t1 t2 = case (t1, t2) of
      (VarT _, _) -> True
      (_, VarT _) -> True
      (AtomicT at1, AtomicT at2) -> at1 == at2
      (CompoundT ct1, CompoundT ct2) ->
        functor ct1 == functor ct2 && and (zipWith validTerm (args ct1) (args ct2))
      _ -> False

-- Extend Ctxt along with the Var to Var
extend_ :: Var -> Term -> Context -> Context
extend_ v t ctxt = (v, t) : filter (fst >.> (/= v)) ctxt

-- Extend Ctxt without Var to Var
extend :: Var -> Term -> Context -> Context
extend _ (VarT _) ctxt = ctxt
extend v t ctxt = extend_ v t ctxt

-- Recursive extend Ctxt without Var tp Var
extendTerm :: Term -> Term -> Context -> Maybe Context
extendTerm = extendTerm_ False

-- Extends context for all free vars in term1 with values from term2
extendTerm_ :: Bool -> Term -> Term -> Context -> Maybe Context
extendTerm_ extendVars (VarT var) t ctxt = Just $ (if extendVars then extend_ else extend) var t ctxt
extendTerm_ _ (AtomicT _) (VarT _) ctxt = Just ctxt
extendTerm_ _ (AtomicT at1) (AtomicT at2) ctxt = if at1 == at2 then Just ctxt else Nothing
extendTerm_ _ (AtomicT _) (CompoundT _) _ = Nothing
extendTerm_ _ (CompoundT _) (VarT _) ctxt = Just ctxt
extendTerm_ _ (CompoundT _) (AtomicT _) _ = Nothing
extendTerm_ extendVars (CompoundT ct1) (CompoundT ct2) ctxt = if length (args ct1) == length (args ct2)
  then if CompoundT ct1 < CompoundT ct2
    then foldr
      (\(a1, a2) acc -> case acc of
        Nothing -> Nothing
        Just acc' -> let a1' = bindTermVars a1 acc' in if a1' < a2 -- NOTE: Not sure if bindTermVars is usefull here maybe for a1' < a2
          then extendTerm_ extendVars a1' a2 acc'
          else acc
      )
      (Just ctxt) $ zip (args ct1) (args ct2)
    else Just ctxt
  else Nothing


swapCtxt :: Context -> QueryRes -> QueryRes
swapCtxt ctxt res = (,) ctxt . snd <$> res

unifyCtxt ::Context -> Context ->Context
unifyCtxt c1 c2 = uniq (c1 ++ c2)

instance Callable AtomicTerm where
  call :: Program -> Context -> AtomicTerm -> [QueryRes]
  call prog ctxt t = case t of
    ConstT _ -> case lookupClause (AtomicT t) prog of
        [] -> [Left $ Error "Missing clause"]
        clauses -> foldr (clauseRes >.> (++)) [] clauses
      where
        clauseRes clause = case clause of
          Fact _ -> [Right (ctxt, True)]
          Rule _ gl -> map (swapCtxt ctxt) (evalOrQuery prog [] gl)
          PredefC (PredefClause _ callClause) -> map (swapCtxt ctxt) (callClause prog []) -- swaping because this is clause without arguments and we dont want its context

    IntT _ -> [Left $ Error "Numbers are not callable"]
    FloatT _ -> [Left $ Error "Numbers are not callable"]

instance Callable CompoundTerm where
  call :: Program -> Context -> CompoundTerm -> [QueryRes]
  call prog ctxt CompoundTerm {functor = f1, args = args1} =
    let
      boundArgs1 = map (`bindTermVars` ctxt) args1

      newCtxtFor :: Id -> [Term] -> Maybe Context
      newCtxtFor f2 args2 = extendTerm
          (CompoundT $ CompoundTerm f2 args2)
          (CompoundT $ CompoundTerm f1 boundArgs1) []

      -- NOTE: Bind args before use this func
      newCtxtFrom :: Id -> [Term] -> Maybe Context
      newCtxtFrom f2 args2 = extendTerm_ True -- include vars swap
          (CompoundT $ CompoundTerm f1 boundArgs1)
          (CompoundT $ CompoundTerm f2 args2) ctxt

      -- combine the result of body goals and args
      -- first arg of the function is body Ctxt
      bodyToArgs f2 args2 bodyRes = case bodyRes of
          (Right (ctxt', res)) ->
            case (newCtxtFrom f2 boundArgs2, res) of
              (Just argsCtxt, True) -> Right (unifyCtxt argsCtxt ctxt, True)
              _ -> Right (ctxt, False) -- "ctxt = unifyCtxt [] ctxt"     maybe it wiil return Nothing if args are different count but we handle this from lookup for clauses
            where
              boundArgs2 = map (`bindTermVars` ctxt') args2
          left -> left

    in case lookupClause (CompoundT $ CompoundTerm f1 args1) prog of
      [] -> [Left $ Error "Missing clause"]
      clauses -> foldr (clauseRes >.> (++)) [] clauses
        where
          clauseRes clause = case clause of

            Fact ch -> case asTerm ch of
              CompoundT CompoundTerm {functor = f2, args = args2} ->
                case newCtxtFor f2 args2 of
                  Nothing -> [Right (ctxt, False)]
                  Just new_ctxt' -> case newCtxtFrom f2 (map (`bindTermVars` new_ctxt') args2) of
                    Just new_ctxt'' -> [Right (new_ctxt'', True)]
                    Nothing -> [Right (ctxt, False)]

              -- case for zero arguments CompoundTerm
              AtomicT _ -> [Right (ctxt, True)]

              _ -> [] -- maybe it will never reach this line (if lookup works properly)

            -- NOTE: !!! When return extended ctxt dont extend it with vars in body of Program clause just extended vars from head
            Rule ch gl -> case asTerm ch of
              CompoundT CompoundTerm {functor = f2, args = args2} ->
                case newCtxtFor f2 args2 of
                  Nothing -> [Right (ctxt, False)]
                  Just new_ctxt' -> map (bodyToArgs f2 args2) (evalOrQuery prog new_ctxt' gl)

              -- case for zero arguments CompoundTerm
              AtomicT _ -> map (swapCtxt ctxt) (evalOrQuery prog [] gl)

              _ -> [] -- maybe it will never reach this line (if lookup works properly)

            PredefC (PredefClause ch callClause) -> case asTerm ch of
              CompoundT CompoundTerm {functor = f2, args = args2} ->
                case newCtxtFor f2 args2 of
                  Nothing -> [Right (ctxt, False)]
                  Just new_ctxt' -> map (bodyToArgs f2 args2) (callClause prog new_ctxt')

              AtomicT _ -> map (swapCtxt ctxt) (callClause prog [])

              _ -> []

instance Callable Term where
  call prog ctxt t = case t of
    AtomicT at -> call prog ctxt at
    CompoundT ct -> call prog ctxt ct
    VarT var -> case lookupVar var ctxt of
      Just term -> call prog (filter (fst >.> (/= var)) ctxt) term
      Nothing -> [Left $ Error "Variables are not callable"] -- NOTE: not sure if error should be [] or [..Nothing]



-- [ "true,false ; true" , "false, false" ]
eval :: Program -> [GoalList] -> IO Info
eval _ [] = return Ok
eval prog queries = fromMaybe Ok . find (== EndInterpreter) <$> mapM go queries
  where
    go :: GoalList -> IO Info
    go query = case evalOrQuery prog [] query of
        [] -> putStrLn "You try to execute empty query" >> return Ok
        res -> do
          res' <- printRes res
          putStr "\n"
          return res'
      where
        printRes :: [QueryRes] -> IO Info
        printRes [] = do
          putChar '.'
          return Ok
        printRes (res':rest) = do
          retVal <- case res' of
            Right (ctxt, True) -> if null ctxt
              then putStr "true " >> return Ok
              else putStr (foldr (\(v, t) acc -> varName v ++ " = " ++ show t ++ (if not $ null acc then " \n" else " ") ++ acc) "" ctxt) >> return Ok
            Right (_, False) -> putStr "false " >> return Ok
            Left (Error msg) -> putStr msg >> return Ok
            Left Ok -> return Ok
            Left (Cut ctxt) -> return (Cut ctxt)
            Left EndInterpreter -> return EndInterpreter -- not working because have to be connected with Lib.hs
          
          case retVal of
            EndInterpreter -> return EndInterpreter
            val -> if null rest
              then return val
              else waitForIn rest
          
          
        waitForIn :: [QueryRes] -> IO Info
        waitForIn rest = do
          hSetBuffering stdin NoBuffering
          continue <- getChar
          hSetBuffering stdin LineBuffering

          putChar '\n'
          if continue == ';' then
            printRes rest
          else if continue == '.' then
            return Ok
          else waitForIn rest


evalOrQuery :: Program -> Context -> GoalList -> [QueryRes] -- example why returns Context "true, (X=5;X=6), write(X)."
evalOrQuery prog ctxt = foldr (evalAndQuery prog ctxt >.> reduceFalse) []
  where
    reduceFalse :: [QueryRes] -> [QueryRes] -> [QueryRes]
    reduceFalse andRes acc = foldr (\x acc' -> case x of
        Right (_, False) -> if null acc' then [x] else acc'
        Right (_, True) -> x : acc'
        Left (Cut (_, nextRes)) -> reduceFalse nextRes []
        left -> [left]
      ) [] (andRes ++ acc)

evalAndQuery :: Program -> Context -> [Goal] -> [QueryRes] -- NOTE: return [Bool] because of the OrQuery in EnclosedGoal
evalAndQuery prog ctxt andQuery = case andQuery of -- foldr f Nothing andQuery
  [] -> [Right (ctxt, True)]

  -- for each result of goal eval next goals
  (goal:gs) -> foldr (\x acc -> case x of
      Right (ctxt', True) -> evalAndQuery prog ctxt' gs ++ acc
      Right (_, False) -> x:acc
      Left (Cut (ctxt', [])) -> [ Left $ Cut (ctxt', evalAndQuery prog ctxt' gs) ]
      left -> [left]
    ) [] $ evalGoal prog ctxt goal

evalGoal :: Program -> Context -> Goal -> [QueryRes] -- NOTE: returns [] because of the OrQuery in EnclosedGoal
evalGoal prog ctxt g = case g of -- NOTE: use ctxt because on andQuery "X = 5, goal(X)."/ "likes(X, ivanka), write(X)." goal(X) depend on the new bind for X
  ClauseGoal ch -> callTerm prog ctxt ch
  VarGoal v -> callTerm prog ctxt v
  EnclosedGoal gl -> evalOrQuery prog ctxt gl



  -- ExprGoal expr -> (:[]) <$> callTerm ctxt expr -- X is 5 / X = 5


-- Predefined clauses
trueClause :: Clause
trueClause = PredefC (PredefClause { predefCH, callPredef })
  where
    predefCH :: ClauseHead
    predefCH = ConstHead (Id "true")

    callPredef :: Program -> Context -> [QueryRes]
    callPredef _ ctxt = [Right (ctxt, True)]

falseClause :: Clause
falseClause = PredefC (PredefClause { predefCH, callPredef })
  where
    predefCH :: ClauseHead
    predefCH = ConstHead (Id "false")

    callPredef :: Program -> Context -> [QueryRes]
    callPredef _ _ = [Right ([], False)]

notClause :: Clause
notClause = PredefC (PredefClause { predefCH, callPredef })
  where
    x = Var "X"

    predefCH :: ClauseHead
    predefCH = CompoundHead $ CompoundTerm { functor = Id "not", args = [asTerm x] }

    callPredef :: Program -> Context -> [QueryRes]
    callPredef prog ctxt = case xVal of
      Nothing -> [Left $ Error "Missing clause"]
      Just t -> map (second not <$>) $ call prog ctxt t
      where
        xVal = lookupVar x ctxt

-- ! - if all before cut are true it execute rest and stop
--   - if all before cut are false the cut is not called and continue
cutClause :: Clause
cutClause = PredefC (PredefClause { predefCH, callPredef })
  where
    predefCH :: ClauseHead
    predefCH = ConstHead $ Id "!"

    callPredef :: Program -> Context -> [QueryRes]
    callPredef _ ctxt = [Left $ Cut (ctxt, [])]

haltClause :: Clause
haltClause = PredefC (PredefClause { predefCH, callPredef })
  where
    predefCH :: ClauseHead
    predefCH = ConstHead (Id "halt")

    callPredef :: Program -> Context -> [QueryRes]
    callPredef _ _ = [Left EndInterpreter]

predefClauses :: [Clause]
predefClauses = [trueClause, falseClause, notClause, cutClause, haltClause]



-- -- compare terms (from args: left-queryArg right-databaseArg) and asign left one with the second in the context
-- retrieveTerm :: Term -> Term -> Maybe (Context, Bool)
-- retrieveTerm (VarT v) t = Just ([(v, t)], True) -- add "freeToAnonymousVars t"

-- retrieveTerm (AtomicT _) (VarT _) = Just ([], True)
-- retrieveTerm (AtomicT at) (AtomicT at') = Just ([], at == at')
-- retrieveTerm (AtomicT _) (CompoundT _) = trace "here" Nothing

-- retrieveTerm (CompoundT _) (VarT _) = Just ([], True)
-- retrieveTerm (CompoundT _) (AtomicT _) = Nothing
-- retrieveTerm (CompoundT ct) (CompoundT ct') =
--   if length (args ct) == length (args ct')
--     then let
--         -- not used but another way to check if hasMultBindings (with proof)
--         _foldDiffBindings :: Context -> Context -> [(Var, [Term])]
--         _foldDiffBindings c c' = foldr foldVarBindings [] $ uniq (c ++ c')
--           where
--             foldVarBindings (v, t) [] = [(v, [t])]
--             foldVarBindings (v, t) ((v', ts) : xs) = if v == v'
--               then (v', t:ts) : xs
--               else (v', ts) : foldVarBindings (v, t) xs

--         hasMultBindings :: Context -> Context -> Bool
--         hasMultBindings c c' = uc /= uniq uc
--           where
--             uc = map fst $ uniq (c ++ c')

--         retrieveArgs :: (Term, Term) -> Maybe (Context, Bool) -> Maybe (Context, Bool)
--         retrieveArgs (a, a') acc =
--           case acc of
--             Nothing -> Nothing
--             Just (accCtxt, accRes) -> if accRes
--               then case retrieveTerm a a' of
--                 Nothing -> Nothing
--                 Just (_, False) -> Just ([], False)
--                 Just (ctxt', True) -> if hasMultBindings ctxt' accCtxt -- t(X, g(X))  t(a, g(b))
--                   then Just ([], False)
--                   else Just (uniq (ctxt' ++ accCtxt), True)
--               else Just ([], False)
--       in
--         foldr retrieveArgs (Just ([], True)) $ zip (args ct) (args ct')

--     else Nothing


-- foldDiffBindings :: Context -> Context -> [(Var, [Term])]
-- foldDiffBindings c c' = foldr foldVarBindings [] (c ++ c')
--   where
--     foldVarBindings :: (Var, Term) -> [(Var, [Term])] -> [(Var, [Term])]
--     foldVarBindings (v, t) l = foldr (\(v', ts) l' -> if v == v' && t `notElem` ts then (v', t:ts) : l' else (v', ts) : l') [] l