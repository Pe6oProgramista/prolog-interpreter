{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
module Terms where

import Prelude hiding (head)
import Prelude (id)
import Data.Maybe (fromMaybe)

import Utils ( (>.>), uniq, getLine', maybeAndThen )

import Debug.Trace (trace)
import Control.Monad (foldM_, when, unless)
import System.IO (BufferMode(LineBuffering, NoBuffering), hSetBuffering, stdin)
import Data.Foldable (asum)

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
  call :: Program -> Context -> a -> [Maybe (Context, Bool)]

class IsTerm a where
  asTerm :: a -> Term

  callTerm :: Program -> Context -> a -> [Maybe (Context, Bool)]
  callTerm prog ctxt = call prog ctxt . asTerm


type Atom = String

newtype Id = Id { idAtom :: Atom }
  deriving (Eq)

newtype Const = Const { constId :: Id }
  deriving (Eq)

newtype Var = Var { varName :: String }
  deriving (Eq)

data AtomicTerm = ConstT Const | IntT Int | FloatT Float
  deriving (Eq)

data CompoundTerm = CompoundTerm { functor :: Id, args :: [Term] }
  deriving (Eq)

data Term = VarT Var | AtomicT AtomicTerm | CompoundT CompoundTerm --  | PredefT PredefTerm
  deriving (Eq)

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

data PredefClause = PredefClause {
    predefCH :: ClauseHead,
    callPredef :: Program -> Context -> [Maybe (Context, Bool)] }

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

lookupClause :: Id -> Int -> Program -> [Clause]
lookupClause ident arity = filter (((ident, arity) ==) . pairIdArity)
  where
    pairIdArity clause = case clause of
      (Fact head) -> pairIdArity' head
      (Rule head _) -> pairIdArity' head
      (PredefC (PredefClause head _)) -> pairIdArity' head
      where
        pairIdArity' (CompoundHead cterm) = (functor cterm, length $ args cterm)
        pairIdArity' (ConstHead headId) = (headId, 0)

extend :: Var -> Term -> Context -> Context
extend _ (VarT _) ctxt = ctxt
extend v t ctxt = (v, t) : filter (fst >.> (/= v)) ctxt

-- Extends context for all free vars in term1 with values from term2
extendTerm :: Term -> Term -> Context -> Maybe Context
extendTerm (VarT var) t ctxt = Just $ extend var t ctxt
extendTerm (AtomicT _) (VarT _) ctxt = Just ctxt
extendTerm (AtomicT at1) (AtomicT at2) ctxt = if at1 == at2 then Just ctxt else Nothing
extendTerm (AtomicT _) (CompoundT _) _ = Nothing
extendTerm (CompoundT _) (VarT _) ctxt = Just ctxt
extendTerm (CompoundT _) (AtomicT _) _ = Nothing
extendTerm (CompoundT ct1) (CompoundT ct2) ctxt = if length (args ct1) == length (args ct2)
  then if CompoundT ct1 < CompoundT ct2
    then foldr
      (\(a1, a2) acc -> case acc of
        Nothing -> Nothing
        Just acc' -> let a1' = bindTermVars a1 acc' in if a1' < a2 -- NOTE: Not sure if bindTermVars is usefull here maybe for a1' < a2
          then extendTerm a1' a2 acc'
          else acc
      )
      (Just ctxt) $ zip (args ct1) (args ct2)
    else Just ctxt
  else Nothing

-- compare terms (from args: left-queryArg right-databaseArg) and asign left one with the second in the context
retrieveTerm :: Term -> Term -> Maybe (Context, Bool)
retrieveTerm (VarT v) t = Just ([(v, t)], True) -- add "freeToAnonymousVars t"

retrieveTerm (AtomicT _) (VarT _) = Just ([], True)
retrieveTerm (AtomicT at) (AtomicT at') = Just ([], at == at')
retrieveTerm (AtomicT _) (CompoundT _) = trace "here" Nothing

retrieveTerm (CompoundT _) (VarT _) = Just ([], True)
retrieveTerm (CompoundT _) (AtomicT _) = Nothing
retrieveTerm (CompoundT ct) (CompoundT ct') =
  if length (args ct) == length (args ct')
    then let
        -- not used but another way to check if hasMultBindings (with proof)
        _foldDiffBindings :: Context -> Context -> [(Var, [Term])]
        _foldDiffBindings c c' = foldr foldVarBindings [] $ uniq (c ++ c')
          where
            foldVarBindings (v, t) [] = [(v, [t])]
            foldVarBindings (v, t) ((v', ts) : xs) = if v == v'
              then (v', t:ts) : xs
              else (v', ts) : foldVarBindings (v, t) xs

        hasMultBindings :: Context -> Context -> Bool
        hasMultBindings c c' = uc /= uniq uc
          where
            uc = map fst $ uniq (c ++ c')

        retrieveArgs :: (Term, Term) -> Maybe (Context, Bool) -> Maybe (Context, Bool)
        retrieveArgs (a, a') acc =
          case acc of
            Nothing -> Nothing
            Just (accCtxt, accRes) -> if accRes
              then case retrieveTerm a a' of
                Nothing -> Nothing
                Just (_, False) -> Just ([], False)
                Just (ctxt', True) -> if hasMultBindings ctxt' accCtxt -- t(X, g(X))  t(a, g(b))
                  then Just ([], False)
                  else Just (uniq (ctxt' ++ accCtxt), True)
              else Just ([], False)
      in
        foldr retrieveArgs (Just ([], True)) $ zip (args ct) (args ct')

    else Nothing


swapCtxt :: Maybe (Context, Bool) -> Context -> Maybe (Context, Bool)
swapCtxt res ctxt = (,) ctxt . snd <$> res

unifyCtxt :: Maybe (Context, Bool) -> Context -> Maybe (Context, Bool)
unifyCtxt Nothing  _ = Nothing
unifyCtxt (Just (c1, res)) c2 = Just (uniq (c1 ++ c2), res)

instance Callable AtomicTerm where
  call :: Program -> Context -> AtomicTerm -> [Maybe (Context, Bool)]
  call prog ctxt t = case t of
    ConstT co -> case lookupClause (constId co) 0 prog of
        [] -> [Nothing]
        clauses -> foldr appendClauseRes [] clauses
      where
        appendClauseRes clause acc = case clause of
          Fact _ -> Just (ctxt, True) : acc
          Rule _ gl -> map (`swapCtxt` ctxt) (evalOrQuery prog [] gl) ++ acc
          PredefC (PredefClause _ callClause) -> map (`swapCtxt` ctxt) (callClause prog []) ++ acc -- swaping because this is clause without arguments and we dont want its context
    IntT _ -> [Nothing]
    FloatT _ -> [Nothing]

instance Callable CompoundTerm where
  call :: Program -> Context -> CompoundTerm -> [Maybe (Context, Bool)]
  call prog ctxt CompoundTerm {functor = f, args = as} = case lookupClause f (length as) prog of
    [] -> [Nothing]
    clauses -> foldr appendClauseRes [] clauses
      where
        bindedArgs = map (`bindTermVars` ctxt) as

        appendClauseRes clause acc = case clause of
          Fact ch -> case asTerm ch of
            CompoundT CompoundTerm {functor = f', args = as'} ->
              let
                new_ctxt = extendTerm
                    (CompoundT $ CompoundTerm f' as')
                    (CompoundT $ CompoundTerm f  bindedArgs) []

                retrieveCurrArgs ctxt' = retrieveTerm
                    (CompoundT $ CompoundTerm f  bindedArgs)
                    (CompoundT $ CompoundTerm f' (map (`bindTermVars` ctxt') as'))
              in case new_ctxt of
                Nothing -> Just (ctxt, False) : acc
                Just new_ctxt' -> case retrieveCurrArgs new_ctxt' of
                  Nothing -> Just (ctxt, False) : acc
                  res -> unifyCtxt res ctxt : acc

            -- case for zero arguments CompoundTerm
            AtomicT _ -> Just (ctxt, True) : acc

            _ -> acc -- maybe it will never reach this line (if lookup works properly)

          -- NOTE: !!! When return extended ctxt dont extend it with vars in body of Program clause just extended vars from head
          Rule ch gl -> case asTerm ch of
            CompoundT CompoundTerm {functor = f', args = as'} ->
              let
                new_ctxt = extendTerm
                    (CompoundT $ CompoundTerm f' as')
                    (CompoundT $ CompoundTerm f  bindedArgs) []

                retrieveCurrArgs ctxt' = retrieveTerm
                    (CompoundT $ CompoundTerm f  bindedArgs)
                    (CompoundT $ CompoundTerm f' (map (`bindTermVars` ctxt') as'))

                -- combine the result of body goals and args
                -- first arg of the function is body Ctxt
                combineBodyArgs Nothing = Nothing
                combineBodyArgs (Just (ctxt', res)) =
                  case retrieveCurrArgs ctxt' of
                    Just (argsCtxt, res') -> if res' && res
                      then Just (argsCtxt, True)
                      else Just ([], False)
                    _ -> Nothing
              in case new_ctxt of
                Nothing -> Just (ctxt, False) : acc
                Just new_ctxt' -> map ((`unifyCtxt` ctxt) . combineBodyArgs)  (evalOrQuery prog new_ctxt' gl) ++ acc

             -- case for zero arguments CompoundTerm
            AtomicT _ -> map (`swapCtxt` ctxt) (evalOrQuery prog [] gl) ++ acc

            _ -> acc -- maybe it will never reach this line (if lookup works properly)

          PredefC (PredefClause ch callClause) -> case asTerm ch of
            CompoundT CompoundTerm {functor = f', args = as'} ->
              let
                new_ctxt = extendTerm
                    (CompoundT $ CompoundTerm f' as')
                    (CompoundT $ CompoundTerm f  bindedArgs) []

                retrieveCurrArgs ctxt' = retrieveTerm
                    (CompoundT $ CompoundTerm f  bindedArgs)
                    (CompoundT $ CompoundTerm f' (map (`bindTermVars` ctxt') as'))

                -- combine the result of body goals and args
                -- first arg of the function is body Ctxt
                combineBodyArgs Nothing = Nothing
                combineBodyArgs (Just (ctxt', res)) =
                  case retrieveCurrArgs ctxt' of
                    Just (argsCtxt, res') -> if res' && res
                      then Just (argsCtxt, True)
                      else Just ([], False)
                    _ -> Nothing
              in case new_ctxt of
                Nothing -> Just (ctxt, False) : acc
                Just new_ctxt' -> map ((`unifyCtxt` ctxt) . combineBodyArgs) (callClause prog new_ctxt') ++ acc

            AtomicT _ -> map (`swapCtxt` ctxt) (callClause prog []) ++ acc

            _ -> acc

instance Callable Term where
  call prog ctxt t = case t of
    AtomicT at -> call prog ctxt at
    CompoundT ct -> call prog ctxt ct
    VarT var -> case lookupVar var ctxt of
      Just term -> call prog (filter (\(var', _) -> var' /= var) ctxt) term
      Nothing -> [Nothing] -- NOTE: not sure if error should be [] or [..Nothing]

-- [ "true,false ; true" , "false, false" ]
eval :: Program -> [GoalList] -> IO ()
eval _ [] = return ()
eval prog queries = mapM_ go queries
  where
    go :: GoalList -> IO ()
    go query = case evalOrQuery prog [] query of
        [] -> putStrLn "ERROR."
        res -> do
          printRes res
          putStr "\n\n" -- (snd >.> printRes >.> putStrLn)
      where
        printRes :: [Maybe (Context, Bool)] -> IO ()
        printRes [] = return()
        printRes (res':rest) = do
          putStr $ case res' of
            Just (ctxt, True) -> if null ctxt
              then "true."
              else foldr (\(v, t) acc -> varName v ++ " = " ++ show t ++ (if not $ null acc then "\n" else "") ++ acc) "" ctxt
            Just (_, False) -> "false."
            _ -> "ERROR"
          unless (null rest) waitForIn
          where
            waitForIn :: IO ()
            waitForIn = do
              hSetBuffering stdin NoBuffering
              continue <- getChar
              hSetBuffering stdin LineBuffering

              putChar '\n'
              if continue == ';'
                then printRes rest
                else unless (continue == '.') waitForIn


evalOrQuery :: Program -> Context -> GoalList -> [Maybe (Context, Bool)] -- example why returns Context "true, (X=5;X=6), write(X)."
evalOrQuery prog ctxt = foldr (evalAndQuery prog ctxt >.> reduceFalse) []
  where
    reduceFalse :: [Maybe (Context, Bool)] -> [Maybe (Context, Bool)] -> [Maybe (Context, Bool)]
    reduceFalse andRes acc = foldr (\x acc' -> case x of
        Just (_, False) -> if null acc' then [x] else acc'
        Just (_, True) -> x : acc'
        Nothing -> [Nothing]
      ) [] (andRes ++ acc)

evalAndQuery :: Program -> Context -> [Goal] -> [ Maybe (Context, Bool)] -- NOTE: return [Bool] because of the OrQuery in EnclosedGoal
evalAndQuery prog ctxt andQuery = case andQuery of -- foldr f Nothing andQuery
  [] -> [Just (ctxt, True)]

  -- for each result of goal eval next goals
  (goal:gs) -> foldr (\x acc -> case x of
          Just (ctxt', True) -> evalAndQuery prog ctxt' gs ++ acc
          Just (_, False) -> x:acc
          _ -> [Nothing]
        ) [] $ evalGoal prog ctxt goal

evalGoal :: Program -> Context -> Goal -> [Maybe (Context, Bool)] -- NOTE: returns [] because of the OrQuery in EnclosedGoal
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

    callPredef :: Program -> Context -> [Maybe (Context, Bool)]
    callPredef _ ctxt = [Just (ctxt, True)]

falseClause :: Clause
falseClause = PredefC (PredefClause { predefCH, callPredef })
  where
    predefCH :: ClauseHead
    predefCH = ConstHead (Id "false")

    callPredef :: Program -> Context -> [Maybe (Context, Bool)]
    callPredef _ _ = [Just ([], False)]

notClause :: Clause
notClause = PredefC (PredefClause { predefCH, callPredef })
  where
    x = Var "X"

    predefCH :: ClauseHead
    predefCH = CompoundHead $ CompoundTerm { functor = Id "not", args = [asTerm x] }

    callPredef :: Program -> Context -> [Maybe (Context, Bool)]
    callPredef prog ctxt = case xVal of
      Nothing -> [Nothing]
      Just t -> map ((\(c, b) -> (c, not b)) <$>) $ call prog ctxt t
      where
        xVal = lookupVar x ctxt

predefClauses :: [Clause]
predefClauses = [trueClause, falseClause, notClause]






-- foldDiffBindings :: Context -> Context -> [(Var, [Term])]
-- foldDiffBindings c c' = foldr foldVarBindings [] (c ++ c')
--   where
--     foldVarBindings :: (Var, Term) -> [(Var, [Term])] -> [(Var, [Term])]
--     foldVarBindings (v, t) l = foldr (\(v', ts) l' -> if v == v' && t `notElem` ts then (v', t:ts) : l' else (v', ts) : l') [] l