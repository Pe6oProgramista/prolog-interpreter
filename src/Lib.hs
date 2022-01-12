{-# LANGUAGE InstanceSigs #-} -- типова сигнатура на функции в instance
{-# LANGUAGE NamedFieldPuns #-} -- позволява ни да изпускаме изричното наименуване на полетата
-- {-# LANGUAGE FlexibleInstances #-}
{-# Language ScopedTypeVariables #-} -- use rigid type variables in where/let
{-# LANGUAGE RankNTypes #-} -- forall
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- module Lib
--     ( hello,
--       someInt
--     ) where

module Lib where

import Utils
import Data.Char ( isLower, isUpper, isAlpha, isDigit, isSpace, toUpper )
import Data.Tuple ( swap )
import Control.Applicative ( Alternative ( empty, (<|>), many, some ), Applicative (liftA2) )
import Data.Foldable ( asum )
import Text.Read ( readMaybe )

import System.IO
import Control.Monad
import Data.Maybe (fromMaybe)

import Prelude hiding (head, id)
import Data.List (foldl')
import Debug.Trace (trace)

hello :: IO ()
hello = putStrLn "Hello from Lib!"

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

-- NOTE: Maybe there are better ways for doing that

class Callable a where
  -- TODO: add multiple output for printing variables if its generator term
  -- returns array because some terms may map to Rule that has GoalList with more than one andQuery
  call :: Program -> Context -> a -> [(Context, Maybe Bool)]

class IsFunctor a where -- (Callable a) =>
  getArity :: a -> Int
  getFunctor :: a -> String -- returns string because of Numbers (they are not Ids)

class IsTerm a where
  asTerm :: a -> Term

  callTerm :: Program -> Context -> a -> [(Context, Maybe Bool)]
  callTerm prog ctxt = call prog ctxt . asTerm

type Atom = String

newtype Id = Id { idAtom :: Atom }
  deriving (Show, Eq)

newtype Const = Const { constId :: Id }
  deriving (Show, Eq)

newtype Var = Var { varName :: String }
  deriving (Show, Eq)

-- instance Eq Var where
--   (==) _ _ = True

data AtomicTerm = ConstT Const | IntT Int | FloatT Float
  deriving (Show, Eq)

data CompoundTerm = CompoundTerm { functor :: Id, args :: [Term] }
  deriving (Show, Eq)

data Term = VarT Var | AtomicT AtomicTerm | CompoundT CompoundTerm
  deriving (Show, Eq)

validToAsign :: Term -> Term -> Bool
validToAsign (VarT _) _ = True
validToAsign (AtomicT at) (AtomicT at') = at == at'
validToAsign (AtomicT _) _ = False
validToAsign (CompoundT ct) (CompoundT ct') =
    (length args1 == length args2) &&
    foldr (\(a1,a2) acc -> validToAsign a1 a2 && acc) True (zip args1 args2)
  where
    args1 = args ct
    args2 = args ct'
validToAsign (CompoundT _) _ = False

-- Orders Terms by how deep they are (right is deeper if "left <= right" returns True)
instance Ord Term where
  (<=) (VarT _) _ = True
  (<=) (AtomicT _) (AtomicT _) = True
  (<=) (AtomicT _) (CompoundT _) = True
  (<=) (CompoundT (CompoundTerm _ args1)) (CompoundT (CompoundTerm _ args2)) = case args1 of
    [] -> True
    args1' -> case args2 of
      [] -> False
      args2' -> a1 <= a2
        where
          a1 = foldr1 (\x acc -> if x <= acc then acc else x) args1'
          a2 = foldr1 (\x acc -> if x <= acc then acc else x) args2'

  (<=) x y = not $ y <= x

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

data Clause
  = Fact ClauseHead
  | Rule ClauseHead GoalList
  deriving (Show, Eq)

-- NOTE: No support for string, lists
-- TODO: Predefine true false ! not(X) \+ halt clauses and (.) list constructor
type Program = [Clause]
-- BoundVars (any other var is from FreeVars)
type Context = [(Var, Term)]

lookupVar :: Var -> Context -> Maybe Term
lookupVar v = foldr (\(x, term) acc -> if v == x
    then Just term
    else acc
  ) Nothing

anonymousVar :: Var
anonymousVar = Var "_"

freeToAnonymousVars :: Term -> Term
freeToAnonymousVars t = case t of
  (VarT _) -> VarT anonymousVar
  (AtomicT _) -> t
  (CompoundT ct) -> CompoundT $ CompoundTerm (functor ct) $ map freeToAnonymousVars $ args ct

extend :: Var -> Term -> Context -> Context
extend _ (VarT _) ctxt = ctxt
extend v t ctxt = (v, t) : ctxt

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

lookupClause :: Id -> Int -> Program -> [Clause]
lookupClause id arity = filter (((id, arity) ==) . pairIdArity)
  where
    pairIdArity clause = case clause of
      (Fact head) -> pairIdArity' head
      (Rule head _) -> pairIdArity' head
      where
        pairIdArity' (CompoundHead cterm) = (functor cterm, length $ args cterm)
        pairIdArity' (ConstHead headId) = (headId, 0)

instance Callable AtomicTerm where
  call prog ctxt t = case t of
    ConstT co -> case lookupClause (constId co) 0 prog of
        [] -> [(ctxt, Nothing)]
        clauses -> foldr appendClauseRes [] clauses
      where
        appendClauseRes clause acc = case clause of
          Fact _ -> (ctxt, Just True) : acc
          Rule _ gl -> evalOrQuery prog ctxt gl ++ acc
    IntT _ -> [(ctxt, Nothing)]
    FloatT _ -> [(ctxt, Nothing)]

debug :: c -> String -> c
debug = flip trace

instance Callable CompoundTerm where
  call prog ctxt CompoundTerm {functor = f, args = as} = case lookupClause f (length as) prog of
    [] -> [(ctxt, Nothing)]
    clauses -> foldr appendClauseRes [] clauses
      where
        appendClauseRes clause acc =
          let
            foldDiffBindings :: Context -> Context -> [(Var, [Term])]
            foldDiffBindings c c' = foldr foldVarBindings [] (c ++ c')
              where
                foldVarBindings :: (Var, Term) -> [(Var, [Term])] -> [(Var, [Term])]
                foldVarBindings (v, t) l = foldr (\(v', ts) l' -> if v == v' && t `notElem` ts then (v', t:ts) : l' else (v', ts) : l') [] l

            -- compare terms (from args: left-queryArg right-databaseArg) and asign left one with the second in the context
            retrieveTerm :: Term -> Term -> (Context, Maybe Bool)
            retrieveTerm (VarT v) t = ([(v, freeToAnonymousVars t)], Just True)
            retrieveTerm (AtomicT _) (VarT _) = ([], Just True)
            retrieveTerm (AtomicT at) (AtomicT at') = ([], Just $ at == at')
            retrieveTerm (AtomicT _) (CompoundT _) = ([], Nothing)
            retrieveTerm (CompoundT _) (VarT _) = ([], Just True)
            retrieveTerm (CompoundT _) (AtomicT _) = ([], Nothing)
            retrieveTerm (CompoundT ct) (CompoundT ct') = if length (args ct) == length (args ct')
              then foldr (\(a, a') (accCtxt, accRes) -> case accRes of
                Nothing -> ([], Nothing)
                Just False -> ([], Just False)
                Just True -> case retrieveTerm a a' of
                  (_, Nothing) -> ([], Nothing)
                  (_, Just False) -> ([], Just False)
                  (ctxt', Just True) -> case foldDiffBindings ctxt' accCtxt of -- t(X, g(X))  t(a, g(b))
                    [] -> (ctxt' ++ accCtxt, Just True)
                    _ -> ([], Just False)
                )
                ([], Just True) $ zip (args ct) (args ct')
              else ([], Nothing)
          in case clause of
            Fact ch -> case asTerm ch of
              CompoundT CompoundTerm {functor = f', args = as'} ->
                let
                  new_ctxt = extendTerm (CompoundT $ CompoundTerm f' as')
                                        (CompoundT $ CompoundTerm f  as ) ctxt
                in case new_ctxt of
                  Nothing -> ([], Nothing) : acc
                  Just new_ctxt' -> retrieveTerm
                      (CompoundT $ CompoundTerm f  as )
                      (CompoundT $ CompoundTerm f' (map (`bindTermVars` new_ctxt') as')) : acc

              _ -> acc -- maybe it will never reach this line (if lookup works properly)

            -- NOTE: !!! When return extended ctxt dont extend it with vars in body of Program clause just extended vars from head
            Rule ch gl -> case asTerm ch of
              CompoundT CompoundTerm {functor = f', args = as'} ->
                let
                  new_ctxt = extendTerm (CompoundT $ CompoundTerm f' as')
                                        (CompoundT $ CompoundTerm f  as ) ctxt

                  retrieveCurrArgs ctxt' = retrieveTerm
                      (CompoundT $ CompoundTerm f  as )
                      (CompoundT $ CompoundTerm f' (map (`bindTermVars` ctxt') as'))

                  -- combine the result of body goals and args
                  combineBodyArgs (ctxt', res) = case res of
                      Nothing -> ([], Nothing)
                      Just res' -> case retrieveCurrArgs ctxt' of
                          (argsCtxt, Just True) -> (argsCtxt, Just res')
                          (_, Nothing) -> ([], Nothing)
                          false_res -> false_res
                in case new_ctxt of
                  Nothing -> ([], Nothing) : acc
                  Just new_ctxt' -> map combineBodyArgs (evalOrQuery prog new_ctxt' gl) ++ acc

              _ -> acc -- maybe it will never reach this line (if lookup works properly)

bindTermVars :: Term -> Context -> Term
bindTermVars t ctxt = case t of
  VarT var -> fromMaybe t (lookupVar var ctxt)
  CompoundT CompoundTerm {functor, args} -> CompoundT $ CompoundTerm {
    functor,
    args = map (`bindTermVars` ctxt) args
  }
  _ -> t -- AtomicTerms dont contain Vars

instance Callable Term where
  call prog ctxt t = case t of
    AtomicT at -> call prog ctxt at
    CompoundT ct -> call prog ctxt ct
    VarT var -> case lookupVar var ctxt of
      Just term -> call prog (filter (\(var', _) -> var' /= var) ctxt) term
      Nothing -> [(ctxt, Nothing)] -- NOTE: not sure if error should be [] or [..Nothing]

instance IsTerm Const where
  asTerm c = AtomicT $ ConstT c

instance IsTerm Var where
  asTerm v = VarT v

instance IsTerm AtomicTerm where
  asTerm at = AtomicT at

instance IsTerm CompoundTerm where
  asTerm ct = CompoundT ct

instance IsTerm ClauseHead where
  asTerm :: ClauseHead -> Term
  asTerm ch = case ch of
    CompoundHead ct -> asTerm ct
    ConstHead id -> asTerm $ Const id



-- Functor - unwrap its value apply function on it and wrap the result with the same Functor
-- usefull when want to map the value without changing the structure of main type
-- например имаме албум със снимки и искаме да сложим някакъв филтър на снимките вътре без да нарушаваме структурата на албума
--    fmap

-- Applicative - позволява последователно комбиниране на обекти
-- удобно когато искаме да комбинираме обекти от един тип знаейки как да оперираме с тяхните стойности и запазвайки типа
-- например имаме 2 албум със снимки и 
--    pure - lift/wrap a value to current Functor type
--    liftA2 - lift/wrap function to current Functor type like fmap but with 2 arguments
--    <*> - map wraped value with wraped function to Functor with other value type

-- Alternatives - Monoid on applicative functiors
--  група (има неутрален елемент и асоциативна операция)
--  empty
--  <|>  return-ва първият успешен вариант
type ParserRes a = Maybe (String, a)

newtype Parser a = Parser { runParser :: String -> ParserRes a } -- TODO: Use Either (Integer, Integer, String) where (Ln, Col, Message)

type ParserFunc a = a -> Parser a

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f Parser { runParser = g} = Parser { runParser }
    where
      runParser input =
        case g input of
          Nothing -> Nothing
          Just (input', x) -> Just (input', f x)

-- Applicative за да може да ползваме traverse което изисква елементите на листа да могат да се комбинират
-- (в нашия случай отделните parser-и да се run-ват последователно)
instance Applicative Parser where
  pure :: a -> Parser a
  pure x = Parser { runParser }
    where
      runParser input = Just (input, x)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) Parser { runParser = f } Parser { runParser = g } = Parser { runParser }
    where
      runParser input = case f input of
          Nothing -> Nothing
          Just (input', f') -> case g input' of
            Nothing -> Nothing
            Just (input'', x) -> Just (input'', f' x)

-- удобно когато не знаем какво точно парсваме, но имаме избор от няколко неща
-- (пр: термовете могат да са няколко вида и просто очакваме някакъв без значение)
instance Alternative Parser where
  empty :: Parser a
  empty = Parser { runParser }
    where
      runParser _ = Nothing

  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) Parser { runParser = f } Parser { runParser = g } = Parser { runParser }
    where
      runParser input = case f input of
        Nothing -> g input
        res -> res

-- <$  =  fmap . const  - v1 <$ Parser (\x -> Just (rest, v2))  =  Parser (\x -> Just (rest, v1))
-- *>  =  discard first
-- <*  =  discard second

-- one char (cant be forall types because runParser accepts )
satisfyP :: (Char -> Bool) -> Parser Char
satisfyP p = Parser { runParser = satisfy p }

-- zero or more
satisfyManyP :: (Char -> Bool) -> Parser String
satisfyManyP p = many $ satisfyP p

-- one or more
satisfySomeP :: (Char -> Bool) -> Parser String
satisfySomeP p = some $ satisfyP p

-- parse whole input
wholeInP :: Parser a -> Parser a
wholeInP p = Parser { runParser = f }
  where
    f input = case runParser p input of
      Just ([], res) -> Just ([], res)
      _ -> Nothing


unwrapMaybeP :: Parser (Maybe a) -> Parser a
unwrapMaybeP p = Parser { runParser = f }
  where
    f input = case runParser p input of
      Just (input', Just val) -> Just (input', val)
      _ -> Nothing

readP :: forall a. Read a => Parser String -> Parser a
readP p = unwrapMaybeP $ readMaybe <$> p

-- NOTE: Not sure if it handle tabs
ws :: Parser String
ws = satisfyManyP (isSpace .||. isLn)

splitSomeByP :: Parser a -> Parser b -> Parser [a]
splitSomeByP parser delim = (:) <$> parser <*> many (delim *> parser)

splitManyByP :: Parser a -> Parser b -> Parser [a]
splitManyByP parser delim = splitSomeByP parser delim <|> pure []

charP :: ParserFunc Char
charP x = satisfyP (== x)

stringP :: ParserFunc String
stringP = traverse charP

-- Comment parser
plComment :: Parser String
plComment = charP commentSign *> satisfyManyP (notp isLn) <* satisfyP isLn

comOrWs :: Parser String
comOrWs =  some (ws *> plComment) *> ws <|> ws

-- Atoms
plAtom :: Parser Atom
plAtom = satisfyP isLower .: satisfyManyP (isAlpha .||. isDigit .||. isUnderS)

-- NOTE: Maybe there are better ways for doing that
plId :: Parser Id
plId = Id <$> plAtom

plConst :: Parser Const
plConst = Const <$> plId

-- Var
plVar :: Parser Var
plVar = Var <$> satisfyP (isUpper .||. isUnderS) .: satisfyManyP (isAlpha .||. isDigit .||. isUnderS)

-- Terms
plConstTerm :: Parser AtomicTerm
plConstTerm = ConstT <$> plConst

plIntTerm :: Parser AtomicTerm
plIntTerm = IntT <$> readP (satisfySomeP isDigit)

plFloatTerm :: Parser AtomicTerm
plFloatTerm = FloatT <$> readP (satisfySomeP isDigit .++ stringP "." .++ satisfySomeP isDigit)

plAtomicTerm :: Parser AtomicTerm
plAtomicTerm = asum [plConstTerm, plFloatTerm, plIntTerm]

plCompoundTerm :: Parser CompoundTerm
plCompoundTerm = CompoundTerm <$> plId <*> (charP '(' *> comOrWs *> termsP <* comOrWs <* charP ')')
  where
    termsP :: Parser [Term]
    termsP = splitManyByP plTerm (comOrWs *> charP ',' <* comOrWs)

plTerm :: Parser Term
plTerm = asum [VarT <$> plVar, CompoundT <$> plCompoundTerm, AtomicT <$> plAtomicTerm]

-- Head
plCompoundHead :: Parser ClauseHead
plCompoundHead = CompoundHead <$> plCompoundTerm

plConstHead :: Parser ClauseHead
plConstHead = ConstHead <$> plId

plClauseHead :: Parser ClauseHead
plClauseHead = asum [plCompoundHead, plConstHead]

-- Goal
plClauseGoal :: Parser Goal
plClauseGoal = ClauseGoal <$> plClauseHead

plVarGoal :: Parser Goal
plVarGoal = VarGoal <$> plVar


plEnclosedGoal :: Parser Goal
plEnclosedGoal = EnclosedGoal <$> (charP '(' *> comOrWs *> plGoalList <* comOrWs <* charP ')');

plGoal :: Parser Goal
plGoal = asum [plClauseGoal, plVarGoal, plEnclosedGoal]

-- TODO: test with splitManyByP and splitSomeByP
plGoalList :: Parser GoalList
plGoalList = splitManyByP andGoalP (comOrWs *> charP ';' <* comOrWs)
  where
    andGoalP = splitSomeByP plGoal (comOrWs *> charP ',' <* comOrWs)

-- Query
plQuery :: Parser GoalList
plQuery = comOrWs *> plGoalList <* comOrWs <* charP clauseEnd

-- Clause
plFact :: Parser Clause
plFact = Fact <$> plClauseHead <* comOrWs <* charP clauseEnd

plRule :: Parser Clause
plRule = Rule <$> plClauseHead <* comOrWs <* stringP ":-" <*> plQuery

plClause :: Parser Clause
plClause = asum [plFact, plRule]

-- Program
plProgram :: Parser Program
-- plProgram = wholeInP $ ws *> splitManyByP plClause (ws <|> (ws *> plComment *> ws)) <* ws
plProgram = wholeInP $ comOrWs *> splitManyByP plClause comOrWs <* comOrWs

-- NOTE: backspace and arrows dont work in interpreter
--       compile the project and run the executable to delete and move through stdin
plReadFile :: String -> IO ()
plReadFile fileName = do
  contents <- readFile $ retrieveFileName fileName

  case runParser plProgram contents of
    Just (_rest, res) -> plRun res
    Nothing -> putStrLn "Some Error"

  return ()

-- readLn - chete konkreten tip (ako e string trqbva da e v "")
-- getLine - chete string ot stdin
plRun :: Program -> IO ()
plRun prog = go prog initialInput
  where
    go :: Program -> String -> IO ()
    go prog' last_input = do
      if last_input == initialInput
        then putStr "?- "
        else putStr $ "|  " ++ last_input

      curr_input <- getLine'
      let input = last_input ++ curr_input

      if input == "halt."
        then putStrLn "End!"
        else do
          let x = runParser (many plQuery) input
          case x of
            Just (rest, res) -> do
              eval prog' res
              if clauseEnd `elem` rest
                then do
                  putStrLn "Wrong Input 2"
                  go prog' initialInput
                else go prog' rest
            Nothing -> putStrLn "Wrong Input 1"

-- [ "true,false ; true" , "false, false" ]
eval :: Program -> [GoalList] -> IO ()
eval _ [] = return ()
eval prog queries = mapM_ go queries
  where
    go :: GoalList -> IO ()
    go query = case evalOrQuery prog [] query of
        [] -> putStrLn "Wrong Input 3"
        res -> mapM_ (snd >.> strRes >.> putStrLn) res
      where
        strRes :: Maybe Bool -> String
        strRes (Just True) = "true."
        strRes (Just False) = "false."
        strRes _ = "ERROR."

evalOrQuery :: Program -> Context -> GoalList -> [(Context, Maybe Bool)] -- example why returns Context "true, (X=5;X=6), write(X)."
evalOrQuery prog ctxt = foldr (\andQuery acc -> evalAndQuery prog ctxt andQuery ++ acc) []

evalAndQuery :: Program -> Context -> [Goal] -> [(Context, Maybe Bool)] -- NOTE: return [Bool] because of the OrQuery in EnclosedGoal
evalAndQuery prog ctxt andQuery = case andQuery of -- foldr f Nothing andQuery
  [] -> []
  [goal] -> case evalGoal prog ctxt goal of
      -- [] -> undefined
      res -> res
  (goal:gs) -> case evalGoal prog ctxt goal of
      -- [] -> undefined
      res -> foldr (\x acc -> case x of
          (ctxt', Just True) -> evalAndQuery prog ctxt' gs ++ acc
          x' -> x':acc
        ) [] res

evalGoal :: Program -> Context -> Goal -> [(Context, Maybe Bool)] -- NOTE: return [Bool] because of the OrQuery in EnclosedGoal
evalGoal prog ctxt g = case g of -- NOTE: use ctxt because on andQuery "X = 5, goal(X)."/ "likes(X, ivanka), write(X)." goal(X) depend on the new bind for X
  ClauseGoal ch -> callTerm prog ctxt ch
  VarGoal v -> callTerm prog ctxt v
  -- ExprGoal expr -> (:[]) <$> callTerm ctxt expr -- X is 5 / X = 5
  EnclosedGoal gl -> evalOrQuery prog ctxt gl -- run this query with new ctxt





























-- pureParser :: forall a. ParserFunc a
-- pureParser x = Parser { runParser }
--   where
--     runParser input = Just (input, x)

-- -- chainParsers :: forall a b. Parser a -> Parser b -> Parser b 

-- validateListParser :: forall a. [Parser a] -> Parser [a]
-- validateListParser = foldr go (pureParser [])
--   where
--     go :: Parser a -> Parser [a] -> Parser [a]
--     go (Parser x) (Parser acc) = Parser { runParser }
--       where
--         runParser :: String -> Maybe (String, [a])
--         runParser input = case x input of
--             Nothing -> Nothing
--             Just (x_input, x_res) -> case acc x_input of
--               Nothing  -> Nothing
--               Just (acc_input, acc_res) -> Just (acc_input, x_res : acc_res)

-- traverseListParser :: (a -> Parser b) -> [a] -> Parser [b]
-- traverseListParser f xs = validateListParser $ map f xs

-- charParser2 :: ParserFunc Char
-- charParser2 x = Parser { runParser }
--   where
--     runParser :: String -> Maybe (String, Char)
--     runParser [] = Nothing
--     runParser (y:ys)
--       | x == y = Just (ys, y)
--       | otherwise = Nothing

-- stringParser2 :: ParserFunc String
-- stringParser2 = traverseListParser charParser2

-- -- newtype Id = Id { getId :: String }
-- --   deriving (Show, Eq)

-- -- toId :: String -> Maybe Id
-- -- toId [] = Nothing
-- -- toId (x : xs)
-- --   | isLower x && True = Just $ Id "zdr"
-- --   | otherwise = Nothing


-- -- data My = Heh | Meh

-- -- instance IsString My where
-- --   fromString "Heh" = Heh
-- --   fromString _ = Meh


-- -- g :: String -> String
-- -- g x = "zdr " ++ x

-- -- gg :: My -> String
-- -- gg Heh = "zdr Heh"
-- -- gg Meh = "zdr Meh"


-- TODO: after eval, iterate zipped args again and return new context with extended values for first args
-- foldl
-- check equality in head and extends context if needed
-- g :: Context -> [(Term, Term)] -> [(Context, Maybe Bool)]
-- g ctxt' ((a,a') : zippedArgs) = case (a, bindTermVars a' ctxt') of
--   (AtomicT at, t) -> case t of
--     VarT var -> g (extend var a ctxt') zippedArgs -- bind 'var' in ctxt with 'at' and then exec
--     AtomicT at' -> if at == at' then g ctxt' zippedArgs else [(ctxt, Nothing)]
--     CompoundT _ -> [(ctxt, Nothing)]

--   (CompoundT ct, t) -> case t of
--     VarT var -> g (extend var a ctxt') zippedArgs
--     CompoundT ct' -> undefined
--     AtomicT at -> [(ctxt, Nothing)]

--   (VarT var, t) -> case t of
--     VarT var' -> undefined
--     AtomicT at -> g ctxt' zippedArgs
--     CompoundT ct -> undefined
-- -- TODO: eval body
-- g _ [] = undefined