{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
module Parser where
import Control.Applicative (Alternative ((<|>), empty, many, some))
import Text.Read (readMaybe)
import Data.Char (isSpace, isLower, isAlpha, isDigit, isUpper)
import Data.Foldable (asum)

import Utils
import Terms

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

plEndInter :: Parser String
plEndInter = satisfyManyP (/= 'h') <* stringP "halt."