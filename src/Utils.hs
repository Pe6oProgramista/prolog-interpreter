{-# LANGUAGE RankNTypes #-}

module Utils where

import Data.List ( isSuffixOf )
import System.IO ( hFlush, stdout )

clauseEnd :: Char
clauseEnd = '.'

commentSign :: Char
commentSign = '%'

initialInput :: String
initialInput = ""

isUnderS :: Char -> Bool
isUnderS = (== '_')

isLn :: Char -> Bool
isLn = (== '\n')


retrieveFileName :: String -> String
retrieveFileName name
  | ".pl" `isSuffixOf` name = name
  | otherwise = name ++ ".pl"

getLine' :: IO String
getLine' = do
  hFlush stdout -- за да не буферира данните
  getLine

(>.>) :: (a -> b) -> (b -> c) -> a -> c
(>.>) = flip (.)

validateList :: [Maybe a] -> Maybe [a]
validateList = foldr go $ Just []
  where
    go (Just x) (Just r) = Just $ x : r
    go _ _ = Nothing

traverseListMaybe :: (a -> Maybe b) -> [a] -> Maybe [b]
traverseListMaybe f xs = validateList $ map f xs

maybeAndThen :: Maybe a -> (a -> Maybe b) -> Maybe b
maybeAndThen Nothing _ = Nothing
maybeAndThen (Just x) f = f x

satisfy :: (a -> Bool) -> [a] -> Maybe ([a], a)
satisfy _ [] = Nothing
satisfy p (x:xs)
  | p x = Just (xs, x)
  | otherwise = Nothing

(.||.) :: forall a. (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(.||.) p q x = p x || q x
infixl 6 .||.

(.&&.) :: forall a. (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(.&&.) p q x = p x && q x
infixl 7 .&&.

-- notp(redicate)
notp :: forall a. (a -> Bool) -> a -> Bool
notp = (not .)

-- (.>.) :: forall a. (a -> Bool) -> (a -> Bool) -> (a -> Bool)
-- (.>.) _ q = q
-- infixl 3 .>.

(.:) :: Applicative f => f a -> f [a] -> f [a]
(.:) p ps = (:) <$> p <*> ps
infixl 6 .:

(.++) :: Applicative f => f [a] -> f [a] -> f [a]
(.++) p ps = (++) <$> p <*> ps
infixl 6 .++

intersect :: forall a. (Eq a) => [a] -> [a] -> [a]
intersect l l' = filter (`elem` l') l

uniq :: forall a. Eq a => [a] -> [a]
uniq [] = []
uniq (x:xs) = x : uniq (filter (/=x) xs)

first :: forall a b c. (a -> b) -> (a, c) -> (b, c)
first f (x, y) = (f x, y)




