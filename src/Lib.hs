-- {-# LANGUAGE InstanceSigs #-} -- типова сигнатура на функции в instance
-- {-# LANGUAGE NamedFieldPuns #-} -- позволява ни да изпускаме изричното наименуване на полетата
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# Language ScopedTypeVariables #-} -- use rigid type variables in where/let
{-# LANGUAGE RankNTypes #-} -- forall
-- {-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Lib where

import System.IO ( IO, putStr, putStrLn, readFile )
import Control.Monad ( Monad(return) )

import Control.Applicative (Alternative(many))

import Utils ( clauseEnd, initialInput, retrieveFileName, getLine', uniq )
import Terms ( Program, eval, predefClauses, Info (EndInterpreter) )
import Parser ( Parser(runParser), plQuery, plProgram, plEndInter )

import Debug.Trace (trace)
debug :: c -> String -> c
debug = flip trace

-- NOTE: backspace and arrows dont work in interpreter
--       compile the project and run the executable to delete and move through stdin
plReadFile :: String -> IO ()
plReadFile fileName = do
  fileData <- readFile $ retrieveFileName fileName

  case runParser plProgram fileData of
    Just (_rest, res) -> plRun (predefClauses ++ res)
    Nothing -> putStrLn "File parsing error"

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
      
      let x = runParser (many plQuery) input
      case x of
        Just (rest, res) -> do
          evalRes <- eval prog' res
          case evalRes of
            EndInterpreter -> return ()
            _ -> if clauseEnd `elem` rest
              then do
                putStrLn "Input parsing error"
                go prog' initialInput
              else go prog' rest

        Nothing -> putStrLn "Input parsing error"







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


bla :: [(Int, Int)] -> [(Int, Int)] -> [(Int, [Int])]
bla c c' = foldr foldVarBindings [] $ uniq (c ++ c') `debug` ( "uniq: " ++ (show $ uniq (c++c')) )
  where
    foldVarBindings :: (Int, Int) -> [(Int, [Int])] -> [(Int, [Int])]
    foldVarBindings (v, t) [] = [(v, [t])]
    foldVarBindings (v, t) ((v', ts) : xs) = if v == v'
      then (v', t:ts) : xs
      else (v', ts) : foldVarBindings (v, t) xs


-- :r
-- let x = [(1,1), (1,2), (1,2), ( 2,1 ), ( 3,5 )]
-- let y = [( 6, 1), ( 2, 3), ( 3,5 ), ( 7, 9), ( 2,1 )]