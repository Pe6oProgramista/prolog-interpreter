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
import Parser ( Parser(runParser), plQuery, plProgram )

import Debug.Trace (trace)
debug :: c -> String -> c
debug = flip trace

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
            -- LoadFile -> _
            _ -> if clauseEnd `elem` rest
              then do
                putStrLn "Input parsing error"
                go prog' initialInput
              else go prog' rest

        Nothing -> putStrLn "Input parsing error"
