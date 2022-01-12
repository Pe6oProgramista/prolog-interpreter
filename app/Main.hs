module Main where

import Lib
import Utils

main :: IO ()
main = do
  putStr "FileName: "
  -- fileName <- getLine'
  plReadFile "test3.pl"-- fileName

