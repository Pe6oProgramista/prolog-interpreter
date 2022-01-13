module Main where

import Lib
import Utils

main :: IO ()
main = do
  putStr "FileName: "
  fileName <- getLine'
  plReadFile fileName
  -- plReadFile "test3.pl"

