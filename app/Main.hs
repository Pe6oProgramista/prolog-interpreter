module Main where

import Lib
import Utils
import System.IO (hSetBuffering, stdin, BufferMode (NoBuffering), stdout)
import Data.List.NonEmpty (fromList)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  -- hSetBuffering stdin NoBuffering
  putStr "FileName: "
  -- fileName <- getLine'
  -- plReadFile fileName
  plReadFile "test3.pl"
