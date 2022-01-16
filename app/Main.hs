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
  fileName <- getLine'
  program <- plReadFile fileName
  case program of
    Just prog -> plRun prog
    Nothing -> putStrLn "File parsing error2"
  -- plReadFile "test3.pl"
