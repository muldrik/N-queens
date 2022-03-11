module Main where

import SatGen
import Picosat
import Data.List (intersperse)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let gridSize = read $ head args :: Int
  solution <- solve (nQueens gridSize)
  case solution of
    (Solution s) -> putStrLn $ solutionToString gridSize s
    _ -> putStrLn "The problem has no solutions"

-- Generate a pretty grid. The second argument is a list of cells, where positive integer indicates a queen present and negative not present
solutionToString :: Int -> [Int] -> String
solutionToString boardSize solution = helper boardSize (filter (>0) solution) 0 where
  helper :: Int -> [Int] -> Int -> String
  helper 0 _ _ = "[]"
  helper n s rowNum = let
    row = fmap (\col -> if cellToVar boardSize (rowNum, col) `elem` s then 'Q' else '.') [0..n-1]
    outputRow = '[' : intersperse ' ' row ++ "]"
    in if rowNum == (n-1) then outputRow else outputRow ++ "\n" ++ helper n s (rowNum+1)

  