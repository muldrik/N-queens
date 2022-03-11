module SatGen where
import Control.Monad.Identity (guard)
--import Picosat

type Var = Int
type Clause = [Int]
type CNF = [Clause]

fls :: CNF
fls = [[1], [-1]]

tru :: CNF
tru = [[1]]

-- Check if two cells are in queen-sight from each other
collision :: (Int, Int) -> (Int, Int) -> Bool
collision (r1, c1) (r2, c2) = r1 == r2 || c1 == c2 || abs (r1 - r2) == abs (c1 - c2)

-- Convert a cell from coordinates to one integer
cellToVar :: Int -> (Int, Int) -> Var
cellToVar gridSize (row, col) = row * gridSize + col + 1 -- Plus one to allow negative numbers

-- Convert back from integer to coordinates
varToCell :: Int -> Int -> (Int, Int)
varToCell gridSize var = ((var - 1) `div` gridSize, (var - 1) `mod` gridSize) 


-- Variable <i,j> is true iff a queen occupies cell (i, j)
nQueens :: Int -> CNF
nQueens n | n < 0 = fls
          | n <= 1 = tru
          | n <= 4 = fls
          | otherwise = let
              rows = [0..n-1]
              cols = [0..n-1]
              cells = [(row, col) | row <- [0..n-1], col <- [0..n-1]]
              hitRestrictions = do -- For every two cells in queen sight from each other there can be no more than one queen
                cell1 <- cells
                cell2 <- cells
                let varC1 = cellToVar n cell1
                let varC2 = cellToVar n cell2
                guard $ varC1 < varC2 && collision cell1 cell2 -- Ensure no duplicates and only colliding cells
                return [-varC1, -varC2]
              rowPresence = do -- At least one queen must be present in every row
                 row <- rows
                 return [cellToVar n (row, col) | col <- cols]
              in hitRestrictions ++ rowPresence

