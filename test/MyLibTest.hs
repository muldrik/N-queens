module Main (main) where
import SatGen
import Test.Tasty.HUnit
import Test.Tasty
import Picosat
import GHC.IO (unsafePerformIO)

-- Returns True if a queen setup is valid
verifySolution :: Int -> [Int] -> Bool
verifySolution gridSize solution' = let
  solution = filter (> 0) solution'
  correctSize = length solution == gridSize
  cells = fmap (varToCell gridSize) solution
  pairs = [(cell1, cell2) | cell1 <- cells, cell2 <- cells, cell1 /= cell2]
  noCollisions = all (\(c1, c2) -> not $ collision c1 c2) pairs
  in (gridSize == 0) || correctSize && noCollisions

-- Returns True if a valid queen setup is found
assertRightSolutionFound :: Int -> Bool
assertRightSolutionFound gridSize = unsafePerformIO $ do
  solution <- solve (nQueens gridSize)
  case solution of
    Solution s -> return $ verifySolution gridSize s
    _ -> return False

-- Returns True if no valid queen setup is found
assertNoSolutionFound :: Int -> Bool
assertNoSolutionFound gridSize = unsafePerformIO $ do
  solution <- solve (nQueens gridSize)
  case solution of
    Solution _ -> return False
    _ -> return True


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [
    testGroup "Grids with no solution" unsatAssertions,
    testGroup "Grids with a solution" satAssertions
  ]

unsatisfiable :: [Int]
unsatisfiable = [-1, 2, 3]

satisfiable :: [Int]
satisfiable = 0 : [4..40]


satAssertions :: [TestTree]
satAssertions = fmap (\n -> testCase (show n) $ assertBool ("Solution exists for grid size " ++ show n) (assertRightSolutionFound n)) satisfiable

unsatAssertions :: [TestTree]
unsatAssertions = fmap (\n -> testCase (show n) $ assertBool ("Solution doesn't exist for grid size " ++ show n) (assertNoSolutionFound n)) unsatisfiable


