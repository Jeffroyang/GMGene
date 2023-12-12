module GameAITest where

import Chess qualified as C
import ChessSimpleAI
import GameAI
import Test.HUnit
import Test.QuickCheck

newtype SmallInt = SmallInt Int deriving (Show)

instance Arbitrary SmallInt where
  arbitrary = do
    n <- choose (1, 2)
    return $ SmallInt n

-- minimax finds the best move for the player at depth 1
prop_minimaxDepthOne :: C.GameState -> Property
prop_minimaxDepthOne g =
  not (gameOver g) ==> snd (minimaxSearch g 1) == maximum (map (`evaluate` p) (update g <$> generateMoves g))
  where
    p = player g

prop_negamaxSearch :: SmallInt -> C.GameState -> Property
prop_negamaxSearch (SmallInt d) g =
  not (gameOver g) ==> snd (minimaxSearch g d) == snd (negamaxSearch g d)

prop_AlphaBetaPrunedSearch :: SmallInt -> C.GameState -> Property
prop_AlphaBetaPrunedSearch (SmallInt d) g =
  not (gameOver g) ==> snd (negamaxSearch g d) == snd (alphaBetaSearch g d)

{-
Benchmarking
------------
We can benchmark the performance of the search algorithms by simulating a game
to the end using the algorithms for both players. We can then compare the time
it takes for each algorithm to win the game. Black plays with depth 1, white
plays with depth d.
-}

-- simulate a game to the end running search algorithms for user specified depth
-- black plays with depth 1, white plays with depth d
simulateGameAsWhite :: C.GameState -> SearchAlgorithm C.GameState -> Int -> Int -> C.Result
simulateGameAsWhite g algo depth steps =
  if C.gameOver g || steps == 0
    then C.checkResult g
    else
      simulateGameAsWhite
        (update g (fst (algo g currDepth)))
        algo
        depth
        (steps - 1)
  where
    p = player g
    currDepth = if p == C.W then depth else 1

-- >>> simulateGameAsWhite C.initBoard minimaxSearch 2 52
-- WhiteWin
-- (1.36 secs, 7,489,057,184 bytes)

-- >>> simulateGameAsWhite C.initBoard alphaBetaSearch 2 52
-- WhiteWin
-- (0.41 secs, 2,181,033,392 bytes)

-- >>> simulateGameAsWhite C.initBoard minimaxSearch 3 52
-- WhiteWin
-- (34.09 secs, 199,807,500,032 bytes)

-- >>> simulateGameAsWhite C.initBoard alphaBetaSearch 3 52
-- WhiteWin
-- (8.31 secs, 48,411,535,008 bytes)

-- >>> simulateGameAsWhite C.initBoard alphaBetaSearch 4 52
-- WhiteWin
-- (71.13 secs, 387,414,819,592 bytes)

-- Alpha Beta Pruning Benchmarks after adding move ordering
-- >>> simulateGameAsWhite C.initBoard alphaBetaSearch 2 52
-- WhiteWin
-- (0.33 secs, 1,755,410,480 bytes)

-- >>> simulateGameAsWhite C.initBoard alphaBetaSearch 3 52
-- WhiteWin
-- (4.28 secs, 24,529,876,992 bytes)

-- >>> simulateGameAsWhite C.initBoard alphaBetaSearch 4 52
-- WhiteWin
-- (21.53 secs, 118,525,111,368 bytes)

qc :: IO ()
qc = do
  quickCheck prop_minimaxDepthOne
  quickCheck prop_negamaxSearch
  quickCheck prop_AlphaBetaPrunedSearch
  return ()