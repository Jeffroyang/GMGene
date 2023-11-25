module SearchTest where

import Chess
import ChessTest
import Search
  ( alphaBetaPrunedSearch,
    dfs,
    iterativeDeepeningSearch,
  )
import Test.HUnit
import Test.QuickCheck

prop_IterativeDeepeningSearch :: Int -> GameState -> Bool
prop_IterativeDeepeningSearch d g = dfs d g == iterativeDeepeningSearch d g

prop_AlphaBetaPrunedSearch :: Int -> GameState -> Bool
prop_AlphaBetaPrunedSearch d g = dfs d g == alphaBetaPrunedSearch d g
