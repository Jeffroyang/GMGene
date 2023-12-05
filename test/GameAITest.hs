module GameAITest where

import Chess
import ChessSimpleAI
import ChessTest ()
import GameAI
  ( alphaBetaSearch,
    iddfs,
    minimaxSearch,
  )
import Test.HUnit
import Test.QuickCheck

prop_IterativeDeepeningSearch :: Int -> GameState -> Bool
prop_IterativeDeepeningSearch d g = minimaxSearch d g == iddfs d g

prop_AlphaBetaPrunedSearch :: Int -> GameState -> Bool
prop_AlphaBetaPrunedSearch d g = minimaxSearch d g == alphaBetaSearch d g
