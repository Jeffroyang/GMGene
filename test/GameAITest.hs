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
prop_IterativeDeepeningSearch d g =
  let p = player g
   in minimaxSearch g p d == iddfs d g

prop_AlphaBetaPrunedSearch :: Int -> GameState -> Bool
prop_AlphaBetaPrunedSearch d g =
  let p = player g
   in minimaxSearch g p d == alphaBetaSearch d g
