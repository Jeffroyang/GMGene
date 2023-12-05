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

prop_negamaxSearch :: SmallInt -> C.GameState -> Property
prop_negamaxSearch (SmallInt d) g =
  not (gameOver g) ==> minimaxSearch g d == negamaxSearch g d

prop_IterativeDeepeningSearch :: SmallInt -> C.GameState -> Property
prop_IterativeDeepeningSearch (SmallInt d) g =
  not (gameOver g) ==> minimaxSearch g d == iddfs d g

prop_AlphaBetaPrunedSearch :: SmallInt -> C.GameState -> Property
prop_AlphaBetaPrunedSearch (SmallInt d) g =
  not (gameOver g) ==> minimaxSearch g d == alphaBetaSearch d g
