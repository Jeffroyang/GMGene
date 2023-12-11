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