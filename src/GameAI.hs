{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module GameAI where

import Data.List (maximumBy)
import Data.Map qualified as Map
import Data.Ord (comparing)

-- | A searchable game is a game that can be searched using a tree
-- Moves should be ordered by best to worst to speed up the search
class (Ord (Move g)) => SearchableGame g where
  type Move g
  type Player g
  update :: g -> Move g -> g
  gameOver :: g -> Bool
  evaluate :: g -> Player g -> Int -- Passed in player is the maximizing player
  generateMoves :: g -> [Move g]

data GameTree g = TreeNode g (Map.Map (Move g) (GameTree g))

-- | Search the tree for the best move up to a certain depth
minimaxSearch :: forall g. (SearchableGame g) => g -> Player g -> Int -> Move g
minimaxSearch g p 0 = error "Cannot search to depth 0"
minimaxSearch g p d = fst $ maximumBy (comparing snd) (map (\m -> (m, mini (update g m) (d - 1))) (generateMoves g))
  where
    maxi :: (SearchableGame g) => g -> Int -> Int
    maxi g 0 = evaluate g p
    maxi g d = case generateMoves g of
      [] -> evaluate g p
      ms -> maximum (map (\m -> mini (update g m) (d - 1)) ms)

    mini :: (SearchableGame g) => g -> Int -> Int
    mini g 0 = evaluate g p
    mini g d = case generateMoves g of
      [] -> evaluate g p
      ms -> minimum (map (\m -> maxi (update g m) (d - 1)) ms)

-- | Search the tree for the best move up to a certain depth
negamaxSearch :: forall g. (SearchableGame g) => g -> Player g -> Int -> Move g
negamaxSearch g p 0 = error "Cannot search to depth 0"
negamaxSearch g p d = fst $ maximumBy (comparing snd) (map (\m -> (m, -negamax (update g m) (d - 1))) (generateMoves g))
  where
    negamax :: (SearchableGame g) => g -> Int -> Int
    negamax g 0 = evaluate g p
    negamax g d = case generateMoves g of
      [] -> if gameOver g then evaluate g p else -negamax g (d - 1)
      ms -> maximum (map (\m -> -negamax (update g m) (d - 1)) ms)

-- | updates the game tree with a move
updateGameTree :: (SearchableGame g) => GameTree g -> Move g -> Maybe (GameTree g)
updateGameTree (TreeNode g children) move = children Map.!? move

-- | Iteratively deepening depth-first search
iddfs :: (SearchableGame g) => Int -> g -> Move g
iddfs d g = undefined

-- | Search the tree for the best move up to a certain depth
alphaBetaSearch :: (SearchableGame g) => Int -> g -> Move g
alphaBetaSearch d g = undefined