{-# LANGUAGE TypeFamilies #-}

module GameAI where

class SearchableGame g where
  type Move g
  type Player g
  update :: g -> Move g -> g
  gameOver :: g -> Bool
  evaluate :: g -> Player g -> Int
  generateMoves :: g -> [Move g]

data Tree a = Node a [Tree a] deriving (Show)

-- | Generate a tree of all possible moves up to a certain depth
generateTree :: (SearchableGame g) => Int -> g -> Tree g
generateTree = undefined

-- | Search the tree for the best move up to a certain depth
minimaxSearch :: (SearchableGame g) => Int -> g -> Move g
minimaxSearch d g = undefined

-- | Iteratively deepening depth-first search
iddfs :: (SearchableGame g) => Int -> g -> Move g
iddfs d g = undefined

-- | Search the tree for the best move up to a certain depth
alphaBetaSearch :: (SearchableGame g) => Int -> g -> Move g
alphaBetaSearch d g = undefined