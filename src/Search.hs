{-# LANGUAGE TypeFamilies #-}

module Search where

class Game g where
  type Move g
  initBoard :: g
  move :: g -> Move g -> g
  gameOver :: g -> Bool
  evaluate :: g -> Int
  generateMoves :: g -> [Move g]

data Tree a = Node a [Tree a] deriving (Show)

-- | Generate a tree of all possible moves up to a certain depth
generateTree :: (Game g) => Int -> g -> Tree g
generateTree = undefined

-- | Search the tree for the best move up to a certain depth
dfs :: (Game g) => Int -> g -> Move g
dfs d g = undefined

iterativeDeepeningSearch :: (Game g) => Int -> g -> Move g
iterativeDeepeningSearch d g = undefined

-- | Search the tree for the best move up to a certain depth
alphaBetaPrunedSearch :: (Game g) => Int -> g -> Move g
alphaBetaPrunedSearch d g = undefined