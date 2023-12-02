{-# LANGUAGE TypeFamilies #-}

module ChessAI where

import Chess (GameState)
import Chess qualified as C
import GameAI qualified as G

-- Scoring functions based on https://www.dailychess.com/rival/programming/evaluation.php
-- Need to do more research to find out more specifics about the scoring functions

-- | Returns the pawn score for a game state
pawnScore :: GameState -> Int
pawnScore g = undefined

-- | Returns the knight score for a game state
knightScore :: GameState -> Int
knightScore g = undefined

-- | Returns the bishop score for a game state
bishopScore :: GameState -> Int
bishopScore g = undefined

-- | Returns the rook score for a game state
rookScore :: GameState -> Int
rookScore g = undefined

-- | Returns the queen score for a game state
queenScore :: GameState -> Int
queenScore g = undefined

-- | Returns the king score for a game state
kingScore :: GameState -> Int
kingScore g = undefined

-- | Returns the total score for a game state
evaluate :: GameState -> Int
evaluate g = undefined

instance G.SearchableGame GameState where
  type Move GameState = C.Move

  update :: GameState -> C.Move -> GameState
  update = C.move

  gameOver :: GameState -> Bool
  gameOver = C.gameOver

  evaluate :: GameState -> Int
  evaluate = evaluate

  generateMoves :: GameState -> [C.Move]
  generateMoves = C.generateMoves