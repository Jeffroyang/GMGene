{-# LANGUAGE TypeFamilies #-}

module ChessSimpleAI where

import Chess
import Chess qualified as C
import Data.Array
import Data.List (transpose)
import Data.Map ((!?))
import Data.Map qualified as M
import Data.Maybe
import GameAI qualified as G

-- The following piece values and piece-square tables are taken from Chess Programming Wiki
-- https://www.chessprogramming.org/Simplified_Evaluation_Function
-- All values are in terms of white's perspective, black's values are the negative of white's

-- | Returns the value of a piece
pieceValue :: Piece -> Int
pieceValue (Piece Pawn c) = if c == W then 100 else -100
pieceValue (Piece Knight c) = if c == W then 320 else -320
pieceValue (Piece Bishop c) = if c == W then 330 else -330
pieceValue (Piece Rook c) = if c == W then 500 else -500
pieceValue (Piece Queen c) = if c == W then 900 else -900
pieceValue (Piece King c) = if c == W then 20000 else -20000

pawnPST :: Array (Int, Int) Int
pawnPST =
  listArray
    ((1, 1), (8, 8))
    ( concat . transpose . reverse $
        [ [0, 0, 0, 0, 0, 0, 0, 0],
          [50, 50, 50, 50, 50, 50, 50, 50],
          [10, 10, 20, 30, 30, 20, 10, 10],
          [5, 5, 10, 25, 25, 10, 5, 5],
          [0, 0, 0, 20, 20, 0, 0, 0],
          [5, -5, -10, 0, 0, -10, -5, 5],
          [5, 10, 10, -20, -20, 10, 10, 5],
          [0, 0, 0, 0, 0, 0, 0, 0]
        ]
    )

knightPST :: Array (Int, Int) Int
knightPST =
  listArray
    ((1, 1), (8, 8))
    ( concat . transpose . reverse $
        [ [-50, -40, -30, -30, -30, -30, -40, -50],
          [-40, -20, 0, 0, 0, 0, -20, -40],
          [-30, 0, 10, 15, 15, 10, 0, -30],
          [-30, 5, 15, 20, 20, 15, 5, -30],
          [-30, 0, 15, 20, 20, 15, 0, -30],
          [-30, 5, 10, 15, 15, 10, 5, -30],
          [-40, -20, 0, 5, 5, 0, -20, -40],
          [-50, -40, -30, -30, -30, -30, -40, -50]
        ]
    )

bishopPST :: Array (Int, Int) Int
bishopPST =
  listArray
    ((1, 1), (8, 8))
    ( concat . transpose . reverse $
        [ [-20, -10, -10, -10, -10, -10, -10, -20],
          [-10, 0, 0, 0, 0, 0, 0, -10],
          [-10, 0, 5, 10, 10, 5, 0, -10],
          [-10, 5, 5, 10, 10, 5, 5, -10],
          [-10, 0, 10, 10, 10, 10, 0, -10],
          [-10, 10, 10, 10, 10, 10, 10, -10],
          [-10, 5, 0, 0, 0, 0, 5, -10],
          [-20, -10, -10, -10, -10, -10, -10, -20]
        ]
    )

rookPST :: Array (Int, Int) Int
rookPST =
  listArray
    ((1, 1), (8, 8))
    ( concat . transpose . reverse $
        [ [0, 0, 0, 0, 0, 0, 0, 0],
          [5, 10, 10, 10, 10, 10, 10, 5],
          [-5, 0, 0, 0, 0, 0, 0, -5],
          [-5, 0, 0, 0, 0, 0, 0, -5],
          [-5, 0, 0, 0, 0, 0, 0, -5],
          [-5, 0, 0, 0, 0, 0, 0, -5],
          [-5, 0, 0, 0, 0, 0, 0, -5],
          [0, 0, 0, 5, 5, 0, 0, 0]
        ]
    )

-- >>> rookPST ! (2, 7)
-- 10

queenPST :: Array (Int, Int) Int
queenPST =
  listArray
    ((1, 1), (8, 8))
    ( concat . transpose . reverse $
        [ [-20, -10, -10, -5, -5, -10, -10, -20],
          [-10, 0, 0, 0, 0, 0, 0, -10],
          [-10, 0, 5, 5, 5, 5, 0, -10],
          [-5, 0, 5, 5, 5, 5, 0, -5],
          [0, 0, 5, 5, 5, 5, 0, -5],
          [-10, 5, 5, 5, 5, 5, 0, -10],
          [-10, 0, 5, 0, 0, 0, 0, -10],
          [-20, -10, -10, -5, -5, -10, -10, -20]
        ]
    )

kingPSTMidgame :: Array (Int, Int) Int
kingPSTMidgame =
  listArray
    ((1, 1), (8, 8))
    ( concat . transpose . reverse $
        [ [-30, -40, -40, -50, -50, -40, -40, -30],
          [-30, -40, -40, -50, -50, -40, -40, -30],
          [-30, -40, -40, -50, -50, -40, -40, -30],
          [-30, -40, -40, -50, -50, -40, -40, -30],
          [-20, -30, -30, -40, -40, -30, -30, -20],
          [-10, -20, -20, -20, -20, -20, -20, -10],
          [20, 20, 0, 0, 0, 0, 20, 20],
          [20, 30, 10, 0, 0, 10, 30, 20]
        ]
    )

kingPSTEndgame :: Array (Int, Int) Int
kingPSTEndgame =
  listArray
    ((1, 1), (8, 8))
    ( concat . transpose . reverse $
        [ [-50, -40, -30, -20, -20, -30, -40, -50],
          [-30, -20, -10, 0, 0, -10, -20, -30],
          [-30, -10, 20, 30, 30, 20, -10, -30],
          [-30, -10, 30, 40, 40, 30, -10, -30],
          [-30, -10, 30, 40, 40, 30, -10, -30],
          [-30, -10, 20, 30, 30, 20, -10, -30],
          [-30, -30, 0, 0, 0, 0, -30, -30],
          [-50, -30, -30, -30, -30, -30, -30, -50]
        ]
    )

-- | Returns the total piece value of a board
evaluatePieceValues :: M.Map Piece Int -> Int
evaluatePieceValues counts =
  let getVal :: (Piece, Int) -> Int
      getVal (Piece p c, n) = n * pieceValue (Piece p c)
   in sum (map getVal (M.toList counts))

-- | Returns the count of each piece on the board
getPieceCounts :: Board -> M.Map Piece Int
getPieceCounts board = M.fromListWith (+) (map (\p -> (p, 1)) (catMaybes (elems board)))

-- | checks if we are in the endgame
-- "Both sides have no queens or every side which has a queen has additionally
-- no other pieces or one minorpiece maximum."
isEndgame :: M.Map Piece Int -> Bool
isEndgame counts =
  (whiteQueenCount == 0 || whiteMinorCount <= 1)
    && (blackQueenCount == 0 || blackMinorCount <= 1)
  where
    whiteQueenCount = fromMaybe 0 (counts !? Piece Queen W)
    blackQueenCount = fromMaybe 0 (counts !? Piece Queen B)
    blackMinorCount = fromMaybe 0 (counts !? Piece Knight B) + fromMaybe 0 (counts !? Piece Bishop B)
    whiteMinorCount = fromMaybe 0 (counts !? Piece Knight W) + fromMaybe 0 (counts !? Piece Bishop W)

-- | returns the mirrored position of a position
getMirrorPosition :: (Int, Int) -> (Int, Int)
getMirrorPosition (x, y) = (8 - x + 1, 8 - y + 1)

-- | Returns the total piece-square value of a board
evaluatePST :: Board -> Bool -> Int
evaluatePST board endgame =
  let getVal :: ((Int, Int), Maybe Piece) -> Int
      getVal (_, Nothing) = 0
      getVal (pos, Just (Piece Pawn c)) =
        if c == W
          then pawnPST ! pos
          else -pawnPST ! getMirrorPosition pos
      getVal (pos, Just (Piece Knight c)) =
        if c == W
          then knightPST ! pos
          else -knightPST ! getMirrorPosition pos
      getVal (pos, Just (Piece Bishop c)) =
        if c == W
          then bishopPST ! pos
          else -bishopPST ! getMirrorPosition pos
      getVal (pos, Just (Piece Rook c)) =
        if c == W
          then rookPST ! pos
          else -rookPST ! getMirrorPosition pos
      getVal (pos, Just (Piece Queen c)) =
        if c == W
          then queenPST ! pos
          else -queenPST ! getMirrorPosition pos
      getVal (pos, Just (Piece King c)) =
        let kingPST = if endgame then kingPSTEndgame else kingPSTMidgame
         in if c == W
              then kingPST ! pos
              else -kingPST ! getMirrorPosition pos
   in sum (map getVal (assocs board))

-- | Returns the total value of a board
simpleEval :: GameState -> Player -> Int
simpleEval gs perspective =
  let b = board gs
      counts = getPieceCounts b
      pieceValue = evaluatePieceValues counts
      endgame = isEndgame counts
      pstValue = evaluatePST b endgame
   in if perspective == W
        then pieceValue + pstValue
        else -(pieceValue + pstValue)

instance G.SearchableGame GameState where
  type Move GameState = C.Move
  type Player GameState = C.Player

  update :: GameState -> C.Move -> GameState
  update = C.move

  gameOver :: GameState -> Bool
  gameOver = C.gameOver

  evaluate :: GameState -> C.Player -> Int
  evaluate = simpleEval

  generateMoves :: GameState -> [C.Move]
  generateMoves = C.generateMoves