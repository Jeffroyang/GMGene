{-# LANGUAGE TypeFamilies #-}

module Chess
  ( Color (..),
    PieceType (..),
    Position,
    Piece (..),
    MoveC (..),
    Board,
    Player,
    GameState,
    Result (..),
    initBoard,
    move,
    validMove,
    gameOver,
    onBoard,
    squareSafe,
    showBoard,
    constructBoard,
    generateMoves,
    evaluate,
  )
where

import Control.Monad.State.Lazy
import Data.Array
import Data.Ix
import Search qualified as S

data Color = B | W deriving (Show, Eq)

data PieceType = King | Queen | Rook | Bishop | Knight | Pawn deriving (Show, Eq)

type Position = (Char, Int)

data Piece = Piece
  { pieceType :: PieceType,
    pieceColor :: Color
  }
  deriving (Show, Eq)

data MoveC = MoveC
  { piece :: Piece,
    from :: Position,
    to :: Position
  }
  deriving (Show, Eq)

type Board = Array Position (Maybe Piece)

type Player = Color

type History = [MoveC]

type GameState = State (Player, History) Board

data Result = BlackWin | WhiteWin | Draw | None deriving (Show, Eq)

initBoard :: GameState
initBoard = do
  let board =
        array
          (('a', 1), ('h', 8))
          ( [ (('a', 1), Just (Piece Rook W)),
              (('b', 1), Just (Piece Knight W)),
              (('c', 1), Just (Piece Bishop W)),
              (('d', 1), Just (Piece Queen W)),
              (('e', 1), Just (Piece King W)),
              (('f', 1), Just (Piece Bishop W)),
              (('g', 1), Just (Piece Knight W)),
              (('h', 1), Just (Piece Rook W)),
              (('a', 2), Just (Piece Pawn W)),
              (('b', 2), Just (Piece Pawn W)),
              (('c', 2), Just (Piece Pawn W)),
              (('d', 2), Just (Piece Pawn W)),
              (('e', 2), Just (Piece Pawn W)),
              (('f', 2), Just (Piece Pawn W)),
              (('g', 2), Just (Piece Pawn W)),
              (('h', 2), Just (Piece Pawn W)),
              (('a', 7), Just (Piece Pawn B)),
              (('b', 7), Just (Piece Pawn B)),
              (('c', 7), Just (Piece Pawn B)),
              (('d', 7), Just (Piece Pawn B)),
              (('e', 7), Just (Piece Pawn B)),
              (('f', 7), Just (Piece Pawn B)),
              (('g', 7), Just (Piece Pawn B)),
              (('h', 7), Just (Piece Pawn B)),
              (('a', 8), Just (Piece Rook B)),
              (('b', 8), Just (Piece Knight B)),
              (('c', 8), Just (Piece Bishop B)),
              (('d', 8), Just (Piece Queen B)),
              (('e', 8), Just (Piece King B)),
              (('f', 8), Just (Piece Bishop B)),
              (('g', 8), Just (Piece Knight B)),
              (('h', 8), Just (Piece Rook B))
            ]
              ++ [((x, y), Nothing) | x <- ['a' .. 'h'], y <- [3 .. 6]]
          )
  put (W, [])
  return board

-- | construct a board from a list of pieces and a current player
constructBoard :: [(Position, Piece)] -> Player -> GameState
constructBoard = undefined

-- | checks validity of move then moves piece and updates board
move :: GameState -> MoveC -> GameState
move board = undefined

-- | returns whether a move is valid
validMove :: GameState -> MoveC -> Bool
validMove = undefined

-- | checks if the current player's king is mated
gameOver :: GameState -> Bool
gameOver = undefined

-- | returns whether a position is on the board
onBoard :: Position -> Bool
onBoard (x, y) = inRange (('a', 1), ('h', 8)) (x, y)

-- | returns whether a position is safe(not under attack) for a given color
squareSafe :: Board -> Color -> Position -> Bool
squareSafe = undefined

-- | display the board
showBoard :: Board -> String
showBoard = undefined

-- | check result of game
checkResult :: GameState -> Result
checkResult = undefined

-- | evaluate chess position and return a score
evaluate :: GameState -> Int
evaluate = undefined

-- | generate all possible moves for a given position
generateMoves :: GameState -> [MoveC]
generateMoves = undefined

instance S.Game GameState where
  type Move GameState = MoveC
  initBoard :: GameState
  initBoard = initBoard

  move :: GameState -> S.Move GameState -> GameState
  move = move

  gameOver :: GameState -> Bool
  gameOver = gameOver

  evaluate :: GameState -> Int
  evaluate = evaluate

  generateMoves :: GameState -> [S.Move GameState]
  generateMoves = generateMoves