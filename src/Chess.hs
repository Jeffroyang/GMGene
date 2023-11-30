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
    inCheck,
    showBoard,
    constructBoard,
    generateMoves,
    evaluate,
  )
where

import Control.Monad.State.Lazy
import Data.Array
import Data.Ix
import Data.Maybe
import Search qualified as S

data Color = B | W deriving (Show, Eq)

data PieceType = King | Queen | Rook | Bishop | Knight | Pawn deriving (Show, Eq)

-- files are represented as ints 1-8 as well for easier translation
type Position = (Int, Int)

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
          ((1, 1), (8, 8))
          ( [ ((1, 1), Just (Piece Rook W)),
              ((2, 1), Just (Piece Knight W)),
              ((3, 1), Just (Piece Bishop W)),
              ((4, 1), Just (Piece Queen W)),
              ((5, 1), Just (Piece King W)),
              ((6, 1), Just (Piece Bishop W)),
              ((7, 1), Just (Piece Knight W)),
              ((8, 1), Just (Piece Rook W)),
              ((1, 8), Just (Piece Rook B)),
              ((2, 8), Just (Piece Knight B)),
              ((3, 8), Just (Piece Bishop B)),
              ((4, 8), Just (Piece Queen B)),
              ((5, 8), Just (Piece King B)),
              ((6, 8), Just (Piece Bishop B)),
              ((7, 8), Just (Piece Knight B)),
              ((8, 8), Just (Piece Rook B))
            ]
              ++ [((x, 2), Just (Piece Pawn W)) | x <- [1 .. 8]]
              ++ [((x, 7), Just (Piece Pawn B)) | x <- [1 .. 8]]
              ++ [((x, y), Nothing) | x <- [1 .. 8], y <- [3 .. 6]]
          )
  put (W, [])
  return board

-- | construct a board from a list of pieces and a current player
constructBoard :: [(Position, Piece)] -> Player -> GameState
constructBoard = undefined

-- | checks validity of move then moves piece and updates board, else returns same board
move :: GameState -> MoveC -> GameState
move state m = do
  (player, history) <- get
  board <- state

  if validMove board player m
    then do
      put (if player == W then B else W, m : history)
      -- TODO account for en passant and castling
      -- en passant removes pawn not at destination, castling moves rook as well
      return $ board // [(to m, Just (piece m)), (from m, Nothing)]
    else state

-- | returns whether a move is valid
-- TODO account for en passant, castling, promotions
validMove :: Board -> Color -> MoveC -> Bool
validMove board c m@(MoveC piece@(Piece pt pc) from@(x, y) to@(x', y')) = m `elem` genMovesChess board c

-- | checks if the current player's king is mated
gameOver :: GameState -> Bool
gameOver = undefined

-- | returns whether a position is on the board
onBoard :: Position -> Bool
onBoard (x, y) = inRange ((1, 1), (8, 8)) (x, y)

-- | returns whether in a position the king is under attack for a given color
inCheck :: Board -> Color -> Bool
inCheck board c =
  let kingPos = head $ map fst $ filter (\(_, p) -> p == Just (Piece King c)) (assocs board)
   in kingPos `elem` map (\m@(MoveC _ _ to) -> to) (genPsuedoMoves board (if c == W then B else W))

-- | display the board
showBoard :: Board -> String
showBoard = undefined

-- | check result of game
checkResult :: GameState -> Result
checkResult = undefined

-- | evaluate chess position and return a score
evaluate :: GameState -> Int
evaluate = undefined

-- | generate all possible moves for a given position (INTERFACE WRAPPER)
generateMoves :: GameState -> [MoveC]
generateMoves = undefined

-- generate all reachable positions from a position along a translation dir
accReachable :: Color -> Board -> Position -> (Int, Int) -> [Position] -> [Position]
accReachable c board src@(x, y) translate@(x', y') acc =
  let pos = (x + x', y + y')
   in if onBoard pos
        then case board ! pos of
          Nothing -> accReachable c board pos translate (pos : acc)
          Just (Piece _ c2) -> if c == c2 then acc else pos : acc
        else acc

-- | generate all psuedo legal moves for a given position
genPsuedoMovesPos :: Board -> Color -> Position -> [MoveC]
genPsuedoMovesPos board c pos@(f, r) = case board ! pos of
  Nothing -> []
  Just p@(Piece pt pc) ->
    if pc /= c
      then []
      else case pt of
        -- check normal 1 sq move, 2 sq move(if on starting sq), and takeable diagonals
        -- TODO: en passant
        Pawn ->
          map (MoveC p pos) (filter (\x -> onBoard x && takeable x) [(f + 1, r + forward), (f - 1, r + forward)])
            ++ map (MoveC p pos) ([f1 | onBoard f1, isNothing (board ! f1)] ++ [f2 | onBoard f2, isNothing (board ! f2), isNothing (board ! f1), if c == W then r == 2 else r == 7])
          where
            f1 = (f, r + forward)
            f2 = (f, r + 2 * forward)
            takeable :: Position -> Bool
            takeable pos =
              let qp = board ! pos
               in onBoard pos && isJust qp && pieceColor (fromJust qp) /= c
        -- knight just translate all possible and filter out invalid
        Knight ->
          map (MoveC p pos) $
            filter (\x -> onBoard x && notBlocked x) $
              map (translate pos) [(-2, -1), (-1, -2), (1, -2), (2, -1), (-2, 1), (-1, 2), (1, 2), (2, 1)]
        -- for straight sliding pieces, generate all reachable positions along a translation dir
        Bishop -> reachable [(1, 1), (-1, 1), (1, -1), (-1, -1)]
        Rook -> reachable [(1, 0), (-1, 0), (0, 1), (0, -1)]
        Queen -> reachable [(1, 1), (-1, 1), (1, -1), (-1, -1), (1, 0), (-1, 0), (0, 1), (0, -1)]
        -- king same as knight
        King ->
          map (MoveC p pos) $
            filter (\x -> onBoard x && notBlocked x) $
              [translate pos (x, y) | x <- [-1 .. 1], y <- [-1 .. 1], (x, y) /= (0, 0)]
    where
      forward = if c == W then 1 else -1
      -- uses translations to generate all reachable moves(sliding)
      reachable :: [Position] -> [MoveC]
      reachable = concatMap (\(x, y) -> map (MoveC (Piece pt pc) pos) (accReachable c board pos (x, y) []))
      -- checks if a position is not blocked by a friendly piece
      notBlocked :: Position -> Bool
      notBlocked pos = isNothing (board ! pos) || pieceColor (fromJust (board ! pos)) /= c
      -- translate a position
      translate :: Position -> Position -> Position
      translate (x, y) (x', y') = (x + x', y + y')

-- generate all psuedo moves(no check checks)
-- only for checking if a move puts you in check
genPsuedoMoves :: Board -> Color -> [MoveC]
genPsuedoMoves board c = concatMap (genPsuedoMovesPos board c) (indices board)

-- generate all legal moves for a color in a position
genMovesChess :: Board -> Color -> [MoveC]
genMovesChess board c = concatMap (filter (\m@(MoveC p from to) -> not $ inCheck (board // [(from, Nothing), (to, Just p)]) c) . genPsuedoMovesPos board c) (indices board)

onepiece :: Board
onepiece = array ((1, 1), (8, 8)) [((x, y), Nothing) | x <- [1 .. 8], y <- [1 .. 8]] // [((5, 2), Just (Piece Pawn W))]

-- >>> genMovesChess onepiece W
-- []

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
