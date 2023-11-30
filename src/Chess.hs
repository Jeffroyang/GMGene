{-# LANGUAGE LambdaCase #-}
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

data MoveC
  = SMoveC
      { piece :: Piece,
        from :: Position,
        to :: Position
      } -- standard move or take
  | ShortCastle Color -- O-O
  | LongCastle Color -- O-O-O
  | Promotion {piece :: Piece, from :: Position, to :: Position} -- pawn promotion to piece at position (to) from position (from) (assumed pawn)
  | EnPassant {piece :: Piece, from :: Position, to :: Position} -- en passant capture
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

  if validMove board player m history
    then do
      put (if player == W then B else W, m : history)
      -- TODO account for castling
      return $ updateBoard board m
    else state

-- | returns whether a move is valid
-- TODO account for castling
validMove :: Board -> Color -> MoveC -> History -> Bool
validMove board c m h = m `elem` genMovesChess board c h

-- | checks if the current player's king is mated
gameOver :: GameState -> Bool
gameOver gs = undefined

-- | returns whether a position is on the board
onBoard :: Position -> Bool
onBoard (x, y) = inRange ((1, 1), (8, 8)) (x, y)

-- | returns whether in a position the king is under attack for a given color
inCheck :: Board -> Color -> History -> Bool
inCheck board c h =
  let kingPos = head $ map fst $ filter (\(_, p) -> p == Just (Piece King c)) (assocs board)
   in kingPos
        `elem` map
          ( \case
              SMoveC _ _ to -> to
              Promotion _ _ to -> to
              _ -> (0, 0) -- dummy value for other possible enemy moves since they dont attack
          )
          (genPsuedoMoves board (if c == W then B else W) h)

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
genPsuedoMovesPos :: Board -> Color -> History -> Position -> [MoveC]
genPsuedoMovesPos board c h pos@(f, r) = case board ! pos of
  Nothing -> []
  Just p@(Piece pt pc) ->
    if pc /= c
      then []
      else case pt of
        -- check normal 1 sq move, 2 sq move(if on starting sq), and takeable diagonals
        Pawn ->
          -- combine normal take moves, en passant moves, normal advance moves, expand all that result in promotions
          let pmoves =
                map (SMoveC p pos) (filter (\x -> onBoard x && takeable x) [(f + 1, r + forward), (f - 1, r + forward)])
                  ++ map (EnPassant p pos) (filter (\x -> onBoard x && enPassant x) [(f + 1, r + forward), (f - 1, r + forward)])
                  ++ map (SMoveC p pos) ([f1 | onBoard f1, isNothing (board ! f1)] ++ [f2 | onBoard f2, isNothing (board ! f2), isNothing (board ! f1), if c == W then r == 2 else r == 7])
           in concatMap expandPromotionMoves pmoves
          where
            f1 = (f, r + forward)
            f2 = (f, r + 2 * forward)
            -- if takeable moves are valid(enemy piece there)
            takeable :: Position -> Bool
            takeable pos@(tx, ty) =
              let qp = board ! pos
               in onBoard pos && (isJust qp && pieceColor (fromJust qp) /= c)
            -- en passantable
            enPassant :: Position -> Bool
            enPassant pos@(tx, ty) =
              let qp = board ! pos
               in onBoard pos
                    && ( not (null h) && case head h of -- en passant
                           SMoveC (Piece Pawn c2) from to -> c2 /= c && from == (tx, ty + forward) && to == (tx, ty - forward)
                           _ -> False
                       )
            -- expand moves that promote(only standard moves, en passant does not go to last rank)
            expandPromotionMoves :: MoveC -> [MoveC]
            expandPromotionMoves m@(SMoveC p@(Piece Pawn c) from to@(x, y)) =
              if (c == W && y == 8) || (c == B && y == 1)
                then map (\pt -> Promotion (Piece pt c) from to) [Queen, Rook, Bishop, Knight]
                else [m]
            expandPromotionMoves m = [m]
        -- knight just translate all possible and filter out invalid
        Knight ->
          map (SMoveC p pos) $
            filter (\x -> onBoard x && notBlocked x) $
              map (translate pos) [(-2, -1), (-1, -2), (1, -2), (2, -1), (-2, 1), (-1, 2), (1, 2), (2, 1)]
        -- for straight sliding pieces, generate all reachable positions along a translation dir
        Bishop -> reachable [(1, 1), (-1, 1), (1, -1), (-1, -1)]
        Rook -> reachable [(1, 0), (-1, 0), (0, 1), (0, -1)]
        Queen -> reachable [(1, 1), (-1, 1), (1, -1), (-1, -1), (1, 0), (-1, 0), (0, 1), (0, -1)]
        -- king same as knight
        King ->
          map
            (SMoveC p pos)
            ( filter (\x -> onBoard x && notBlocked x) $
                [translate pos (x, y) | x <- [-1 .. 1], y <- [-1 .. 1], (x, y) /= (0, 0)]
            )
            ++ [ShortCastle c | not (inCheck board c h) && kingNotMoved && rookValid (8, if c == W then 1 else 8) && shortCastleClear]
            ++ [LongCastle c | not (inCheck board c h) && kingNotMoved && rookValid (1, if c == W then 1 else 8) && longCastleClear]
          where
            -- checks whether king has moved
            kingNotMoved :: Bool
            kingNotMoved =
              not $
                any
                  ( \case
                      SMoveC (Piece King c) _ _ -> True
                      ShortCastle qc -> c == qc
                      LongCastle qc -> c == qc
                      _ -> False
                  )
                  h
            -- checks whether a rook in a pos has moved
            rookValid :: Position -> Bool
            rookValid pos =
              (board ! pos == Just (Piece Rook c))
                && not
                  ( any
                      ( \case
                          -- no friendly piece has moved from this position and no other piece has moved here
                          SMoveC p@(Piece pt qc) from to -> (c == qc && from == pos) || to == pos
                          -- no enemy pawn has moved here through special promotion move
                          Promotion p@(Piece pt qc) from to -> to == pos
                          _ -> False
                      )
                      h
                  )
            -- short castle path clear
            shortCastleClear :: Bool
            shortCastleClear =
              isNothing (board ! (6, if c == W then 1 else 8))
                && isNothing (board ! (7, if c == W then 1 else 8))
                && not (inCheck board c h) -- currently not in check
                && not (inCheck (board // [((6, if c == W then 1 else 8), Just $ Piece King c), ((5, if c == W then 1 else 8), Nothing)]) c h) -- not in check at f1/8
                -- long castle path clear
            longCastleClear :: Bool
            longCastleClear =
              isNothing (board ! (4, if c == W then 1 else 8))
                && isNothing (board ! (3, if c == W then 1 else 8))
                && isNothing (board ! (2, if c == W then 1 else 8))
                && not (inCheck board c h) -- currently not in check
                && not (inCheck (board // [((4, if c == W then 1 else 8), Just $ Piece King c), ((5, if c == W then 1 else 8), Nothing)]) c h) -- not in check at d1/8
    where
      forward = if c == W then 1 else -1
      -- uses translations to generate all reachable moves(sliding)
      reachable :: [Position] -> [MoveC]
      reachable = concatMap (\(x, y) -> map (SMoveC (Piece pt pc) pos) (accReachable c board pos (x, y) []))
      -- checks if a position is not blocked by a friendly piece
      notBlocked :: Position -> Bool
      notBlocked pos = isNothing (board ! pos) || pieceColor (fromJust (board ! pos)) /= c
      -- translate a position
      translate :: Position -> Position -> Position
      translate (x, y) (x', y') = (x + x', y + y')

-- generate all psuedo moves(no check checks)
-- only for checking if a move puts you in check
genPsuedoMoves :: Board -> Color -> History -> [MoveC]
genPsuedoMoves board c h = concatMap (genPsuedoMovesPos board c h) (indices board)

-- generate all legal moves for a color in a position
genMovesChess :: Board -> Color -> History -> [MoveC]
genMovesChess board c h = concatMap (filter (\m -> not $ inCheck (updateBoard board m) c h) . genPsuedoMovesPos board c h) (indices board)

-- update board with a move
updateBoard :: Board -> MoveC -> Board
updateBoard board m = case m of
  SMoveC p from to -> board // [(to, Just p), (from, Nothing)] -- normal moves
  ShortCastle c -> board // [((7, if c == W then 1 else 8), Just $ Piece King W), ((6, if c == W then 1 else 8), Just $ Piece Rook W), ((5, if c == W then 1 else 8), Nothing), ((8, if c == W then 1 else 8), Nothing)] -- short castle
  LongCastle c -> board // [((3, if c == W then 1 else 8), Just $ Piece King W), ((4, if c == W then 1 else 8), Just $ Piece Rook W), ((5, if c == W then 1 else 8), Nothing), ((1, if c == W then 1 else 8), Nothing)] -- long castle
  Promotion p from to -> board // [(to, Just p), (from, Nothing)] -- promotion moves(same as normal)
  EnPassant p@(Piece _ c) from to@(x, y) -> board // [(to, Just p), (from, Nothing), ((x, y - if c == W then 1 else -1), Nothing)] -- en passant moves

onepiece :: Board
onepiece = array ((1, 1), (8, 8)) [((x, y), Nothing) | x <- [1 .. 8], y <- [1 .. 8]] // [((8, 1), Just (Piece Rook W)), ((5, 1), Just (Piece King W)), ((6, 8), Just (Piece Rook B))]

-- >>> genMovesChess onepiece W []
-- [SMoveC {piece = Piece {pieceType = King, pieceColor = W}, from = (5,1), to = (4,1)},SMoveC {piece = Piece {pieceType = King, pieceColor = W}, from = (5,1), to = (4,2)},SMoveC {piece = Piece {pieceType = King, pieceColor = W}, from = (5,1), to = (5,2)},SMoveC {piece = Piece {pieceType = Rook, pieceColor = W}, from = (8,1), to = (6,1)},SMoveC {piece = Piece {pieceType = Rook, pieceColor = W}, from = (8,1), to = (7,1)},SMoveC {piece = Piece {pieceType = Rook, pieceColor = W}, from = (8,1), to = (8,8)},SMoveC {piece = Piece {pieceType = Rook, pieceColor = W}, from = (8,1), to = (8,7)},SMoveC {piece = Piece {pieceType = Rook, pieceColor = W}, from = (8,1), to = (8,6)},SMoveC {piece = Piece {pieceType = Rook, pieceColor = W}, from = (8,1), to = (8,5)},SMoveC {piece = Piece {pieceType = Rook, pieceColor = W}, from = (8,1), to = (8,4)},SMoveC {piece = Piece {pieceType = Rook, pieceColor = W}, from = (8,1), to = (8,3)},SMoveC {piece = Piece {pieceType = Rook, pieceColor = W}, from = (8,1), to = (8,2)}]

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
