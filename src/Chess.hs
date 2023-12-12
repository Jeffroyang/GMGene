{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Chess
  ( Color (..),
    PieceType (..),
    Position,
    Piece (..),
    Move (..),
    Board,
    Player,
    GameState (..),
    Result (..),
    initBoard,
    move,
    validMove,
    checkResult,
    gameOver,
    onBoard,
    inCheck,
    showBoard,
    constructBoard,
    generateMoves,
    generateOrderedMoves,
  )
where

import Data.Array
import Data.Ix
import Data.List
import Data.Maybe
import Test.QuickCheck (Arbitrary (..), Gen, choose, elements, shrink)

data Color = B | W deriving (Show, Eq, Ord)

data PieceType
  = King
  | Queen
  | Rook
  | Bishop
  | Knight
  | Pawn
  deriving (Show, Eq, Ord)

-- | row col representation of a position for easier translation
type Position = (Int, Int)

data Piece = Piece {pieceType :: PieceType, pieceColor :: Color}
  deriving (Eq, Ord)

instance Show Piece where
  show :: Piece -> String
  show (Piece pt c) = case (pt, c) of
    (King, B) -> "k"
    (Queen, B) -> "q"
    (Rook, B) -> "r"
    (Bishop, B) -> "b"
    (Knight, B) -> "n"
    (Pawn, B) -> "p"
    (King, W) -> "K"
    (Queen, W) -> "Q"
    (Rook, W) -> "R"
    (Bishop, W) -> "B"
    (Knight, W) -> "N"
    (Pawn, W) -> "P"

type Board = Array Position (Maybe Piece)

data Move
  = SMove {piece :: Piece, from :: Position, to :: Position} -- standard move or take
  | ShortCastle Color -- O-O
  | LongCastle Color -- O-O-O
  | Promotion {piece :: Piece, from :: Position, to :: Position} -- pawn promotion to piece at position (to) from position (from) (assumed pawn)
  | EnPassant {piece :: Piece, from :: Position, to :: Position} -- en passant capture
  deriving (Eq, Show)

type History = [Move]

type Player = Color

data GameState = GameState
  { player :: Player,
    history :: History,
    captured :: [Piece],
    board :: Board
  }

instance Ord Move where
  compare :: Move -> Move -> Ordering
  compare Promotion {} Promotion {} = EQ
  compare Promotion {} _ = GT
  compare _ Promotion {} = LT
  compare EnPassant {} EnPassant {} = EQ
  compare EnPassant {} _ = GT
  compare _ EnPassant {} = LT
  compare ShortCastle {} ShortCastle {} = EQ
  compare ShortCastle {} _ = GT
  compare _ ShortCastle {} = LT
  compare LongCastle {} LongCastle {} = EQ
  compare LongCastle {} _ = GT
  compare _ LongCastle {} = LT
  compare SMove {} SMove {} = EQ

instance Show GameState where
  show :: GameState -> String
  show (GameState player history captured board) =
    "\n"
      ++ showBoard board
      ++ "White Captured: "
      ++ show (filter (\(Piece _ c) -> c == W) captured)
      ++ "\n"
      ++ "Black Captured: "
      ++ show (filter (\(Piece _ c) -> c == B) captured)
      ++ "\n"
      ++ "Current Player: "
      ++ show player

instance Arbitrary Color where
  arbitrary = elements [W, B]

instance Arbitrary PieceType where
  arbitrary = elements [King, Queen, Rook, Bishop, Knight, Pawn]

instance Arbitrary Piece where
  arbitrary = do
    c <- arbitrary
    t <- arbitrary
    return $ Piece t c

instance Arbitrary GameState where
  arbitrary :: Gen GameState
  -- simulate random number of turns (up to 100) to get a random board
  arbitrary = do
    n <- choose (0, 100)
    foldr ($) (pure initBoard) (replicate n randomTransition)
    where
      randomTransition :: Gen GameState -> Gen GameState
      randomTransition g = do
        g <- g
        let moves = generateMoves g
        if null moves
          then return g
          else do
            m <- elements moves
            return $ move g m

  shrink :: GameState -> [GameState]
  shrink gs = []

-- | display the board
showBoard :: Board -> String
showBoard board =
  let frameRow = "  +---+---+---+---+---+---+---+---+\n"
      showPiece :: Maybe Piece -> String
      showPiece Nothing = "   "
      showPiece (Just p) = " " ++ show p ++ " "
      showRow :: Int -> String
      showRow r =
        show r
          ++ " |"
          ++ concatMap (\c -> showPiece (board ! (c, r)) ++ "|") [1 .. 8]
          ++ "\n"
   in frameRow
        ++ intercalate frameRow (map showRow [8, 7 .. 1])
        ++ frameRow
        ++ "    a   b   c   d   e   f   g   h\n"

data Result = BlackWin | WhiteWin | Draw | InProgress deriving (Show, Eq)

initBoard :: GameState
initBoard = GameState W [] [] board
  where
    board =
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

type PositionC = (Char, Int)

-- | construct a board from a list of pieces and a current player
constructBoard :: Player -> History -> [(PositionC, Piece)] -> GameState
constructBoard player history positions = GameState player history [] board
  where
    board =
      array
        ((1, 1), (8, 8))
        [((x, y), Nothing) | x <- [1 .. 8], y <- [1 .. 8]]
        // map
          (\((x, y), p) -> ((fromEnum x - fromEnum 'a' + 1, y), Just p))
          positions

-- | checks validity of move then moves piece and updates board, else returns same board
move :: GameState -> Move -> GameState
move gs m =
  let b = board gs
      p = player gs
      h = history gs
      cap = captured gs
      opp = if p == W then B else W
      cap' = case m of
        SMove _ _ to ->
          case b ! to of
            Just p@(Piece _ c) -> if c == opp then p : cap else cap
            _ -> cap
        EnPassant _ _ to -> case b ! to of
          Just p@(Piece _ c) -> if c == opp then p : cap else cap
          _ -> cap
        _ -> cap
   in if validMove gs m
        then GameState opp (m : h) cap' (updateBoard b m)
        else gs

-- | returns whether a move is valid
validMove :: GameState -> Move -> Bool
validMove gs m = m `elem` generateMoves gs

-- | returns whether a position is on the board
onBoard :: Position -> Bool
onBoard (x, y) = inRange ((1, 1), (8, 8)) (x, y)

-- | returns whether in a position the king is under attack for a given player
inCheck :: GameState -> Bool
inCheck gs =
  let b = board gs
      p = player gs
      h = history gs
      cap = captured gs
      findKingPosition :: Board -> Color -> Position
      findKingPosition board color =
        case map fst $ filter (\(_, piece) -> piece == Just (Piece King p)) (assocs b) of
          [] -> (-1, -1) -- dummy value since king must be on board
          (x : _) -> x
      kingPos = findKingPosition b p
   in kingPos
        `elem` map
          ( \case
              -- for most pieces standard moves represent attack range, except pawns which attack diagonally
              -- need to filter out advance moves
              SMove p@(Piece pt _) from@(x, y) to@(x', y') ->
                if pt == Pawn then if x /= x' then to else (0, 0) else to
              -- same with promotions
              Promotion _ from@(x, y) to@(x', y') ->
                if x /= x' then to else (0, 0)
              _ -> (0, 0) -- dummy value for other possible enemy moves since they dont attack
          )
          (genPsuedoMoves $ GameState (if p == W then B else W) h cap b)

-- | check result of game
checkResult :: GameState -> Result
checkResult gs@(GameState p h cap b)
  | inCheck gs =
      if null (generateMoves $ GameState p h cap b)
        then if p == W then BlackWin else WhiteWin
        else InProgress
  | null (generateMoves $ GameState p h cap b) = Draw
  | otherwise = InProgress

-- | checks if game is in a terminal state
gameOver :: GameState -> Bool
gameOver gs = checkResult gs /= InProgress

-- generate all reachable positions from a position along a translation dir
accReachable :: Player -> Board -> Position -> (Int, Int) -> [Position] -> [Position]
accReachable c board src@(x, y) translate@(x', y') acc =
  let pos = (x + x', y + y')
   in if onBoard pos
        then case board ! pos of
          Nothing -> accReachable c board pos translate (pos : acc)
          Just (Piece _ c2) -> if c == c2 then acc else pos : acc
        else acc

-- helper for if a pawn can take a position
pawnTakeable :: Position -> Color -> Board -> Bool
pawnTakeable pos@(tx, ty) c b =
  onBoard pos
    && ( isJust (b ! pos)
           && pieceColor (fromJust (b ! pos)) /= c
       )

-- helper for checking if a position is en passantable
enPassant :: Position -> History -> Color -> Bool
enPassant pos@(tx, ty) h c =
  onBoard pos
    && ( not (null h) && case head h of -- en passant
           SMove (Piece Pawn c2) from to ->
             c2 /= c
               && from == (tx, ty + forward)
               && to == (tx, ty - forward)
           _ -> False
       )
  where
    forward = if c == W then 1 else -1

-- helper for expanding pawn promotion moves
expandPromotionMoves :: Move -> [Move]
expandPromotionMoves m@(SMove p@(Piece Pawn c) from to@(x, y)) =
  if (c == W && y == 8) || (c == B && y == 1)
    then
      map
        (\pt -> Promotion (Piece pt c) from to)
        [ Queen,
          Rook,
          Bishop,
          Knight
        ]
    else [m]
expandPromotionMoves m = [m]

-- | generate all psuedo legal moves for a given position
genPsuedoMovesPos :: GameState -> Position -> [Move]
genPsuedoMovesPos gs pos@(f, r) =
  let b = board gs
      c = player gs
      h = history gs
   in case b ! pos of
        Nothing -> []
        Just p@(Piece pt pc) ->
          if pc /= c
            then []
            else case pt of
              -- check normal 1 sq move, 2 sq move(if on starting sq), and takeable diagonals
              Pawn ->
                -- combine normal take moves, en passant moves, normal advance moves, expand all that result in promotions
                concatMap
                  expandPromotionMoves
                  ( map
                      (SMove p pos)
                      ( filter
                          (\x -> onBoard x && pawnTakeable x c b)
                          [ (f + 1, r + forward),
                            (f - 1, r + forward)
                          ]
                      )
                      ++ map
                        (EnPassant p pos)
                        ( filter
                            (\x -> onBoard x && enPassant x h c)
                            [ (f + 1, r + forward),
                              (f - 1, r + forward)
                            ]
                        )
                      ++ map
                        (SMove p pos)
                        ( [f1 | onBoard f1, isNothing (b ! f1)]
                            ++ [ f2
                                 | onBoard f2,
                                   isNothing (b ! f2),
                                   isNothing (b ! f1),
                                   if c == W then r == 2 else r == 7
                               ]
                        )
                  )
                where
                  f1 = (f, r + forward)
                  f2 = (f, r + 2 * forward)
              -- knight just translate all possible and filter out invalid
              Knight ->
                map (SMove p pos) $
                  filter (\x -> onBoard x && notBlocked x) $
                    map
                      (translate pos)
                      [ (-2, -1),
                        (-1, -2),
                        (1, -2),
                        (2, -1),
                        (-2, 1),
                        (-1, 2),
                        (1, 2),
                        (2, 1)
                      ]
              -- for straight sliding pieces, generate all reachable positions along a translation dir
              Bishop ->
                reachable
                  [ (1, 1),
                    (-1, 1),
                    (1, -1),
                    (-1, -1)
                  ]
              Rook ->
                reachable
                  [ (1, 0),
                    (-1, 0),
                    (0, 1),
                    (0, -1)
                  ]
              Queen ->
                reachable
                  [ (1, 1),
                    (-1, 1),
                    (1, -1),
                    (-1, -1),
                    (1, 0),
                    (-1, 0),
                    (0, 1),
                    (0, -1)
                  ]
              -- king same as knight
              King ->
                map
                  (SMove p pos)
                  ( filter (\x -> onBoard x && notBlocked x) $
                      [ translate pos (x, y)
                        | x <- [-1 .. 1],
                          y <- [-1 .. 1],
                          (x, y) /= (0, 0)
                      ]
                  )
          where
            forward = if c == W then 1 else -1
            -- uses translations to generate all reachable moves(sliding)
            reachable :: [Position] -> [Move]
            reachable =
              concatMap
                ( \(x, y) ->
                    map
                      (SMove (Piece pt pc) pos)
                      (accReachable c b pos (x, y) [])
                )
            -- checks if a position is not blocked by a friendly piece
            notBlocked :: Position -> Bool
            notBlocked pos =
              isNothing (b ! pos)
                || pieceColor (fromJust (b ! pos)) /= c
            -- translate a position
            translate :: Position -> Position -> Position
            translate (x, y) (x', y') = (x + x', y + y')

-- separate castle moves from above psuedo to avoid incheck recursion(these are not attack sqs)
genCastleMoves :: GameState -> [Move]
genCastleMoves gs =
  let b = board gs
      c = player gs
      h = history gs
      cap = captured gs
      kingNotMoved :: Bool
      kingNotMoved =
        not $
          any
            ( \case
                SMove (Piece King c) _ _ -> True
                ShortCastle qc -> c == qc
                LongCastle qc -> c == qc
                _ -> False
            )
            h
      -- checks whether a rook in a pos has moved
      rookValid :: Position -> Bool
      rookValid pos =
        (b ! pos == Just (Piece Rook c))
          && not
            ( any
                ( \case
                    -- no friendly piece has moved from this position and no other piece has moved here
                    SMove p@(Piece pt qc) from to ->
                      (c == qc && from == pos)
                        || to == pos
                    -- no enemy pawn has moved here through special promotion move
                    Promotion p@(Piece pt qc) from to -> to == pos
                    _ -> False
                )
                h
            )
      -- short castle path clear
      shortCastleClear :: Bool
      shortCastleClear =
        isNothing (b ! (6, if c == W then 1 else 8))
          && isNothing (b ! (7, if c == W then 1 else 8))
          && not (inCheck gs) -- currently not in check
          && not
            ( inCheck $
                GameState
                  c
                  h
                  cap
                  ( b
                      // [ ((6, if c == W then 1 else 8), Just $ Piece King c),
                           ((5, if c == W then 1 else 8), Nothing) -- not in check at f1/8
                           -- long castle path clear
                         ]
                  )
            )

      longCastleClear :: Bool
      longCastleClear =
        isNothing (b ! (4, if c == W then 1 else 8))
          && isNothing (b ! (3, if c == W then 1 else 8))
          && isNothing (b ! (2, if c == W then 1 else 8))
          && not (inCheck gs) -- currently not in check
          && not
            ( inCheck $
                GameState
                  c
                  h
                  cap
                  ( b
                      // [ ((4, if c == W then 1 else 8), Just $ Piece King c),
                           ((5, if c == W then 1 else 8), Nothing) -- not in check at d1/8
                         ]
                  )
            )
   in [ ShortCastle c
        | not (inCheck gs)
            && kingNotMoved
            && rookValid (8, if c == W then 1 else 8)
            && shortCastleClear
      ]
        ++ [ LongCastle c
             | not (inCheck gs)
                 && kingNotMoved
                 && rookValid (1, if c == W then 1 else 8)
                 && longCastleClear
           ]

-- generate all psuedo moves(no check checks)
-- only for checking if a move puts you in check
genPsuedoMoves :: GameState -> [Move]
genPsuedoMoves gs =
  let b = board gs
   in concatMap (genPsuedoMovesPos gs) (indices b)

-- generate all legal moves for a color in a position
generateMoves :: GameState -> [Move]
generateMoves gs@(GameState p h cap board) =
  concatMap
    ( filter
        (not . inCheck . GameState p h cap . updateBoard board)
        . genPsuedoMovesPos gs
    )
    (indices board)
    ++ genCastleMoves gs

-- generate all legal moves ordered by heuristic goodness
generateOrderedMoves :: GameState -> [Move]
generateOrderedMoves gs@(GameState p h cap board) =
  let moves = generateMoves gs
      isCaptureMove :: GameState -> Move -> Bool
      isCaptureMove gs m = case m of
        SMove _ _ to -> onBoard to && isJust (board ! to)
        EnPassant {} -> True
        _ -> False
      orderMoves :: Move -> Move -> Ordering
      orderMoves m1 m2
        | isCaptureMove gs m1 && not (isCaptureMove gs m2) = LT
        | not (isCaptureMove gs m1) && isCaptureMove gs m2 = GT
        | otherwise = compare m1 m2
   in sortBy orderMoves moves

-- update board with a move
updateBoard :: Board -> Move -> Board
updateBoard board m = case m of
  SMove p from to -> board // [(to, Just p), (from, Nothing)] -- normal moves
  ShortCastle c ->
    board
      // [ ((7, if c == W then 1 else 8), Just $ Piece King W),
           ((6, if c == W then 1 else 8), Just $ Piece Rook W),
           ((5, if c == W then 1 else 8), Nothing),
           ((8, if c == W then 1 else 8), Nothing) -- short castle
         ]
  LongCastle c ->
    board
      // [ ((3, if c == W then 1 else 8), Just $ Piece King W),
           ((4, if c == W then 1 else 8), Just $ Piece Rook W),
           ((5, if c == W then 1 else 8), Nothing),
           ((1, if c == W then 1 else 8), Nothing) -- long castle
         ]
  Promotion p from to ->
    board
      // [ (to, Just p),
           (from, Nothing) -- promotion moves(same as normal)
         ]
  EnPassant p@(Piece _ c) from to@(x, y) ->
    board
      // [ (to, Just p),
           (from, Nothing),
           ((x, y - if c == W then 1 else -1), Nothing) -- en passant moves
         ]
