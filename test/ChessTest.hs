module ChessTest where

import Chess
import Data.Array (elems, (!))
import Data.Maybe (isJust)
import Test.HUnit
import Test.QuickCheck

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

  shrink = const []

-- in check state
testBoard1 :: GameState
testBoard1 =
  constructBoard
    W
    []
    [ (('e', 1), Piece King W),
      (('d', 2), Piece Pawn W),
      (('e', 7), Piece Queen B),
      (('e', 8), Piece King B)
    ]

-- pinned state
testBoard2 =
  constructBoard
    W
    []
    [ (('e', 1), Piece King W),
      (('e', 2), Piece Queen W),
      (('e', 7), Piece Queen B),
      (('e', 8), Piece King B)
    ]

-- quarter board for white movement
testBoard3 =
  constructBoard
    W
    []
    [ (('e', 1), Piece King W),
      (('d', 1), Piece Queen W),
      (('c', 1), Piece Bishop W),
      (('b', 1), Piece Knight W),
      (('a', 1), Piece Rook W),
      (('e', 8), Piece King B)
    ]

-- castle testing board(able to castle short, cant castle long due to danger squares)
testBoard4 =
  constructBoard
    W
    []
    [ (('e', 1), Piece King W),
      (('a', 1), Piece Rook W),
      (('h', 1), Piece Rook W),
      (('e', 8), Piece King B),
      (('d', 8), Piece Queen B),
      (('c', 8), Piece Bishop B),
      (('b', 8), Piece Knight B),
      (('a', 8), Piece Rook B)
    ]

-- en passantable
testBoard5 = constructBoard W [SMove (Piece Pawn B) (5, 7) (5, 5)] [(('e', 1), Piece King W), (('d', 5), Piece Pawn W), (('e', 5), Piece Pawn B), (('e', 8), Piece King B)]

testBoard6 = constructBoard W [SMove (Piece Pawn B) (5, 6) (5, 5), SMove (Piece Pawn B) (5, 7) (5, 6)] [(('e', 1), Piece King W), (('d', 5), Piece Pawn W), (('e', 5), Piece Pawn B), (('e', 8), Piece King B)]

-- promotion
testBoard7 = constructBoard W [] [(('e', 1), Piece King W), (('e', 7), Piece Pawn W), (('e', 8), Piece King B)]

testBoard8 = constructBoard W [] [(('e', 1), Piece King W), (('e', 7), Piece Pawn W), (('d', 8), Piece Pawn B), (('a', 8), Piece King B)]

-- | validMove correctly classifies moves
-- there are many edge cases here, this attempts to cover most
testValidMove :: Test
testValidMove =
  TestList
    [ -- general
      "Valid move" ~: validMove initBoard (SMove (Piece Pawn W) (5, 2) (5, 4)) ~?= True,
      "Wrong color move" ~: validMove initBoard (SMove (Piece Pawn B) (5, 7) (5, 5)) ~?= False,
      "Initial pos wrong" ~: validMove initBoard (SMove (Piece Pawn W) (5, 3) (5, 4)) ~?= False,
      -- piece movement
      "Bad pawn move" ~: validMove initBoard (SMove (Piece Pawn W) (5, 2) (5, 5)) ~?= False,
      "Good pawn move" ~: validMove initBoard (SMove (Piece Pawn W) (5, 2) (5, 4)) ~?= True,
      "Pawn bad capture" ~: validMove initBoard (SMove (Piece Pawn W) (4, 2) (5, 3)) ~?= False,
      "Bad knight move" ~: validMove initBoard (SMove (Piece Knight W) (2, 1) (2, 3)) ~?= False,
      "Good knight move" ~: validMove initBoard (SMove (Piece Knight W) (2, 1) (3, 3)) ~?= True,
      "Bad bishop move" ~: validMove testBoard3 (SMove (Piece Bishop W) (3, 1) (2, 3)) ~?= False,
      "Good bishop move" ~: validMove testBoard3 (SMove (Piece Bishop W) (3, 1) (8, 6)) ~?= True,
      "Bad rook move" ~: validMove testBoard3 (SMove (Piece Rook W) (1, 1) (2, 8)) ~?= False,
      "Good rook move" ~: validMove testBoard3 (SMove (Piece Rook W) (1, 1) (1, 8)) ~?= True,
      "Bad queen move" ~: validMove testBoard3 (SMove (Piece Queen W) (4, 1) (2, 8)) ~?= False,
      "Good queen move" ~: validMove testBoard3 (SMove (Piece Queen W) (4, 1) (4, 8)) ~?= True,
      "Good queen move2" ~: validMove testBoard3 (SMove (Piece Queen W) (4, 1) (1, 4)) ~?= True,
      "Bad king move" ~: validMove testBoard3 (SMove (Piece King W) (5, 1) (5, 3)) ~?= False,
      "Good king move" ~: validMove testBoard3 (SMove (Piece King W) (5, 1) (5, 2)) ~?= True,
      -- check cases
      "Into check move" ~: validMove testBoard1 (SMove (Piece King W) (5, 1) (5, 2)) ~?= False,
      "Move other piece while in check" ~: validMove testBoard1 (SMove (Piece Pawn W) (4, 2) (4, 3)) ~?= False,
      -- pin cases
      "Move pinned piece away" ~: validMove testBoard2 (SMove (Piece Queen W) (5, 2) (4, 3)) ~?= False,
      "Move pinned piece along pin" ~: validMove testBoard2 (SMove (Piece Queen W) (5, 2) (5, 4)) ~?= True,
      -- castling
      "Castle short" ~: validMove testBoard4 (ShortCastle W) ~?= True,
      "Castle long bad" ~: validMove testBoard4 (LongCastle W) ~?= False,
      -- en passant
      "En passantable" ~: validMove testBoard5 (EnPassant (Piece Pawn W) (4, 5) (5, 6)) ~?= True,
      "not en passantable" ~: validMove testBoard6 (EnPassant (Piece Pawn W) (4, 5) (5, 6)) ~?= False,
      -- promotion
      "Blocked promotion" ~: validMove testBoard7 (Promotion (Piece Queen W) (5, 7) (5, 8)) ~?= False,
      "Advance promote" ~: validMove testBoard8 (Promotion (Piece Rook W) (5, 7) (5, 8)) ~?= True,
      "Take promote" ~: validMove testBoard8 (Promotion (Piece Knight W) (5, 7) (4, 8)) ~?= True
    ]

-- >>> runTestTT testValidMove
-- Counts {cases = 28, tried = 28, errors = 0, failures = 0}

testBoard9 :: GameState
testBoard9 = constructBoard W [] [(('e', 1), Piece King W), (('e', 2), Piece Queen B), (('e', 3), Piece King B)]

testGameOver :: Test
testGameOver =
  TestList
    [ "Game over" ~: gameOver testBoard9 ~?= True,
      "Game not over" ~: gameOver initBoard ~?= False
    ]

testBoard10 :: GameState
testBoard10 = constructBoard B [] [(('e', 1), Piece King W), (('c', 7), Piece Queen W), (('e', 8), Piece King B), (('h', 8), Piece Rook W)]

testBoard11 :: GameState
testBoard11 = constructBoard B [] [(('e', 1), Piece King W), (('g', 6), Piece Queen W), (('h', 8), Piece King B)]

testResult :: Test
testResult =
  TestList
    [ "Black wins" ~: checkResult testBoard9 ~?= BlackWin,
      "No res" ~: checkResult initBoard ~?= InProgress,
      "White wins" ~: checkResult testBoard10 ~?= WhiteWin,
      "Draw" ~: checkResult testBoard11 ~?= Draw
    ]

containsPieceTypeMove :: [Move] -> PieceType -> Bool
containsPieceTypeMove [] _ = False
containsPieceTypeMove (SMove (Piece pt _) _ _ : ms) pt' = pt == pt' || containsPieceTypeMove ms pt'
containsPieceTypeMove (_ : ms) pt = containsPieceTypeMove ms pt

-- pawns can only move forward at most 2 spaces
prop_pawnsMoveForward :: GameState -> Property
prop_pawnsMoveForward g = containsPieceTypeMove m Pawn ==> all (pawnsForward2 g) m
  where
    m = generateMoves g
    pawnsForward2 :: GameState -> Move -> Bool
    pawnsForward2 g (SMove (Piece Pawn _) (x, y) (x', y')) =
      if player g == W
        then y' - y <= 2
        else y - y' <= 2
    pawnsForward2 _ _ = True

-- bishops can only move diagonally
prop_bishopsMoveDiagonal :: GameState -> Property
prop_bishopsMoveDiagonal g = containsPieceTypeMove m Bishop ==> all (bishopsDiagonal g) m
  where
    m = generateMoves g
    bishopsDiagonal :: GameState -> Move -> Bool
    bishopsDiagonal g (SMove (Piece Bishop _) (x, y) (x', y')) = abs (x - x') == abs (y - y')
    bishopsDiagonal _ _ = True

-- knights can only move in an L shape
prop_knightsMoveL :: GameState -> Property
prop_knightsMoveL g = containsPieceTypeMove m Knight ==> all (knightsL g) m
  where
    m = generateMoves g
    knightsL :: GameState -> Move -> Bool
    knightsL g (SMove (Piece Knight _) (x, y) (x', y')) =
      abs (x - x') == 1 && abs (y - y') == 2 || abs (x - x') == 2 && abs (y - y') == 1
    knightsL _ _ = True

-- rooks can only move horizontally or vertically
prop_rooksMoveHorizontalVertical :: GameState -> Property
prop_rooksMoveHorizontalVertical g = containsPieceTypeMove m Rook ==> all (rooksHorizontalVertical g) m
  where
    m = generateMoves g
    rooksHorizontalVertical :: GameState -> Move -> Bool
    rooksHorizontalVertical g (SMove (Piece Rook _) (x, y) (x', y')) = x == x' || y == y'
    rooksHorizontalVertical _ _ = True

-- queens can only move horizontally, vertically, or diagonally
prop_queensMoveHorizontalVerticalDiagonal :: GameState -> Property
prop_queensMoveHorizontalVerticalDiagonal g = containsPieceTypeMove m Queen ==> all (queensHorizontalVerticalDiagonal g) m
  where
    m = generateMoves g
    queensHorizontalVerticalDiagonal :: GameState -> Move -> Bool
    queensHorizontalVerticalDiagonal g (SMove (Piece Queen _) (x, y) (x', y')) = x == x' || y == y' || abs (x - x') == abs (y - y')
    queensHorizontalVerticalDiagonal _ _ = True

-- kings can only move one space in any direction
prop_kingsMoveOneSpace :: GameState -> Property
prop_kingsMoveOneSpace g = containsPieceTypeMove m King ==> all (kingsOneSpace g) m
  where
    m = generateMoves g
    kingsOneSpace :: GameState -> Move -> Bool
    kingsOneSpace g (SMove (Piece King _) (x, y) (x', y')) = abs (x - x') <= 1 && abs (y - y') <= 1
    kingsOneSpace _ _ = True

-- make sure piece count is always under 32
prop_pieceCount :: GameState -> Bool
prop_pieceCount g =
  let b = board g
   in length (filter isJust (elems b)) <= 32

-- making a valid move never leaves the king in check
prop_kingSafeAfterMove :: GameState -> Bool
prop_kingSafeAfterMove g = not (any (inCheck . gsForSamePlayer g) m)
  where
    b = board g
    m = generateMoves g
    p = player g
    gsForSamePlayer :: GameState -> Move -> GameState
    gsForSamePlayer gs m = (move gs m) {player = p}

-- make sure the king is never in check before castling
prop_kingNotInCheckPreCastle :: GameState -> Bool
prop_kingNotInCheckPreCastle g = all (castle g) m
  where
    m = generateMoves g
    castle :: GameState -> Move -> Bool
    castle g (ShortCastle _) = not (inCheck g)
    castle g (LongCastle _) = not (inCheck g)
    castle _ _ = True

-- make sure the king is never in check after castling
prop_kingNotInCheckPostCastle :: GameState -> Bool
prop_kingNotInCheckPostCastle g = all (castle g) m
  where
    m = generateMoves g
    castle :: GameState -> Move -> Bool
    castle g (ShortCastle p) = not (inCheck (move g (ShortCastle p)) {player = p})
    castle g (LongCastle p) = not (inCheck (move g (LongCastle (player g))) {player = p})
    castle _ _ = True

-- make sure the king has never moved before castling
prop_kingNeverMovedPreCastle :: GameState -> Bool
prop_kingNeverMovedPreCastle g = all (castle g) m
  where
    m = generateMoves g
    h = history g
    p = player g
    kingMoved :: [Move] -> Color -> Bool
    kingMoved [] _ = False
    kingMoved (ShortCastle p : ms) c = p == c || kingMoved ms c
    kingMoved (LongCastle p : ms) c = p == c || kingMoved ms c
    kingMoved ((SMove (Piece King p) _ _) : ms) c = p == c || kingMoved ms c
    kingMoved (_ : ms) c = kingMoved ms c

    castle :: GameState -> Move -> Bool
    castle g (ShortCastle _) = not (kingMoved h p)
    castle g (LongCastle _) = not (kingMoved h p)
    castle _ _ = True

-- make sure that en passant is only possible immediately after a pawn moves 2 spaces
prop_enPassantable :: GameState -> Bool
prop_enPassantable g = all (enPassant g) m
  where
    m = generateMoves g
    h = history g
    enPassant :: GameState -> Move -> Bool
    enPassant g (EnPassant (Piece Pawn p) (x, y) (x', y')) =
      case h of
        [] -> False
        (SMove (Piece Pawn p') (x'', y'') (x''', y''') : _) ->
          p /= p'
            && x' == x''' -- pawn must move into the square behind the other pawn
            && abs (y - y') == 1
            && abs (y'' - y''') == 2 -- other pawn must move two spaces vertically
        _ -> False
    enPassant _ _ = True

-- make sure that promotion is only possible when a pawn reaches the end of the board
prop_promotionRanks :: GameState -> Bool
prop_promotionRanks g = all (promotion g) m
  where
    m = generateMoves g
    promotion :: GameState -> Move -> Bool
    promotion g (Promotion (Piece Pawn p) (x, y) (x', y')) =
      case p of
        W -> y' == 8
        B -> y' == 1
    promotion _ _ = True

-- make sure that promoted pawns are replaced with the correct piece
prop_promotionPiece :: GameState -> Bool
prop_promotionPiece g = all (promotion g) m
  where
    m = generateMoves g
    promotion :: GameState -> Move -> Bool
    promotion g (Promotion (Piece Pawn _) (x, y) (x', y')) = False
    promotion g (Promotion (Piece King _) (x, y) (x', y')) = False
    promotion g m@(Promotion p (x, y) (x', y')) = board (move g m) ! (x', y') == Just p
    promotion _ _ = True

-- make sure that all moves are in bounds
prop_allMovesInBounds :: GameState -> Bool
prop_allMovesInBounds g = all (inBounds g) m
  where
    m = generateMoves g
    inBounds :: GameState -> Move -> Bool
    inBounds g (SMove _ (x, y) (x', y')) = x' >= 1 && x' <= 8 && y' >= 1 && y' <= 8
    inBounds _ _ = True

test_all :: IO Counts
test_all =
  runTestTT $
    TestList
      [ testValidMove,
        testGameOver
      ]

qc :: IO ()
qc = do
  quickCheck prop_pawnsMoveForward
  quickCheck prop_bishopsMoveDiagonal
  quickCheck prop_knightsMoveL
  quickCheck prop_rooksMoveHorizontalVertical
  quickCheck prop_queensMoveHorizontalVerticalDiagonal
  quickCheck prop_kingsMoveOneSpace
  quickCheck prop_pieceCount
  quickCheck prop_kingSafeAfterMove
  quickCheck prop_kingNotInCheckPreCastle
  quickCheck prop_kingNotInCheckPostCastle
  quickCheck prop_kingNeverMovedPreCastle
  quickCheck prop_enPassantable
  quickCheck prop_promotionRanks
  quickCheck prop_promotionPiece
  quickCheck prop_allMovesInBounds