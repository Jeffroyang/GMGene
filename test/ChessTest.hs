module ChessTest where

import Chess
import Test.HUnit
import Test.QuickCheck

instance Arbitrary GameState where
  arbitrary = undefined

  shrink = undefined

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

-- -- | ¯\_(ツ)_/¯ TBD
-- testEvaluate :: Test
-- testEvaluate =
--   TestList
--     [ "Evaluate 0" ~: evaluate initBoard ~?= 0,
--       "Evaluate 1" ~: evaluate testBoard1 ~?= 0,
--       "Evaluate 2" ~: evaluate testBoard2 ~?= 0
--     ]

test_all :: IO Counts
test_all =
  runTestTT $
    TestList
      [ testValidMove,
        testGameOver
      ]