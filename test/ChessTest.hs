module ChessTest where

import Chess
import Test.HUnit
import Test.QuickCheck

instance Arbitrary GameState where
  arbitrary = undefined

  shrink = undefined

testBoard1 :: GameState
testBoard1 = constructBoard [(('e', 1), Piece King W), (('d', 2), Piece Pawn W), (('e', 7), Piece Queen B), (('e', 8), Piece King B)] W

-- | validMove correctly classifies moves
testValidMove :: Test
testValidMove =
  TestList
    [ "Valid move" ~: validMove initBoard (MoveC (Piece Pawn W) ('e', 2) ('e', 4)) ~?= True,
      "Invalid pawn move" ~: validMove initBoard (MoveC (Piece Pawn W) ('e', 2) ('e', 5)) ~?= False,
      "Invalid color move" ~: validMove initBoard (MoveC (Piece Pawn B) ('e', 7) ('e', 5)) ~?= False,
      "Invalid into check move" ~: validMove testBoard1 (MoveC (Piece King W) ('e', 1) ('e', 2)) ~?= False,
      "Invalid check move" ~: validMove testBoard1 (MoveC (Piece Pawn W) ('d', 2) ('d', 3)) ~?= False
    ]

testBoard2 :: GameState
testBoard2 = constructBoard [(('e', 1), Piece King W), (('e', 2), Piece Queen B), (('e', 3), Piece King B)] W

testGameOver :: Test
testGameOver =
  TestList
    [ "Game over" ~: gameOver testBoard2 ~?= True,
      "Game not over" ~: gameOver initBoard ~?= False
    ]

-- | ¯\_(ツ)_/¯ TBD
testMove :: Test
testMove = undefined

-- | ¯\_(ツ)_/¯ TBD
testEvaluate :: Test
testEvaluate =
  TestList
    [ "Evaluate 0" ~: evaluate initBoard ~?= 0,
      "Evaluate 1" ~: evaluate testBoard1 ~?= 0,
      "Evaluate 2" ~: evaluate testBoard2 ~?= 0
    ]

testBoard3 :: GameState
testBoard3 = constructBoard [(('e', 2), Piece Pawn W)] W

-- 早上好中国，现在我有冰淇淋
testGenerateMoves :: Test
testGenerateMoves =
  TestList
    [ "Single pawn" ~: generateMoves testBoard3 ~?= [MoveC (Piece Pawn W) ('e', 2) ('e', 3), MoveC (Piece Pawn W) ('e', 2) ('e', 4)]
    ]