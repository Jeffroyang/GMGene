module ChessScoringTest where

import Chess
import ChessScoring
import Test.HUnit

onePawn :: GameState
onePawn = constructBoard [(('e', 2), Piece Pawn W)] W

sameFilePawns :: GameState
sameFilePawns =
  constructBoard
    [ (('e', 2), Piece Pawn W),
      (('e', 3), Piece Pawn W)
    ]
    W

testPawnScore :: Test
testPawnScore =
  TestList
    [ "Pawn score 1" ~: pawnScore onePawn ~?= 100,
      "Pawn score 2" ~: pawnScore sameFilePawns ~?= 193
    ]

oneBishop :: GameState
oneBishop = constructBoard [(('e', 2), Piece Bishop W)] W

testBishopScore :: Test
testBishopScore =
  TestList
    [ "Bishop score 1" ~: bishopScore oneBishop ~?= 340
    ]

oneKnight :: GameState
oneKnight = constructBoard [(('e', 2), Piece Knight W)] W

testKnightScore :: Test
testKnightScore =
  TestList
    [ "Knight score 1" ~: knightScore oneKnight ~?= 325
    ]

oneRook :: GameState
oneRook = constructBoard [(('e', 2), Piece Rook W)] W

testRookScore :: Test
testRookScore =
  TestList
    [ "Rook score 1" ~: rookScore oneRook ~?= 500
    ]

oneQueen :: GameState
oneQueen = constructBoard [(('e', 2), Piece Queen W)] W

testQueenScore :: Test
testQueenScore =
  TestList
    [ "Queen score 1" ~: queenScore oneQueen ~?= 900
    ]