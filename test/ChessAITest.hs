module ChessAITest where

import Chess
import ChessAI
import Test.HUnit

onePawn :: GameState
onePawn = constructBoard W [] [(('e', 2), Piece Pawn W)]

sameFilePawns :: GameState
sameFilePawns =
  constructBoard
    W
    []
    [ (('e', 2), Piece Pawn W),
      (('e', 3), Piece Pawn W)
    ]

testPawnScore :: Test
testPawnScore =
  TestList
    [ "Pawn score 1" ~: pawnScore onePawn ~?= 100,
      "Pawn score 2" ~: pawnScore sameFilePawns ~?= 193
    ]

oneBishop :: GameState
oneBishop =
  constructBoard
    W
    []
    [(('e', 2), Piece Bishop W)]

testBishopScore :: Test
testBishopScore =
  TestList
    [ "Bishop score 1" ~: bishopScore oneBishop ~?= 340
    ]

oneKnight :: GameState
oneKnight = constructBoard W [] [(('e', 2), Piece Knight W)]

testKnightScore :: Test
testKnightScore =
  TestList
    [ "Knight score 1" ~: knightScore oneKnight ~?= 325
    ]

oneRook :: GameState
oneRook = constructBoard W [] [(('e', 2), Piece Rook W)]

testRookScore :: Test
testRookScore =
  TestList
    [ "Rook score 1" ~: rookScore oneRook ~?= 500
    ]

oneQueen :: GameState
oneQueen = constructBoard W [] [(('e', 2), Piece Queen W)]

testQueenScore :: Test
testQueenScore =
  TestList
    [ "Queen score 1" ~: queenScore oneQueen ~?= 900
    ]
