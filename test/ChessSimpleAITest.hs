module ChessSimpleAITest where

import Chess
import ChessSimpleAI (simpleEval)
import Test.HUnit
import Test.QuickCheck

-- >>> runTestTT test_Pawns
-- Counts {cases = 3, tried = 3, errors = 0, failures = 1}
test_Pawns :: Test
test_Pawns =
  TestList
    [ "Single pawn White Persp" ~: simpleEval (constructBoard W [] [(('e', 1), Piece Pawn W)]) W ~?= 100,
      "Single pawn Black Persp" ~: simpleEval (constructBoard B [] [(('e', 1), Piece Pawn W)]) B ~?= -100,
      "Two pawns with PST" ~: simpleEval (constructBoard W [] [(('e', 3), Piece Pawn W), (('e', 4), Piece Pawn W)]) W ~?= 220
    ]

-- >>> runTestTT test_Knights
-- Counts {cases = 3, tried = 3, errors = 0, failures = 0}
test_Knights :: Test
test_Knights =
  TestList
    [ "Single knight White Persp" ~: simpleEval (constructBoard W [] [(('c', 2), Piece Knight W)]) W ~?= 320,
      "Single knight Black Persp" ~: simpleEval (constructBoard B [] [(('c', 2), Piece Knight W)]) B ~?= -320,
      "Two knights with PST" ~: simpleEval (constructBoard W [] [(('e', 2), Piece Knight W), (('e', 3), Piece Knight W)]) W ~?= 660
    ]

-- >>> runTestTT test_Bishops
-- Counts {cases = 3, tried = 3, errors = 0, failures = 0}
test_Bishops :: Test
test_Bishops =
  TestList
    [ "Single bishop White Persp" ~: simpleEval (constructBoard W [] [(('e', 2), Piece Bishop W)]) W ~?= 330,
      "Single bishop Black Persp" ~: simpleEval (constructBoard B [] [(('e', 2), Piece Bishop W)]) B ~?= -330,
      "Two bishops with PST" ~: simpleEval (constructBoard W [] [(('e', 2), Piece Bishop W), (('e', 3), Piece Bishop W)]) W ~?= 670
    ]

-- >>> runTestTT test_Rooks
-- Counts {cases = 3, tried = 3, errors = 0, failures = 0}
test_Rooks :: Test
test_Rooks =
  TestList
    [ "Single rook White Persp" ~: simpleEval (constructBoard W [] [(('e', 2), Piece Rook W)]) W ~?= 500,
      "Single rook Black Persp" ~: simpleEval (constructBoard B [] [(('e', 2), Piece Rook W)]) B ~?= -500,
      "Two rooks with PST" ~: simpleEval (constructBoard W [] [(('d', 1), Piece Rook W), (('e', 1), Piece Rook W)]) W ~?= 1010
    ]

-- >>> runTestTT test_Queens
-- Counts {cases = 3, tried = 3, errors = 0, failures = 1}
test_Queens :: Test
test_Queens =
  TestList
    [ "Single queen White Persp" ~: simpleEval (constructBoard W [] [(('e', 2), Piece Queen W)]) W ~?= 900,
      "Single queen Black Persp" ~: simpleEval (constructBoard B [] [(('e', 2), Piece Queen W)]) B ~?= -900,
      "Two queens with PST" ~: simpleEval (constructBoard W [] [(('d', 1), Piece Queen W), (('e', 1), Piece Queen W)]) W ~?= 1790
    ]

-- >>> runTestTT test_Kings
-- Counts {cases = 2, tried = 2, errors = 0, failures = 0}
test_Kings :: Test
test_Kings =
  TestList
    [ "Single king White Persp" ~: simpleEval (constructBoard W [] [(('e', 2), Piece King W)]) W ~?= 20000,
      "Single king Black Persp" ~: simpleEval (constructBoard B [] [(('e', 2), Piece King W)]) B ~?= -20000
    ]

-- | Tests that the evaluation function is symmetric
prop_symmetricEval :: GameState -> Bool
prop_symmetricEval g = simpleEval g W == -simpleEval g B

qc :: IO ()
qc = do
  quickCheck prop_symmetricEval
  return ()

test_all :: IO Counts
test_all = do
  runTestTT test_Pawns
