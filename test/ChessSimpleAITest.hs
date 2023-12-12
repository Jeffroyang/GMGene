module ChessSimpleAITest where

import Chess
import ChessSimpleAI (simpleEval)
import Test.HUnit
import Test.QuickCheck

-- miniimal pieces for testing, contains only two kings that cancel each other out
minimalPieces :: [((Char, Int), Piece)]
minimalPieces = [(('d', 2), Piece King W), (('d', 7), Piece King B)]

-- >>> runTestTT test_minimalBoard
-- Counts {cases = 2, tried = 2, errors = 0, failures = 0}
test_minimalBoard :: Test
test_minimalBoard =
  TestList
    [ "Minimal board W" ~: simpleEval (constructBoard W [] minimalPieces) W ~?= 0,
      "Minimal board B" ~: simpleEval (constructBoard W [] minimalPieces) B ~?= 0
    ]

-- >>> runTestTT test_Pawns
-- Counts {cases = 3, tried = 3, errors = 0, failures = 0}
test_Pawns :: Test
test_Pawns =
  TestList
    [ "Single pawn white persp" ~: simpleEval (constructBoard W [] ((('e', 1), Piece Pawn W) : minimalPieces)) W ~?= 100,
      "Single pawn black persp" ~: simpleEval (constructBoard W [] ((('e', 1), Piece Pawn W) : minimalPieces)) B ~?= -100,
      "Two pawns with PST" ~: simpleEval (constructBoard W [] [(('e', 3), Piece Pawn W), (('e', 4), Piece Pawn W)]) W ~?= 220
    ]

-- >>> runTestTT test_Knights
-- Counts {cases = 3, tried = 3, errors = 0, failures = 0}
test_Knights :: Test
test_Knights =
  TestList
    [ "Single knight White Persp" ~: simpleEval (constructBoard W [] ((('c', 2), Piece Knight W) : minimalPieces)) W ~?= 320,
      "Single knight Black Persp" ~: simpleEval (constructBoard B [] ((('c', 2), Piece Knight W) : minimalPieces)) B ~?= -320,
      "Two knights with PST" ~: simpleEval (constructBoard W [] ([(('e', 2), Piece Knight W), (('e', 3), Piece Knight W)] ++ minimalPieces)) W ~?= 660
    ]

-- >>> runTestTT test_Bishops
-- Counts {cases = 3, tried = 3, errors = 0, failures = 0}
test_Bishops :: Test
test_Bishops =
  TestList
    [ "Single bishop White Persp" ~: simpleEval (constructBoard W [] ((('e', 2), Piece Bishop W) : minimalPieces)) W ~?= 330,
      "Single bishop Black Persp" ~: simpleEval (constructBoard B [] ((('e', 2), Piece Bishop W) : minimalPieces)) B ~?= -330,
      "Two bishops with PST" ~: simpleEval (constructBoard W [] ([(('e', 2), Piece Bishop W), (('e', 3), Piece Bishop W)] ++ minimalPieces)) W ~?= 670
    ]

-- >>> runTestTT test_Rooks
-- Counts {cases = 3, tried = 3, errors = 0, failures = 0}
test_Rooks :: Test
test_Rooks =
  TestList
    [ "Single rook White Persp" ~: simpleEval (constructBoard W [] ((('e', 2), Piece Rook W) : minimalPieces)) W ~?= 500,
      "Single rook Black Persp" ~: simpleEval (constructBoard B [] ((('e', 2), Piece Rook W) : minimalPieces)) B ~?= -500,
      "Two rooks with PST" ~: simpleEval (constructBoard W [] ([(('d', 1), Piece Rook W), (('e', 1), Piece Rook W)] ++ minimalPieces)) W ~?= 1010
    ]

-- >>> runTestTT test_Queens
-- Counts {cases = 3, tried = 3, errors = 0, failures = 0}
test_Queens :: Test
test_Queens =
  TestList
    [ "Single queen White Persp" ~: simpleEval (constructBoard W [] ((('e', 2), Piece Queen W) : minimalPieces)) W ~?= 900,
      "Single queen Black Persp" ~: simpleEval (constructBoard B [] ((('e', 2), Piece Queen W) : minimalPieces)) B ~?= -900,
      "Two queens with PST" ~: simpleEval (constructBoard W [] ([(('d', 1), Piece Queen W), (('e', 1), Piece Queen W)] ++ minimalPieces)) W ~?= 1790
    ]

-- >>> runTestTT test_Kings
-- Counts {cases = 2, tried = 2, errors = 0, failures = 0}
test_Kings :: Test
test_Kings =
  TestList
    [ "Single king White Persp" ~: simpleEval (constructBoard W [] ((('e', 2), Piece King W) : minimalPieces)) W ~?= 20000,
      "Single king Black Persp" ~: simpleEval (constructBoard B [] ((('e', 2), Piece King W) : minimalPieces)) B ~?= -20000
    ]

-- | Tests that the evaluation function is symmetric
prop_symmetricEval :: GameState -> Bool
prop_symmetricEval g = simpleEval g W == -simpleEval g B

-- | Tests the evaluation score for the different game states
prop_evalGameStates :: GameState -> Bool
prop_evalGameStates g = case checkResult g of
  InProgress -> value >= minBound && value <= maxBound
  BlackWin -> value == minBound * (if player g == B then 1 else -1)
  WhiteWin -> value >= maxBound * (if player g == W then 1 else -1)
  Draw -> value == 0
  where
    value = simpleEval g W

qc :: IO ()
qc = do
  quickCheck prop_symmetricEval
  quickCheck prop_evalGameStates
  return ()

test_all :: IO Counts
test_all = do
  runTestTT test_Pawns
  runTestTT test_Knights
  runTestTT test_Bishops
  runTestTT test_Rooks
  runTestTT test_Queens
  runTestTT test_Kings
  runTestTT test_minimalBoard
