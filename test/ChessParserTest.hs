module ChessParserTest where

import Chess
import ChessParser
import Parser qualified as P
import Test.HUnit
import Test.QuickCheck

-- >>> runTestTT testParsePiece
-- Counts {cases = 7, tried = 7, errors = 0, failures = 0}
testParsePiece :: Test
testParsePiece =
  TestList
    [ "Parse piece K" ~: P.parse (parsePiece W) "K" ~?= Right (Piece King W),
      "Parse piece Q" ~: P.parse (parsePiece W) "Q" ~?= Right (Piece Queen W),
      "Parse piece R" ~: P.parse (parsePiece B) "R" ~?= Right (Piece Rook B),
      "Parse piece B" ~: P.parse (parsePiece B) "B" ~?= Right (Piece Bishop B),
      "Parse piece N" ~: P.parse (parsePiece W) "N" ~?= Right (Piece Knight W),
      "Parse piece invalid" ~: P.parse (parsePiece W) "X" ~?= Left "No parses",
      "Parse piece P" ~: P.parse (parsePiece B) "P asd" ~?= Right (Piece Pawn B)
    ]

-- >>> runTestTT testParsePosition
-- Counts {cases = 9, tried = 9, errors = 0, failures = 1}
testParsePosition :: Test
testParsePosition =
  TestList
    [ "Parse position 1" ~: P.parse parsePosition "a1" ~?= Right (1, 1),
      "Parse position 2" ~: P.parse parsePosition "b2" ~?= Right (2, 2),
      "Parse position 3" ~: P.parse parsePosition "c3" ~?= Right (3, 3),
      "Parse position 4" ~: P.parse parsePosition "d4" ~?= Right (4, 4),
      "Parse position 5" ~: P.parse parsePosition "e5" ~?= Right (5, 5),
      "Parse position 6" ~: P.parse parsePosition "f6" ~?= Right (6, 6),
      "Parse position 7" ~: P.parse parsePosition "g7" ~?= Right (7, 7),
      "Parse position 8" ~: P.parse parsePosition "h8" ~?= Right (8, 8)
    ]

testParseStandardMove :: Test
testParseStandardMove =
  TestList
    [ "Parse standard move 1" ~: P.parse (parseStandardMove W) "K d4 f5" ~?= Right (SMove (Piece King W) (4, 4) (6, 5)),
      "Parse standard move 2" ~: P.parse (parseStandardMove W) "Q e5 e6" ~?= Right (SMove (Piece Queen W) (5, 5) (5, 6)),
      "Parse standard move 3" ~: P.parse (parseStandardMove B) "R f6 f5" ~?= Right (SMove (Piece Rook B) (6, 6) (6, 5)),
      "Parse standard move 4" ~: P.parse (parseStandardMove B) "B g7 g6" ~?= Right (SMove (Piece Bishop B) (7, 7) (7, 6)),
      "Parse standard move 5" ~: P.parse (parseStandardMove W) "N h3 h4" ~?= Right (SMove (Piece Knight W) (8, 3) (8, 4))
    ]

testParseLongCastle :: Test
testParseLongCastle =
  TestList
    [ "Parse long castle 1" ~: P.parse (parseLongCastle W) "OOO" ~?= Right (LongCastle W),
      "Parse long castle 2" ~: P.parse (parseLongCastle B) "OOO" ~?= Right (LongCastle B)
    ]

testParseShortCastle :: Test
testParseShortCastle =
  TestList
    [ "Parse short castle 1" ~: P.parse (parseShortCastle W) "OO" ~?= Right (ShortCastle W),
      "Parse short castle 2" ~: P.parse (parseShortCastle B) "OO" ~?= Right (ShortCastle B)
    ]

testParsePromotion :: Test
testParsePromotion =
  TestList
    [ "Parse promotion 1" ~: P.parse (parsePromotion W) "^Q e7 e8" ~?= Right (Promotion (Piece Queen W) (5, 7) (5, 8)),
      "Parse promotion 2" ~: P.parse (parsePromotion W) "^R e7 e8" ~?= Right (Promotion (Piece Rook W) (5, 7) (5, 8)),
      "Parse promotion 3" ~: P.parse (parsePromotion W) "^B e7 e8" ~?= Right (Promotion (Piece Bishop W) (5, 7) (5, 8)),
      "Parse promotion 4" ~: P.parse (parsePromotion W) "^N e7 e8" ~?= Right (Promotion (Piece Knight W) (5, 7) (5, 8))
    ]

test_all :: IO Counts
test_all =
  runTestTT $
    TestList
      [ "Parse piece" ~: testParsePiece,
        "Parse position" ~: testParsePosition,
        "Parse standard move" ~: testParseStandardMove,
        "Parse long castle" ~: testParseLongCastle,
        "Parse short castle" ~: testParseShortCastle,
        "Parse promotion" ~: testParsePromotion
      ]
