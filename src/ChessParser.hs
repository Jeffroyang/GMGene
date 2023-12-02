module ChessParser where

import Chess
import Control.Applicative
  ( Alternative (many),
    Applicative (liftA2),
    liftA3,
  )
import Control.Monad ()
import Data.Char
import Parser (Parser)
import Parser qualified as P

-- | Moves are defined as follows
--  Standard move: [piece, from, to] (e.g., "N g1 f3")
--  Short castle: "OO"
--  Long castle: "OOO"
--  Promotion: [^Q, from, to] (e.g., "^Q a7 a8")
--  En passant: [ep, from, to] (e.g., "ep a5 b6")
wsP :: Parser a -> Parser a
wsP p = p <* many P.space

-- | Parser for strings
stringP :: String -> Parser ()
stringP = wsP . aux
  where
    aux str = P.string str *> pure ()

constP :: String -> a -> Parser a
constP s x = stringP s *> pure x

-- | Parser for chess moves
parseChessMove :: Color -> Parser Move
parseChessMove c =
  P.choice
    [ parseStandardMove c,
      parseLongCastle c,
      parseShortCastle c,
      parsePromotion c,
      parseEnPassant c
    ]

-- | Parser for pieces
parsePiece :: Color -> Parser Piece
parsePiece c =
  wsP $
    P.choice
      [ constP "K" (Piece King c),
        constP "Q" (Piece Queen c),
        constP "R" (Piece Rook c),
        constP "B" (Piece Bishop c),
        constP "N" (Piece Knight c),
        constP "P" (Piece Pawn c)
      ]

-- | Parser for positions
parsePosition :: Parser Position
parsePosition =
  wsP $
    liftA2
      (,)
      (letterToInt <$> P.satisfy isAlpha)
      (digitToInt <$> P.satisfy isDigit)
  where
    letterToInt :: Char -> Int
    letterToInt c = ord c - ord 'a' + 1

    digitToInt :: Char -> Int
    digitToInt c = ord c - ord '1' + 1

-- | Parser for standard moves
parseStandardMove :: Color -> Parser Move
parseStandardMove c =
  wsP $
    liftA3
      SMove
      (parsePiece c)
      parsePosition
      parsePosition

-- | Parser for long castles
parseLongCastle :: Color -> Parser Move
parseLongCastle c = constP "OOO" (LongCastle c)

-- | Parser for short castles
parseShortCastle :: Color -> Parser Move
parseShortCastle c = constP "OO" (ShortCastle c)

-- | Parser for promotions
parsePromotion :: Color -> Parser Move
parsePromotion c =
  stringP "^"
    *> wsP
      ( liftA3
          Promotion
          (parsePiece c)
          parsePosition
          parsePosition
      )

-- | Parser for en passant
parseEnPassant :: Color -> Parser Move
parseEnPassant c =
  stringP "ep"
    *> wsP
      ( liftA3
          EnPassant
          (parsePiece c)
          parsePosition
          parsePosition
      )
