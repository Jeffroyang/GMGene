module ChessPlay where

import Chess
import Chess qualified as C
import ChessAI
import ChessParser
import Control.Monad.State.Lazy
import Data.Array
import GameAI
import Parser qualified as P

-- | Starts a new game of chess
playGame :: IO ()
playGame = do
  putStrLn "Welcome to Chess!"
  putStrLn "You can enter moves in the following formats:"
  putStrLn "  Standard move: \"N g1 f3\" (Knight from g1 to f3)"
  putStrLn "  Short castle: \"OO\""
  putStrLn "  Long castle: \"OOO\""
  putStrLn "  Promotion: \"^Q a7 a8\" (Pawn from a7 to a8, promoting to Queen)"
  putStrLn "  En passant: \"ep a5 b6\" (Pawn from a5 to b6, capturing en passant)"
  putStrLn "  The abbreviated pieces are K (King), Q (Queen), R (Rook), B (Bishop), N (Knight), and P (Pawn)"
  putStrLn "Would you like to play against the computer? (Y/N)"
  ai <- getLine
  case ai of
    "N" -> playGameHuman C.initBoard
    "Y" -> do
      player <- getPlayerColor
      depth <- getSearchDepth
      playGameAI player depth C.initBoard
    _ -> do
      putStrLn "Invalid input!"
      playGame

-- | Gets the player's color
getPlayerColor :: IO Player
getPlayerColor = do
  putStrLn "Would you like to play as White or Black? (W/B)"
  player <- getLine
  case player of
    "W" -> return W
    "B" -> return B
    _ -> do
      putStrLn "Invalid player!"
      getPlayerColor

-- | Gets a move from the user
getUserMove :: GameState -> IO C.Move
getUserMove g = do
  let p = player g
  putStr "Enter a move:"
  move <- getLine
  let parsedMove = P.parse (parseChessMove p) move
  case parsedMove of
    Left _ -> do
      putStrLn "Invalid move!"
      getUserMove g
    Right m ->
      if C.validMove g m
        then return m
        else do
          putStrLn "Invalid move!"
          getUserMove g

-- | Plays a game of chess against another human
playGameHuman :: GameState -> IO ()
playGameHuman g = do
  print g
  move <- getUserMove g
  let newG = C.move g move
  case C.checkResult newG of
    BlackWin -> print newG >> putStrLn "Game Over! Black wins!"
    WhiteWin -> print newG >> putStrLn "Game Over! White wins!"
    Draw -> print newG >> putStrLn "Game Over! Draw!"
    InProgress -> playGameHuman newG

-- | Get search depth from user
getSearchDepth :: IO Int
getSearchDepth = do
  putStr "Enter search depth (1-3):"
  depth <- getLine
  let parsedDepth = read depth :: Maybe Int
  case parsedDepth of
    Nothing -> do
      putStrLn "Invalid depth!"
      getSearchDepth
    Just d ->
      if d >= 1 && d <= 3
        then return d
        else do
          putStrLn "Invalid depth!"
          getSearchDepth

-- | Plays a game of chess against the AI
playGameAI :: Player -> Int -> GameState -> IO ()
playGameAI selected d g = do
  let currPlayer = player g
  print g
  if currPlayer == selected
    then do
      move <- getUserMove g
      let newG = C.move g move
      case C.checkResult newG of
        BlackWin -> print newG >> putStrLn "Game Over! Black wins!"
        WhiteWin -> print newG >> putStrLn "Game Over! White wins!"
        Draw -> print newG >> putStrLn "Game Over! Draw!"
        InProgress -> playGameAI selected d newG
    else do
      putStrLn "AI is thinking..."
      let move = alphaBetaSearch d g
      let newG = C.move g move
      case C.checkResult newG of
        BlackWin -> print newG >> putStrLn "Game Over! Black wins!"
        WhiteWin -> print newG >> putStrLn "Game Over! White wins!"
        Draw -> print newG >> putStrLn "Game Over! Draw!"
        InProgress -> playGameAI selected d newG