module ChessPlay where

import Chess
import Chess qualified as C
import ChessParser
import Control.Monad.State.Lazy
import Data.Array
import GameAI
import Parser qualified as P

-- | Function to play the game
playGame :: IO ()
playGame = do
  putStrLn "Welcome to Chess!"
  gameLoop Chess.initBoard

gameLoop :: GameState -> IO ()
gameLoop g = do
  print g
  putStrLn "Enter a move:"
  move <- getLine
  let p = player g
      parsedMove = P.parse (parseChessMove p) move
  case parsedMove of
    Left _ -> do
      putStrLn "Invalid move!"
      gameLoop g
    Right m -> do
      let newG = C.move g m
      if C.gameOver newG
        then putStrLn "Game Over!"
        else gameLoop newG