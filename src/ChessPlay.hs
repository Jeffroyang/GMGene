module ChessPlay where

import Chess
import Control.Monad.State.Lazy
import Data.Array
import GameAI

-- | Function to play the game
playGame :: IO ()
playGame = do
  putStrLn "Welcome to Chess!"
  print Chess.initBoard
