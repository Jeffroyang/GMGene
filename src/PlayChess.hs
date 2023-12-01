module PlayChess where

import Chess
import Control.Monad.State.Lazy
import Data.Array
import Search

-- | Function to play the game
playGame :: IO ()
playGame = do
  putStrLn "Welcome to Chess!"
  print Chess.initBoard

-- | Parse a user move
parseUserMove :: String -> MoveC
parseUserMove = undefined
