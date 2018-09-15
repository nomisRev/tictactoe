module Main where

import Game
import Data.List
import Control.Monad.State

main :: IO ()
main = playGame X emptyBoard

-- | Entry point to play a game
playGame :: Stone -> Board -> IO()
playGame stone board = do
    putStrLn $ formatBoard board
    putStrLn $ "Make a move:"
    line <- getLine
    let pos = read line
    let result = runStateT (playTurn' pos stone) (InProgress board)
    case result of
      (Left error) -> do
        putStrLn $ show error ++ " : try again " ++ show stone
        playGame stone board
      (Right (_, s)) -> case s of
        (InProgress board) -> playGame nextTurn board
        (Draw _) -> putStrLn $ "It was a draw!"
        (Won _ winner) -> putStrLn $ "Player " ++ show winner ++ " won!"
  where nextTurn = if stone == X then O else X

-- |Format board as a String as a text based UI.
formatBoard :: Board -> String
formatBoard all = lineSeperator ++ intercalate lineSeperator (map formatLine all) ++ lineSeperator
  where lineSeperator = "\n" ++ replicate (4*width) '-' ++ "-\n"
        formatLine = (\x -> "| " ++ intercalate " | " (rowAsText x) ++ " |")
        rowAsText x = map elementAsText x
        elementAsText a = case a of
          Nothing -> " "
          (Just a) -> show a
