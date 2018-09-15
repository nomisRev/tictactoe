{-# LANGUAGE TemplateHaskell #-}

module Game where

import Predef
import Data.Maybe
import Control.Monad.State
import Control.Lens hiding (element, set)

data Stone = X | O deriving (Show, Eq)
type Board = [[Maybe Stone]]
type Pos = (Int, Int)

data GameState = InProgress { _board :: Board }
               | Draw { _board :: Board  }
               | Won { _board :: Board, _winner :: Stone }
               deriving (Show, Eq)

makeLenses ''GameState

data BoardError = OutOfBoard | PositionTaken deriving (Show, Eq)
type BoardState = StateT GameState (Either BoardError)
type Test = StateT GameState (Either BoardError) ()

width :: Int
width = 3

height :: Int
height = width

winCount :: Int
winCount = width

emptyBoard :: Board
emptyBoard = replicate height . replicate width $ Nothing

debugBoard = [[Just O, Just X, Nothing],
              [Nothing, Nothing, Nothing],
              [Just O, Just X, Just X]]
              
newGame :: GameState
newGame = InProgress emptyBoard

-- |Play a turn. Instead of BoardState this should be GameState w errors, and shouldn't return value here.
playTurn :: Pos -> Stone -> BoardState (Maybe Stone)
playTurn pos s = (putStone pos s) >> whoWon pos

playTurn' :: Pos -> Stone -> BoardState ()
playTurn' pos s = (putStone pos s) >> whoWon pos >>= didWin
  -- where didWin mstone = modify (\state -> case mstone of
  --                                        (Just s') -> Won (view board state) s'
  --                                        Nothing -> state)

didWin :: Maybe Stone -> BoardState ()
didWin mstone = modify (\state -> case mstone of
  (Just s') -> Won (view board state) s'
  Nothing -> state)
  -- do
  -- _ <- putStone pos s
  -- mw <- fmap whoWon (possibleWins pos)
  -- state <- get
  -- case mw of
  --   (Just s') -> put $ Won (view board state) s'
  --   Nothing -> return ()

-- |Puts a player stone @ a given position.
putStone :: Pos -> Stone -> BoardState ()
putStone pos@(x,y) stone = (isInBounds pos) >> isTaken >> updateBoard
  where isTaken = (getStone pos) >>= isTaken'
        isTaken' Nothing = return ()
        isTaken' (Just s) = lift $ Left PositionTaken
        updateBoard = board %= (update y $ set x $ Just stone)

-- |Get a stone @ position in board. Treat outside of board as Nothing for now. Should be improved by using `MonadError BoardError` (StateT Board (Either BoardError))
getStone :: Pos -> BoardState (Maybe Stone)
getStone pos = (isInBounds pos) >> uses board (\b -> elemAt pos b >>= id)

--(FIXME: width/height will be moved to config, so you cannot rely on it here.) 
-- |Checks if position is within board
isInBounds :: Pos -> BoardState ()
isInBounds (x, y) = if xInBounds && yInBounds
                    then return ()
                    else lift $ Left OutOfBoard 
  where xInBounds = x `elem` [0..width - 1]
        yInBounds = y `elem` [0..height - 1]

-- |Tries to find a winning occurunces.
whoWon :: Pos -> BoardState (Maybe Stone)
whoWon pos = fmap whoWon' (possibleWins pos)
  where whoWon' = foldl (\acc xs -> mplus acc (winner' xs Nothing 0)) Nothing
        winner' [] _ _ = Nothing
        winner' (x:xs) x' i
          | x == x' && (i+1) == winCount = x'
          | x == x'                      = winner' xs x' (i+1)
          | a@(Just _) <- x              = winner' xs a 1
          | otherwise                    = winner' xs x 0

-- |Returns list of all possible winning sequences for a given position in the game.
possibleWins :: Pos -> BoardState [[Maybe Stone]]
possibleWins (x, y) = mapM (mapM getStone) [vertMoves, horMoves, rDiagMoves, lDiagMoves]
  where window     = winCount
        xRange     = [(max 0 (x - window)) .. (min (x + window) width-1)]
        yRange     = [(max 0 (y - window)) .. (min (y + window) height-1)]
        vertMoves  = zip (repeat x) yRange
        horMoves   = zip xRange (repeat y)
        rDiagMoves = zip xRange (reverse yRange)
        lDiagMoves = zip xRange yRange
        