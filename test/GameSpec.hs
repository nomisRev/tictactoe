module GameSpec where

import Test.Hspec
import Control.Monad.State
import Game

winningSequence = [[Just X, Just X, Just X]]
emptySequence = [[]]
progressGame = InProgress [[Just X]]

spec :: Spec
spec = do
  describe "whoWon" $ do
    it "should return the winner O from the sequence" $
      let state = InProgress winningSequence
      in evalStateT (whoWon (0,1)) state `shouldBe` (Right $ Just X)

    it "should return Nothing when there is no winner" $
      let state = InProgress emptySequence
      in evalStateT (whoWon (0,0)) state `shouldBe` (Right $ Nothing)

  describe "getStone" $ do
    it "should return correct stone from board" $
      evalStateT (getStone (0,0)) progressGame `shouldBe` (Right $ Just X)
      
    it "should return correct error when out of board" $
      evalStateT (getStone (-1,-1)) progressGame `shouldBe` Left OutOfBoard

  describe "putStone" $ do
    it "should put stone in empty position" $
      let state = InProgress [[Nothing]]
      in runStateT (putStone (0,0) X) state `shouldBe` Right ((), InProgress [[Just X]])

    it "should error when position taken" $
      runStateT (putStone (0,0) X) progressGame `shouldBe` Left PositionTaken

    it "should return correct error when out of board" $
      evalStateT (putStone (-1,-1) O) progressGame `shouldBe` Left OutOfBoard

  describe "playTurn'" $ do
    it "place stone on board and stay in progress" $
      let state = InProgress [[Nothing, Nothing, Nothing]]
      in runStateT (playTurn' (0, 0) X) state `shouldBe` Right ((), InProgress [[Just X, Nothing, Nothing]])

    it "should return a winner with winning sequence" $
      let state = InProgress [[Nothing, Just X, Just X]]
      in runStateT (playTurn' (0, 0) X) state `shouldBe` Right ((), Won [[Just X, Just X, Just X]] X)
      
  