# tictactoe [![Build Status](https://travis-ci.org/nomisRev/tictactoe.svg?branch=master)](https://travis-ci.org/nomisRev/tictactoe)

## Work in progress
 To run and play run `stack build && stack exec tictactoe-exe` from terminal.
 
 Test can be run with `stack test`.
 
 ### TODOs (What is not there or not working yet)
   * Finishing the game: deciding draw or winner. (Winning logic is implemented but not wired properly yet)
   * Bug: seems to be something wrong with PositionTaken logic.
   * Make game configurable and change logic accordingly.
   * Clean up imports as described [here](https://wiki.haskell.org/Import_modules_properly)
   * Wire up in backend with [Servant](https://github.com/haskell-servant/servant)
   * Add persistance using [persistent](https://www.yesodweb.com/book/persistent)
