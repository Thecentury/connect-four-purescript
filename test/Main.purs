module Test.Main where

import Prelude

import ConnectFour (Board, Player(..), Winner(..), boardDiagonals, buildGameTree, configOfRowsColumns, configWithDepth, configWithWin, nextBoardFromMove, nextMove, nextMoves, tryAddToColumn, winner)
import Control.Monad.Reader (runReader, runReaderT)
import Data.Array as Array
import Data.List as List
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import ConnectFour.Prelude (treeChildren, treeValue)
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

boardOfArray :: Array (Array Player) -> Board
boardOfArray = List.fromFoldable <<< map List.fromFoldable

boardToArray :: Board -> Array (Array Player)
boardToArray = Array.fromFoldable <<< map Array.fromFoldable

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "Diagonals" do
    it "calculates diagonals of a 1x1 board" do
      let board = boardOfArray [[X]]
      let cfg = configOfRowsColumns 1 1
      let diagonals = runReader (boardDiagonals board) cfg
      boardToArray diagonals `shouldEqual` [[X], [X]]

    it "calculates diagonals of a 2x1 board" $ do
      let board = boardOfArray [[X], [O]]
      let cfg = configOfRowsColumns 2 1
      let diagonals = runReader (boardDiagonals board) cfg
      boardToArray diagonals `shouldEqual` [[X], [O], [O], [X]]

    it "calculates diagonals of a 1x2 board" $ do
      let board = boardOfArray [[X, O]]
      let cfg = configOfRowsColumns 1 2
      let diagonals = runReader (boardDiagonals board) cfg
      boardToArray diagonals `shouldEqual` [[X], [O], [X], [O]]

    it "calculates diagonals of a 1x3 board" $ do
      let board = boardOfArray [[X, O, X]]
      let cfg = configOfRowsColumns 1 3
      let diagonals = runReader (boardDiagonals board) cfg
      boardToArray diagonals `shouldEqual` [[X], [O], [X], [X], [O], [X]]

    it "calculates diagonals of a 3x1 board" $ do
      let board = boardOfArray [[X], [O], [X]]
      let cfg = configOfRowsColumns 3 1
      let diagonals = runReader (boardDiagonals board) cfg
      boardToArray diagonals `shouldEqual` [[X], [O], [X], [X], [O], [X]]

    it "calculates diagonals of a 2x2 board" $ do
      let board = boardOfArray
            [
              [X, B],
              [O, B]
            ]
      let cfg = configOfRowsColumns 2 2
      let diagonals = runReader (boardDiagonals board) cfg
      let expectedDiagonals =
            [
              [X],
              [B, O],
              [B],
              [O],
              [X, B],
              [B]
            ]
      boardToArray diagonals `shouldEqual` expectedDiagonals

  describe "Winner" $ do
    let cfg = configOfRowsColumns 2 2 # configWithWin 2

    it "no winner" $ do
      let board = boardOfArray
            [
              [X, B],
              [O, B]
            ]
      let winner_ = runReader (winner board) cfg
      winner_ `shouldEqual` Nothing

    it "winner in a row" $ do
      let board = boardOfArray
            [
              [X, X],
              [O, B]
            ]
      let winner_ = runReader (winner board) cfg
      winner_ `shouldEqual` Just X

    it "winner in a column" $ do
      let board = boardOfArray
            [
              [X, B],
              [X, O]
            ]
      let winner_ = runReader (winner board) cfg
      winner_ `shouldEqual` Just X

    it "winner in a diagonal" $ do
      let board = boardOfArray
            [
              [X, B],
              [O, X]
            ]
      let winner_ = runReader (winner board) cfg
      winner_ `shouldEqual` Just X

  describe "TryAdd" $ do
    it "to a full column" $ do
      let column' = tryAddToColumn O $ List.fromFoldable [X, O]
      column' `shouldEqual` Nothing

    it "to a not empty column" $ do
      let column' = tryAddToColumn X $ List.fromFoldable [B, O]
      column' `shouldEqual` (Just $ List.fromFoldable [X, O])

  describe "NextMoves" $ do
    it "of an empty 1x1 board" $ do
      let board = boardOfArray [[B]]
      let nextMoves' = Array.fromFoldable $ map boardToArray $ nextMoves O board
      nextMoves' `shouldEqual` [[[O]]]

    it "of an empty 2x1 board" $ do
      let board = boardOfArray [[B, B]]
      let nextMoves' = Array.fromFoldable $ map boardToArray $ nextMoves O board
      let expectedMoves =
            [
              [[O, B]],
              [[B, O]]
            ]
      nextMoves' `shouldEqual` expectedMoves

    it "of an empty 2x2 board" $ do
      let board = boardOfArray [[B, B], [B, B]]
      let nextMoves' = Array.fromFoldable $ map boardToArray $ nextMoves O board
      let expectedMoves =
            [
              [
                [B, B],
                [O, B]
              ],
              [
                [B, B],
                [B, O]
              ]
            ]
      nextMoves' `shouldEqual` expectedMoves

    it "of a non-empty 2x2 board" $ do
      let board = boardOfArray [[B, B], [X, B]]
      let nextMoves' = Array.fromFoldable $ map boardToArray $ nextMoves O board
      let expectedMoves =
            [
              [
                [O, B],
                [X, B]
              ],
              [
                [B, B],
                [X, O]
              ]
            ]
      nextMoves' `shouldEqual` expectedMoves

    it "of an 2x2 board with one full column" $ do
      let board = boardOfArray [[O, B], [X, B]]
      let nextMoves' = Array.fromFoldable $ map boardToArray $ nextMoves O board
      let expectedMoves =
            [
              [
                [O, B],
                [X, O]
              ]
            ]
      nextMoves' `shouldEqual` expectedMoves

    it "of a full 2x2 board" $ do
      let board = boardOfArray [[O, X], [X, O]]
      let nextMoves' = Array.fromFoldable $ map boardToArray $ nextMoves O board
      nextMoves' `shouldEqual` []

  describe "BuildGameTree" $ do
    it "for a board from video" $ do
      let board = boardOfArray
           [
              [B, B, X],
              [B, X, O],
              [O, O, X]
           ]
      let cfg = configOfRowsColumns 3 3 # configWithWin 3
      let gameTree = runReader (buildGameTree O board) cfg
      let value = treeValue gameTree
      value.winner `shouldEqual` WinnerInChildren X

    it "for after the first move" $ do
      let board = boardOfArray
           [
              [B, B, B],
              [B, B, B],
              [O, B, B]
           ]
      let cfg = configOfRowsColumns 3 3 # configWithWin 3
      let gameTree = runReader (buildGameTree O board) cfg
      let children = treeChildren gameTree
      List.length children `shouldEqual` 3

  describe "NextMove" $ do
    it "when only one move remains" $ do
      let board = boardOfArray
           [
              [X, X, B],
              [O, O, X],
              [O, X, O]
           ]
      let cfg = configOfRowsColumns 3 3 # configWithWin 3 # configWithDepth 7
      move <- runReaderT (nextMove O board) cfg

      case move of
        Just m -> do
          let actualBoard = nextBoardFromMove m
          let expectedBoard = boardOfArray
               [
                  [X, X, O],
                  [O, O, X],
                  [O, X, O]
               ]
          actualBoard `shouldEqual` expectedBoard
        Nothing -> fail "Should find a single move"
