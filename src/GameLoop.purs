module GameLoop where

import Prelude

import Ansi.Codes (Color(..))
import Ansi.Output (foreground, withGraphics)
import BoardComparison (CellDiff(..), diffBoards)
import ConnectFour (AIMove(..), Board, Config, Outcome(..), Player(..), boardOutcome, config, nextMove, nextPlayer, tryAddToBoard)
import Control.Monad.Reader (ReaderT)
import Data.Int (fromString)
import Data.List ((..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Exception.Unsafe (unsafeThrow)
import Node.ReadLine (Interface)
import OwnPrelude (forM_, liftReader, putStr, putStrLn, question)

readPlayerInput :: Interface -> Player -> ReaderT Config Aff Int
readPlayerInput console player = do
    cfg <- liftReader config
    liftAff $ impl cfg
    where
        impl :: Config -> Aff Int
        impl cfg = do
            columnString <- question ("Player " <> show player <> ", choose a column: ") console
            case fromString columnString of
                Just column | column >= 1 && column <= cfg.columns -> pure $ column - 1
                _ -> do
                    putStrLn "Invalid column, try again."
                    impl cfg

data PlayerKind = AI | Human

playerKind :: Player -> PlayerKind
playerKind X = AI
playerKind O = Human
playerKind _ = unsafeThrow "Invalid player B"

--------------------------------------------------------------------------------

drawCell :: CellDiff -> Aff Unit
drawCell (Unchanged p) = putStr $ show p
drawCell (Changed p) = putStr $ withGraphics (foreground BrightRed) $ show p

drawDiffBoard :: Board -> Board -> ReaderT Config Aff Unit
drawDiffBoard prev curr = do
    cfg <- liftReader config
    let diff = diffBoards prev curr
    liftAff $ do 
        forM_ diff $ \row -> do
            forM_ row drawCell
            putStrLn ""

        forM_ (1 .. cfg.columns) (const $ putStr "-")
        putStrLn ""

        forM_ (1 .. cfg.columns) $ putStr <<< show
        putStrLn ""
        putStrLn ""

--------------------------------------------------------------------------------

loopWithDiff :: Interface -> Player -> Board -> Board -> ReaderT Config Aff Unit
loopWithDiff console player prevBoard board = do
  drawDiffBoard prevBoard board
  outcome <- liftReader $ boardOutcome board
  case outcome of
    Win winner' -> liftAff $ putStrLn $ "Player " <> show winner' <> " wins!"
    Draw -> liftAff $ putStrLn "Draw!"
    InProgress ->
      case playerKind player of
        AI -> do
          nextBoard <- nextMove player board
          case nextBoard of
            Just (Definite board') -> do
                liftAff $ putStrLn "I'll win!"
                loopWithDiff console (nextPlayer player) board board'
            Just (RandomGuess board') -> do
                liftAff $ putStrLn "I hope I'll win..."
                loopWithDiff console (nextPlayer player) board board'
            Nothing -> liftAff $ putStrLn "AI failed to make the next move."
        Human -> do
            column <- readPlayerInput console player
            let nextBoard = tryAddToBoard player column board
            case nextBoard of
                Just board' -> do
                    liftAff $ putStrLn ""
                    loopWithDiff console (nextPlayer player) board board'
                Nothing -> do
                    liftAff $ do
                        putStrLn $ "Column " <> show column <> " is full, choose another one."
                        putStrLn ""
                    loopWithDiff console player prevBoard board

loop :: Interface -> Player -> Board -> ReaderT Config Aff Unit
loop console player board = loopWithDiff console player board board