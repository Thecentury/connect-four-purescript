module GameLoop where

import ConnectFour (AIMove(..), Board, Config, Outcome(..), Player(..), boardOutcome, config, nextMove, nextPlayer, tryAddToBoard)
import OwnPrelude (liftReader)
import Prelude

import BoardComparison (CellDiff(..), diffBoards)
import Control.Monad.Reader (ReaderT)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldr)
import Data.Int (fromString)
import Data.List ((..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception.Unsafe (unsafeThrow)
import Node.Encoding as Encoding
import Node.Process as Process
import Node.ReadLine (Interface)
import Node.ReadLine as ReadLine
import Node.Stream (writeString)

question :: String -> Interface -> Aff String
question message interface = makeAff go
  where
    -- go :: (Either Error a -> Effect Unit) -> Effect Canceler
    go runAffFunction = nonCanceler <$
      ReadLine.question message (runAffFunction <<< Right) interface

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

putStr :: String -> Aff Unit
putStr s = do
  _ <- liftEffect $ writeString Process.stdout Encoding.ASCII s (\_ -> pure unit)
  pure unit

putStrLn :: String -> Aff Unit
putStrLn s = putStr (s <> "\n")

drawCell :: CellDiff -> Aff Unit
drawCell (Unchanged p) = putStr $ show p
-- todo colors
drawCell (Changed p) = putStr $ show p

-- todo move me to prelude
mapM_ :: forall a b t m. Monad m => Foldable t => (a -> m b) -> t a -> m Unit
mapM_ f = foldr c (pure unit)
  where c x k = f x *> k

forM_ :: forall a b t m. Monad m => Foldable t => t a -> (a -> m b) -> m Unit
forM_ = flip mapM_

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
          board' <- nextMove player board
          case board' of
            Just (Definite board'') -> do
                liftAff $ putStrLn "I'll win!"
                loopWithDiff console (nextPlayer player) prevBoard board''
            Just (RandomGuess board'') -> do
                liftAff $ putStrLn "I hope I'll win..."
                loopWithDiff console (nextPlayer player) prevBoard board''
            Nothing -> liftAff $ putStrLn "AI failed to make the next move."
        Human -> do
            column <- readPlayerInput console player
            let board' = tryAddToBoard player column board
            case board' of
                Just board'' -> do
                    liftAff $ putStrLn ""
                    loopWithDiff console (nextPlayer player) prevBoard board''
                Nothing -> do
                    liftAff $ do
                        putStrLn $ "Column " <> show column <> " is full, choose another one."
                        putStrLn ""
                    loopWithDiff console player prevBoard board

loop :: Interface -> Player -> Board -> ReaderT Config Aff Unit
loop console player board = loopWithDiff console player board board