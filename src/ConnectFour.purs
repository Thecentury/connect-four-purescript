module ConnectFour where

import OwnPrelude
import Prelude

import Control.Alt ((<|>))
import Control.Monad.Reader (Reader, ReaderT, ask)
import Data.Foldable (foldr, maximum, minimum)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (List(..), all, concat, filter, fromFoldable, head, null, reverse, take, transpose, (..))
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (replicate)
import Effect.Aff (Aff)
import Effect.Exception.Unsafe (unsafeThrow)

type Config = {
  rows :: Int,
  columns :: Int,
  win :: Int,
  maxDepth :: Int
}

defaultConfig :: Config
defaultConfig = {
  rows: 3,
  columns: 3,
  win: 3,
  maxDepth: 6
}

config :: Reader Config Config
config = ask

configOfRowsColumns :: Int -> Int -> Config
configOfRowsColumns rows columns = defaultConfig { rows = rows, columns = columns }

configWithWin :: Int -> Config -> Config
configWithWin win cfg = cfg { win = win }

configWithDepth :: Int -> Config -> Config
configWithDepth depth cfg = cfg { maxDepth = depth }

data Player = O | B | X

instance showPlayer :: Show Player where
  show O = "o"
  show B = "."
  show X = "x"

derive instance eqPlayer :: Eq Player
derive instance ordPlayer :: Ord Player

playerMinimax :: Player -> List Player -> Player
playerMinimax B _ = B
playerMinimax _ Nil = B
playerMinimax O winners = fromMaybe O (minimum winners)
playerMinimax X winners = fromMaybe X (maximum winners)

nextPlayer :: Player -> Player
nextPlayer O = X
nextPlayer B = unsafeThrow "Blank cell is not a player"
nextPlayer X = O

type BoardRow = List Player
type Column = List Player
type Board = List BoardRow

type ColumnNumber = Int

mkBoard :: Config -> Board
mkBoard cfg = replicate cfg.rows (replicate cfg.columns B)

boardRows :: Board -> List BoardRow
boardRows b = b

boardColumns :: Board -> List Column
boardColumns = transpose

boardDiagonals :: Board -> Reader Config (List BoardRow)
boardDiagonals board = do
  cfg <- config
  let growingDiagonals = (0 .. (cfg.columns + cfg.rows - 2)) # map (\shift -> shiftedRows shift (-1))
      decreasingDiagonals = -(cfg.rows - 1) .. (cfg.columns - 1) # map (\shift -> shiftedRows shift 1)
      allDiagonals = concat (fromFoldable [growingDiagonals, decreasingDiagonals]) # filter (not <<< null) # concat
  pure allDiagonals
  where
    shiftedRows :: Int -> Int -> List BoardRow
    shiftedRows shift rowMultiplier =
      board
      # mapWithIndex (\rowIndex row ->
        row
        # safeSkip (shift + rowIndex * rowMultiplier)
        # take 1)
      # filter (not <<< null)
      # transpose

winnerInRow :: BoardRow -> Reader Config (Maybe Player)
winnerInRow row = do
  cfg <- config
  let toWin = cfg.win
  pure $ impl toWin (Tuple B 0) row
  where
    impl :: Int -> (Tuple Player Int) -> List Player -> Maybe Player
    impl toWin (Tuple player count) Nil = if count == toWin then Just player else Nothing
    impl toWin (Tuple player count) (Cons x xs) = 
      if x == player then 
        impl toWin (Tuple player (count + 1)) xs 
      else 
        impl toWin (Tuple (nextPlayer player) 0) xs

winner :: Board -> Reader Config (Maybe Player)
winner board = do
  diagonals <- boardDiagonals board
  let toSearch = concat $ fromFoldable [boardRows board, boardColumns board, diagonals]
  winners <- sequence $ (map winnerInRow) $ toSearch
  pure $ fromMaybe Nothing (maximum winners)

isFullColumn :: Column -> Boolean
isFullColumn column = head column /= Just B

isFullBoard :: Board -> Boolean
isFullBoard board = all isFullColumn (boardColumns board)

tryAddToColumn :: Player -> Column -> Maybe Column
tryAddToColumn player column = 
  let (Tuple added result) = foldr go (Tuple false Nil) column
  in if added then Just result else Nothing
  where
    go :: Player -> Tuple Boolean (List Player) -> Tuple Boolean (List Player)
    go current (Tuple true soFar) = Tuple true (Cons current soFar)
    go B (Tuple _ soFar) = Tuple true (Cons player soFar)
    go current (Tuple _ soFar) = Tuple false (Cons current soFar)

tryAddToBoard :: Player -> ColumnNumber -> Board -> Maybe Board
tryAddToBoard player columnIndex board =
  map transpose $ go columnIndex Nil $ boardColumns board
  where
    go :: ColumnNumber -> List Column -> List Column -> Maybe (List Column)
    go _ _ Nil = Nothing
    go 0 soFar (Cons column rest) = 
      let column' = tryAddToColumn player column in
      case column' of
        Nothing -> Nothing
        Just column'' -> Just $ concat $ List.fromFoldable [reverse soFar, Cons column'' rest]
    go currentColumnIndex soFar (Cons column rest) =
      go (currentColumnIndex - 1) (Cons column soFar) rest

nextMoves :: Player -> Board -> List Board
nextMoves player board = 
  board
  # boardColumns
  # zipperFromList
  # zipperSelfAndRights
  # List.mapMaybe (\z -> tryAddToColumn player z.focus # map (\column -> zipperWithFocus column z))
  # map (boardColumns <<< zipperToList)

--------------------------------------------------------------------------------

data Winner =
  DepthExhausted
  | FoundWinner Player
  | WinnerInChildren Player

derive instance eqWinner :: Eq Winner

winnerToPlayer :: Winner -> Player
winnerToPlayer (FoundWinner player) = player
winnerToPlayer (WinnerInChildren player) = player
winnerToPlayer DepthExhausted = B

type GameTreeNode = {
  playerToPlay :: Player,
  board :: Board,
  winner :: Winner,
  depth :: Int
}

buildGameTree :: Player -> Board -> Reader Config (Tree GameTreeNode)
buildGameTree playerToPlay' board' = do
  cfg <- config
  result <- impl cfg.maxDepth 0 playerToPlay' board'
  pure result
  where
    impl :: Int -> Int -> Player -> Board -> Reader Config (Tree GameTreeNode)
    impl maxDepth currentDepth player board = do
      if currentDepth >= maxDepth then
        let treeValue' = {
          playerToPlay: player,
          board: board,
          winner: DepthExhausted,
          depth: currentDepth
        }
        in pure $ Tree treeValue' Nil
      else do
        let nextPlayer' = nextPlayer player
        winner' <- winner board
        (Tuple actualWinner children) <- do
          case winner' of
            Just w -> pure $ Tuple (FoundWinner w) Nil
            Nothing -> do
              let nextMoves' = nextMoves nextPlayer' board
              children <- 
                nextMoves'
                # map (impl maxDepth (currentDepth + 1) nextPlayer')
                # sequence
              let childrenWinners = map (\(Tree value _) -> winnerToPlayer value.winner) children
              let nodeWinner = playerMinimax player childrenWinners
              pure $ Tuple (WinnerInChildren nodeWinner) children
        let v = {
          playerToPlay: player,
          board: board,
          winner: actualWinner,
          depth: currentDepth
        }
        pure $ Tree v children

--------------------------------------------------------------------------------

data AIMove =
  Definite Board
  | RandomGuess Board

derive instance eqAIMove :: Eq AIMove

nextBoardFromMove :: AIMove -> Board
nextBoardFromMove (Definite board) = board
nextBoardFromMove (RandomGuess board) = board

-- todo define nextMove with randomness
nextMove :: Player -> Board -> ReaderT Config Aff (Maybe AIMove)
nextMove currentPlayer board = do
  tree <- liftReader $ buildGameTree (nextPlayer currentPlayer) board
  let definiteGuess =
        tree
        # treeChildren
        # map treeValue
        # filter (\child -> winnerToPlayer child.winner == currentPlayer)
        # head
        # map (\child -> Definite child.board)
  let candidateOne = definiteGuess
  let candidateTwo =
        tree
        # treeChildren
        # map treeValue
        # filter (\child -> winnerToPlayer child.winner == B)
        # map (\child -> child.board)
        # randomMove
  let candidateThree =
        tree
        # treeChildren
        # map treeValue
        # map _.board
        # randomMove
  pure $ candidateOne <|> candidateTwo <|> candidateThree

  where
    randomMove :: List Board -> Maybe AIMove
    randomMove Nil = Nothing
    randomMove (Cons b _) = Just $ RandomGuess b

--------------------------------------------------------------------------------

data Outcome =
  Win Player
  | Draw
  | InProgress

derive instance eqOutcome :: Eq Outcome

boardOutcome :: Board -> Reader Config Outcome
boardOutcome board = do
  winner' <- winner board
  case winner' of
    Just w -> pure $ Win w
    Nothing | isFullBoard board -> pure Draw
    Nothing -> pure InProgress