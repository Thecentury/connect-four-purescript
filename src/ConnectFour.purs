module ConnectFour where

import OwnPrelude
import Prelude

import Control.Monad.Reader (Reader, ask)
import Data.Foldable (maximum, minimum)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (List, filter, take, transpose, (..), concat, null, fromFoldable)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Unfoldable (replicate)
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

playerMinimax :: Player -> Array Player -> Player
playerMinimax B _ = B
playerMinimax _ [] = B
playerMinimax O winners = fromMaybe O (minimum winners)
playerMinimax X winners = fromMaybe X (maximum winners)

nextPlayer :: Player -> Player
nextPlayer O = X
nextPlayer B = unsafeThrow "Blank cell is not a player"
nextPlayer X = O

type BoardRow = List Player
type Column = List Player
type Board = List BoardRow

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