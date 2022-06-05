module BoardComparison where

import Prelude

import ConnectFour (Player, Board)
import Data.List (List, zipWith)

data CellDiff =
    Unchanged Player
    | Changed Player

cellDiff :: Player -> Player -> CellDiff
cellDiff prev curr
    | prev == curr = Unchanged curr
    | otherwise = Changed curr

diffBoards :: Board -> Board -> List (List CellDiff)
diffBoards = zipWith (zipWith cellDiff)