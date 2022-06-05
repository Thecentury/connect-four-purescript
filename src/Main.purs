module Main where

import Prelude

import ConnectFour (Config, Player(..), mkBoard)
import Control.Monad.Reader (runReaderT)
import Effect (Effect)
import Effect.Console (log)
import GameLoop (loop)
import Effect.Aff (Aff, runAff_)
import Node.ReadLine (Interface, createConsoleInterface, noCompletion, close)

cfg :: Config
cfg = {
  rows: 3,
  columns: 3,
  win: 3,
  maxDepth: 10
}

main :: Effect Unit
main = do
  log ""
  log "Welcome to the Purescript version of Connect Four game!"
  log ""

  interface <- createConsoleInterface noCompletion

  runAff_
    -- Ignore any errors and output and just close the interface
    (\_ -> close interface)
    (useInterface interface)

  where
    useInterface :: Interface -> Aff Unit
    useInterface interface = do
      let board = mkBoard cfg
      runReaderT (loop interface O board) cfg
