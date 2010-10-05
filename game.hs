module Main where

import TicTacToe.Grid
import TicTacToe.Game
import TicTacToe.HumanPlayer
import TicTacToe.ArtificialPlayer
import TicTacToe.NetPlayer

import System.Environment (getArgs)
import Control.Monad (when)
import Data.Ix (inRange)
import Network


main = do
  level <- getAILevel
  --let pl1 = humanPlayer Cross
  handle <- waitForNetClient
  let pl1 = netPlayer handle Cross
  let pl2 = artificialPlayer level Circle
  doGame blankGrid [gameProgram pl1, gameProgram pl2]

getAILevel = do
  args <- getArgs
  let level | length args >= 1 = read $ head args :: Int
            | otherwise        = 4
  when (not $ inRange (0, 9) level) $
    error "AI level must be between 1 and 9. You're doing it wrong."
  putStrLn $ "AI is level " ++ show level
  return level

waitForNetClient = do
  (handle, clt, _) <- listenOn (PortNumber 7777) >>= accept
  putStrLn $ "Client joined from " ++ clt
  return handle

