module TicTacToe.HumanPlayer 
  (humanPlayer)
where

import TicTacToe.Game
import TicTacToe.Grid

import Data.Ix (range)
import Control.Monad.Trans (MonadIO, liftIO)


-- | Creates a human player
humanPlayer :: (MonadIO m) => Mark -> Player m m
humanPlayer mark = Player {
  plMark = mark,
  plTurn = liftIO . turn mark,
  plForbidden = liftIO . forbidden,
  plGameOver = (liftIO .) . gameOver,
  plLift = id,
  plRun = id
}

keysEquiv = zip ["a", "z", "e", "q", "s", "d", "w", "x", "c"] $
                range ((-1,-1), (1,1))

turn myMark grid = do
  putStrLn . show $ grid
  putStrLn $ show myMark ++ ": Cell?"
  cell <- getLine
  case lookup cell keysEquiv of
    Nothing -> do putStrLn $ "Unauthorized position: " ++ cell
                  turn myMark grid
    Just pos -> return pos

forbidden pos = do
  putStrLn $ "You cannot play in " ++ show pos ++ "!"

gameOver grid result = do
  putStrLn . show $ grid
  putStrLn $ case result of
    Draw -> "Draw Game!"
    Victory mark alg -> "'" ++ show mark ++ "'" ++ " wins in " ++ show alg ++ "!"

