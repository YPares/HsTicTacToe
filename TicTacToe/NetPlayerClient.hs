module TicTacToe.NetPlayerClient
  (netPlayerClient)
where

import TicTacToe.Game
import TicTacToe.Grid
import TicTacToe.NetPlayer

import System.IO (Handle)
import Control.Monad.Trans


-- | Creates a client for a NetPlayer from any type of player
netPlayerClient :: (MonadIO m) =>
                   Handle ->
                   (Mark -> Player m m') ->
                   m ()
netPlayerClient handle genPlayer = runComStateT handle $ do
  mark <- recv -- First thing: receive the mark
  let pl = genPlayer mark
      loop = do
        reqSrv <- recv
        case reqSrv of
          Turn      grid     -> lift (plTurn pl grid) >>= send >> loop
          Forbidden pos      -> lift (plForbidden pl pos) >> loop
          GameOver  grid res -> lift $ plGameOver pl grid res
  loop

