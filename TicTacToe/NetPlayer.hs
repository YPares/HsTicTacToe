module TicTacToe.NetPlayer 
  (netPlayer, ComStateT, send, recv, runComStateT, ReqFromServer(..))
where

import TicTacToe.Game
import TicTacToe.Grid

import Control.Monad.State
import System.IO (Handle, hFlush)
import qualified Data.ByteString.Lazy as L
import qualified Data.Binary as B
import qualified Data.Binary.Get as B


type ComStateT = StateT (L.ByteString, Handle)

-- | Creates a network player
netPlayer :: (MonadIO m) => Handle -> Mark -> Player (ComStateT m) m
netPlayer handle mark = Player {
  plMark = mark,
  plTurn = \g -> send (Turn g) >> recv,
  plForbidden = send . Forbidden,
  plGameOver = (send .) . GameOver,
  plLift = lift,
  plRun = \netPlAction -> runComStateT handle $ do
    send mark
    netPlAction
}


-- We could use the package binary-protocol, but the problem is that
-- it runs only in IO for now, not in ant MonadIO.
send :: (MonadIO m, B.Binary a) => a -> ComStateT m ()
send a = do
  (_, handle) <- get
  liftIO $ do L.hPut handle $ B.encode a
              hFlush handle

recv :: (MonadIO m, B.Binary a) => ComStateT m a
recv = do
  (inp, h) <- get
  let (val, restInp, _) = B.runGetState B.get inp 0
  put (restInp, h)
  return val

runComStateT :: (MonadIO m) => Handle -> ComStateT m a -> m a
runComStateT handle action = do
  inp <- liftIO $ L.hGetContents handle
  evalStateT action (inp, handle)


data ReqFromServer = Turn Grid | Forbidden Pos | GameOver Grid GridResult
  deriving (Show)

instance B.Binary ReqFromServer where
  put (Turn g)      = B.putWord8 0 >> B.put g
  put (Forbidden p) = B.putWord8 1 >> B.put p
  put (GameOver g r)  = B.putWord8 2 >> B.put g >> B.put r
  get = do x <- B.getWord8
           case x of
             0 -> liftM Turn B.get
             1 -> liftM Forbidden B.get
             2 -> liftM2 GameOver B.get B.get

