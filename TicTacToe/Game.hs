{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, RankNTypes #-}
module TicTacToe.Game
  (GameProgram, Player(..), gameProgram, doGame)
where

import TicTacToe.Grid

import Control.Monad.State
import Control.Monad.Operational


data Request a where
  GetGrid   :: Request Grid
  TurnDone  :: (Grid, Maybe GridResult) -> Request ()
  GetResult :: Request (Maybe GridResult)

newtype GameProgram m a = GameProgram (ProgramT Request m a)
  deriving (Functor, Monad, MonadTrans)

instance (MonadIO m) => MonadIO (GameProgram m) where
  liftIO = lift . liftIO

-- | mstack and mtop are monads. mstack is a monad stack which can be run, up to mtop
data Player mstack mtop = Player {
  -- | Gets the mark of the player
  plMark      :: Mark,
  -- | Called when the player must play
  plTurn      :: Grid -> mstack Pos,
  -- | Called when player tries to play at a forbidden position
  plForbidden :: Pos -> mstack (),
  -- | Called when game has ended.
  plGameOver  :: Grid -> GridResult -> mstack (),
  -- | Used to reach the topmost monad
  plLift      :: forall a. mtop a -> mstack a,
  -- | Used to run the monad stack the player runs in
  plRun       :: forall a. mstack a -> mtop a
}

instance Eq (Player m m') where
  p1 == p2 = plMark p1 == plMark p2

instance Show (Player m m') where
  show = show . plMark


gameProgram :: (Monad m) => Player m (GameProgram m') -> GameProgram m' ()
gameProgram pl = plRun pl loop
  where toProg = plLift pl . GameProgram . singleton
        loop = do
          grid <- toProg GetGrid
          pos <- plTurn pl grid
          case checkCell grid (plMark pl) pos of
            Nothing -> do        -- The cell was already filled in
              plForbidden pl pos -- We signal the error
              loop               -- We start the turn again
            Just newGridAndResult -> do
                                 -- The cell has been successfully marked, so we got a new grid
              toProg $ TurnDone newGridAndResult
                                 -- At this point, the interpreter will switch to the other player
              mbResult <- toProg GetResult
                                 -- This player is back, and wants to know what's new
              case mbResult of
                Nothing  -> loop
                Just res -> toProg GetGrid >>= \g -> plGameOver pl g res
 
doGame :: (Monad m) => Grid -> [GameProgram m ()] -> m ()
doGame initGrid players =
  mapM unwrap players >>= flip evalStateT (initGrid, Nothing) . eval
  where 
    unwrap (GameProgram prog) = viewT prog

    eval :: (Monad m) => [ProgramViewT Request m ()] ->
                         StateT (Grid, Maybe GridResult) m ()

    eval [] = return ()

    eval ((Return _) : progs) = eval progs

    eval ((GetGrid :>>= prog) : progs) = do
      (grid, _) <- get
      p <- lift . viewT $ prog grid
      eval $ p : progs

    eval ((TurnDone (newGrid, mbResult) :>>= prog) : progs) = do
      put (newGrid, mbResult)
      p <- lift . viewT $ prog ()
      eval $ progs ++ [p]

    eval ((GetResult :>>= prog) : progs) = do
      p <- lift . viewT . prog =<< liftM snd get
      eval $ p : progs

