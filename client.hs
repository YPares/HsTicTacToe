import TicTacToe.NetPlayerClient
import TicTacToe.HumanPlayer
--import TicTacToe.ArtificialPlayer

import Network
import Control.Monad.Trans (liftIO)
import System.Environment (getArgs)


main = do
  host:port:_ <- getArgs
  handle <- connectTo host $ PortNumber $ fromIntegral $ read port
  netPlayerClient handle humanPlayer

