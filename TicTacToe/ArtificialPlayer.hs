module TicTacToe.ArtificialPlayer 
  (artificialPlayer, gridScore)
where

import TicTacToe.Game
import TicTacToe.Grid

import Data.List (maximumBy, minimumBy)
import Control.Monad


-- | Creates an artificial player
artificialPlayer :: (Monad m) => Int -> Mark -> Player m m
artificialPlayer level mark = Player {
  plMark = mark,
  plTurn = turn mark level,
  plForbidden = forbidden,
  plGameOver = \_ _ -> return (),
  plLift = id,
  plRun = id
}

turn myMark level grid = return . snd $ minMax myMark myMark level grid

-- | Given the mark of the IA (twice) and a grid, gives the position to play and the reached score
minMax :: Mark -> Mark -> Int -> Grid -> (Int, Pos)
minMax myMark _    0     grid = (gridScore grid myMark, error "MinMax has a recursion level of 0. You're doing it wrong.")
minMax myMark mark level grid =
  let nextScores = flip map (authorizedMoves grid) $ \pos ->
                     let (g, mbResult) = maybe (err pos) id $ checkCell grid mark pos
                         (score, _) = case mbResult of
                                        Nothing -> minMax myMark (coMark mark) (level-1) g
                                        _       -> minMax myMark mark          0         g
                     in (score, pos)
      chooseFunc | myMark == mark = maximumBy
                 | otherwise      = minimumBy
      err pos = error $ "The position " ++ show pos ++
                        " was declared as authorized but in fact was not! Panic! Panic!"
  in chooseFunc (\(sc1, _) (sc2, _) -> compare sc1 sc2) nextScores

forbidden pos =
  error $ "IA tried to play at an unauthorized position (" ++ show pos ++ ")! Panic! Panic!"

-- | Gives the score of a status of the game grid
gridScore :: Grid -> Mark -> Int
gridScore grid mark = foldr (\alg acc -> acc + algScore alg) 0 allAlgs
  where allAlgs = map (getAlignment grid) $
                      [MainDiag, SecondDiag] ++ ([-1..1] >>= \x -> [Row x, Column x])
        algScore alg = eval mark - eval (coMark mark)
          where
            onlyMarks = filter (/= Nothing) alg
            eval m | all (== Just m) onlyMarks =
                      case length onlyMarks of
                        3 -> 9
                        2 -> 1
                        _ -> 0
                   | otherwise = 0

