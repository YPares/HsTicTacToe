{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module TicTacToe.Grid
  (Mark(..), coMark, Pos, Grid, blankGrid, checkCell, getCell, gridFull,
   Alignment(..), getAlignment, GridResult(..), authorizedMoves)
where

import Data.Array
import Data.List (foldl', groupBy, sortBy, find)
import Control.Monad
import Data.Binary


data Mark = Cross | Circle
  deriving (Eq, Enum)

coMark Circle = Cross
coMark Cross  = Circle

instance Show Mark where
  show Circle = "O"
  show Cross  = "X"

instance Binary Mark where
  put Circle = put True
  put Cross = put False
  get = do b <- get
           return $ case b of
             True  -> Circle
             False -> Cross

type Pos = (Int, Int)

newtype Grid = Grid (Array Pos (Maybe Mark))
  deriving (Eq, Binary)
 
instance Show Grid where
  show (Grid arr) = unlines $
    flip map linesIndices $
      (++ "|") . foldl' (\acc i -> let cell = maybe " " show (arr ! i)
                                   in acc ++ "|" ++ cell)
                        ""
    where linesIndices = groupBy (\(a, _) (b, _) -> a == b) $ indices arr

data Alignment = Row Int | Column Int | MainDiag | SecondDiag
  deriving (Eq, Show)

instance Binary Alignment where
  put (Row x)    = putWord8 0 >> put x
  put (Column x) = putWord8 1 >> put x
  put MainDiag   = putWord8 2
  put SecondDiag = putWord8 3
  get = do n <- getWord8
           case n of
             0 -> liftM Row get
             1 -> liftM Column get
             2 -> return MainDiag
             3 -> return SecondDiag

data GridResult = Victory Mark Alignment | Draw
  deriving (Eq, Show)

instance Binary GridResult where
  put (Victory m alg) = putWord8 0 >> put m >> put alg
  put Draw            = putWord8 1
  get = do n <- getWord8
           case n of
             0 -> liftM2 Victory get get
             1 -> return Draw

-- | A blank 3x3 tic-tac-toe grid. Coordinates goes from (-1,-1) (TL corner) up to (1,1) (BR corner)
blankGrid :: Grid
blankGrid = Grid $ listArray ((-1, -1), (1, 1)) $ repeat Nothing

-- | Fills in a cell either with a cross or a circle.
--   Returns Nothing if the cell is already filled in
--   Else, returns the grid and the result of the grid (if someone wins or if the game is draw)
checkCell :: Grid -> Mark -> Pos -> Maybe (Grid, Maybe GridResult)
checkCell grid@(Grid arr) mark pos = 
  if arr ! pos == Nothing
    then Just $ (newGrid, getGridResult newGrid pos)
    else Nothing
  where newGrid = Grid $ arr // [(pos, Just mark)]

-- | Gets the mark inside a certain cell
getCell :: Grid -> Pos -> Maybe Mark
getCell (Grid arr) pos = arr ! pos
 
-- | Tells whether the grid is full or not
gridFull :: Grid -> Bool
gridFull (Grid arr) = find (not . isChecked) (assocs arr) == Nothing
  where isChecked (_, Just _) = True
        isChecked _           = False

-- | Gets the marks inside an alignment
getAlignment :: Grid -> Alignment -> [Maybe Mark]
getAlignment (Grid arr) align = flip map [-1..1] $ (arr !) . case align of
  Row r      -> \c -> (r, c)
  Column c   -> \r -> (r, c)
  MainDiag   -> \i -> (i, i)
  SecondDiag -> \i -> (-i, i)

-- | Gets the row, the column and possibly the diagonal(s) of a position
getNeighborhood :: Grid -> Pos -> [(Alignment, [Maybe Mark])]
getNeighborhood grid (r, c) =
  map alg [Row r, Column c]
  `mplus`
  do guard $ r == c
     [alg MainDiag]
  `mplus`
  do guard $ r == (-c)
     [alg SecondDiag]
  
  where alg a = (a, getAlignment grid a)

-- | Gives the result of a grid (if it exists), i.e. if a player has won or if the game is draw
--   Takes a grid, but also a pos (corresponding to the last played move) so as to limit the search
getGridResult :: Grid -> Pos -> Maybe GridResult
getGridResult grid pos =
  (mbWinningNbh >>= return . Victory mark) `mplus` (if gridFull grid then Just Draw else Nothing)
  where Just mark = getCell grid pos
        nbh = getNeighborhood grid pos
        mbWinningNbh = fmap fst $ flip find nbh $ \(_, alg) ->
          all (== Just mark) alg

-- | Gets the list of the positions where a mark can be placed
authorizedMoves :: Grid -> [(Int, Int)]
authorizedMoves (Grid arr) = foldr checkPos [] $ assocs arr
  where checkPos (pos, Nothing) acc = pos:acc
        checkPos (pos, _      ) acc = acc

