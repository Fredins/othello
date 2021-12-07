module Othello where

import           Data.List.Extra                ( chunksOf )
import           Data.Map                      as M
                                         hiding ( drop
                                                , map
                                                )
import           Data.Maybe

type Board = M.Map Pos (Maybe Disk)
type Pos = (Int, Int)
data Disk = Black | White deriving (Show, Eq)
data Player = Player Disk Points deriving (Show, Eq)
type Points = Int

startingBoard :: Board
startingBoard =
  M.insert (4, 4) (Just Black)
    . M.insert (4, 3) (Just White)
    . M.insert (3, 4) (Just White)
    $ M.insert (3, 3) (Just Black) blank
  where blank = fromList [ (p, Nothing) | p <- positions ]


showBoard :: Board -> String
showBoard =
  concatMap (\x -> concat x ++ "\n") . chunksOf 8 . map (toS . snd) . toList
 where
  toS (Just x) | x == Black = " B "
               | otherwise  = " W "
  toS Nothing = " _ "

printBoard :: Board -> IO ()
printBoard = putStr . showBoard

positions :: [Pos]
positions = [ (y, x) | y <- [0 .. 7], x <- [0 .. 7] ]

flip :: Pos -> Board -> Board
flip p b = M.insert p v' b
 where
  v  = fromJust $ M.lookup p b
  v' = case v of
    Nothing      -> Nothing
    (Just Black) -> Just White
    (Just White) -> Just Black

emptyPositions :: Board -> [Pos]
emptyPositions = keys . M.filter isNothing 

possibleMoves :: Player -> Board -> [Pos]
possibleMoves p b = undefined

validFlip :: Player -> Pos -> Board -> Bool
validFlip p b = undefined
  -- where v = M.lookup p b

gameOver = undefined


