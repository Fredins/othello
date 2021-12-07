module Othello where

import           Data.List.Extra                ( chunksOf )
import qualified Data.Map                      as M
                                         hiding ( drop
                                                , map
                                                )
import           Data.Maybe
import           Text.Pretty.Simple.Internal.Printer
                                                ( CheckColorTty(CheckColorTty) )

type Board = M.Map Pos (Maybe Disk)
type Pos = (Int, Int)
data Disk = Black | White deriving (Show, Eq)
data Player = Player Disk Points
  deriving (Show, Eq)
type Points = Int

startingBoard :: Board
startingBoard =
  M.insert (4, 4) (Just Black)
    . M.insert (4, 3) (Just White)
    . M.insert (3, 4) (Just White)
    $ M.insert (3, 3) (Just Black) blank
  where blank = M.fromList [ (p, Nothing) | p <- positions ]


showBoard :: Board -> String
showBoard =
  concatMap (\x -> concat x ++ "\n") . chunksOf 8 . map (toS . snd) . M.toList
 where
  toS (Just x) | x == Black = " B "
               | otherwise  = " W "
  toS Nothing = " _ "

printBoard :: Board -> IO ()
printBoard = putStr . showBoard

positions :: [Pos]
positions = [ (y, x) | y <- [0 .. 7], x <- [0 .. 7] ]

updateBoard :: Pos -> Board -> Board
updateBoard p b = M.insert p v' b
 where
  v  = fromJust $ M.lookup p b
  v' = case v of
    Nothing      -> Nothing
    (Just Black) -> Just White
    (Just White) -> Just Black

flip :: Pos -> Board -> Player -> Board
flip = undefined

emptyPositions :: Board -> [Pos]
emptyPositions = M.keys . M.filter isNothing

possibleMoves :: Player -> Board -> [(Pos, [Pos])]
possibleMoves pl b = undefined
  where fp = (map . filter) _ $x 2==0, x/=0 ]  emptyPositions b
        valid p  | not $ null ps = (p, ps)
                 | otherwise     = undefined
          where ps = flippedPositions pl b p


data Dir = North
         | NorthEast
         | East
         | SouthEast
         | South
         | SouthWest
         | West
         | NorthWest

step :: Dir -> Pos -> Pos
step d (y, x) = case d of
  North     -> (y + 1, x)
  NorthEast -> (y + 1, x + 1)
  East      -> (y, x + 1)
  SouthEast -> (y - 1, x + 1)
  South     -> (y - 1, x)
  SouthWest -> (y - 1, x - 1)
  West      -> (y, x - 1)
  NorthWest -> (y + 1, x - 1)


lane :: Disk -> Pos -> Board -> Dir -> [Pos]
lane d0 p0 b dir = lane' p0 []
 where
  lane' :: Pos -> [Pos] -> [Pos]
  lane' p ps
    | isNothing d'           = []                   -- return empty list if out of bounds
    | fromJust d' == Just d0 = ps                   -- return inbetween positions if same Disk color
    | otherwise              = lane' p (p' : ps)    -- continue searching if not same disk color   
   where
    d' = M.lookup p' b
    d  = M.lookup p b
    p'    = step dir p

flippedPositions  :: Player -> Board -> Pos -> [Pos]
flippedPositions (Player d _) b p = concat [lane d p b dir | dir <- [North ..]]

isValid :: Player -> Board -> Pos -> Bool
isValid = not . null . flippedPositions 

gameOver = undefined


