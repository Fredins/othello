module Othello where

import           Data.List                      ( sort )
import           Data.List.Extra                ( chunksOf )
import qualified Data.Map                      as M
                                         hiding ( drop
                                                , map
                                                )
import           Data.Maybe
import           Debug.Trace                    ( trace )
import           Test.QuickCheck

{-
startingBoard
showBoard
printBoard
positions
updateBoard: Pos ->  Color -> Board -> Board
      - adds new disc with the right color
      - calls flip: flips discs
flip: Pos -> Board -> Player -> Board
      - 
findFlips: 
possibleMoves
emptyPositions
validFlip: Pos -> Board -> Player -> [Pos]
      - check if position is empty
      - check if positions around position is opposite color
      - 
gameOver
-}


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



positions :: [Pos]
positions = [ (y, x) | y <- [0 .. 7], x <- [0 .. 7] ]

-- | TODO
updateBoard :: Pos -> Board -> Board
updateBoard p b = M.insert p v' b
 where
  v  = fromJust $ M.lookup p b
  v' = case v of
    Nothing      -> Nothing
    (Just Black) -> Just White
    (Just White) -> Just Black

-- | TODO
flipAll :: [Pos] -> Board -> Player -> Board
flipAll [] b _            = b 
flipAll ps b (Player d _) = flip ps b
 where flip [] b     = b
       flip (p:ps) b = flip ps (M.insert p (Just d) b) 
  


-- | all empty postions
emptyPositions :: Board -> [Pos]
emptyPositions = M.keys . M.filter isNothing

-- | returns all the possible moves together with the pieces that would be flipped
possibleMoves :: Player -> Board -> [(Pos, [Pos])]
possibleMoves pl b =
  filter f . map (\p -> (p, flipped pl b p)) $ emptyPositions b
 where
  f (_, []) = False
  f _       = True


-- | all directions
data Dir = North
         | NorthEast
         | East
         | SouthEast
         | South
         | SouthWest
         | West
         | NorthWest deriving (Enum, Show)


-- | change position depending on direction (in grid)
step :: Dir -> Pos -> Pos
step d (y, x) = case d of
  North     -> (y - 1, x)
  NorthEast -> (y - 1, x + 1)
  East      -> (y, x + 1)
  SouthEast -> (y + 1, x + 1)
  South     -> (y + 1, x)
  SouthWest -> (y + 1, x - 1)
  West      -> (y, x - 1)
  NorthWest -> (y - 1, x - 1)

-- | disks flipped in direction
flippedDir :: Disk -> Pos -> Board -> Dir -> [Pos]
flippedDir d0 p0 b dir = check p0 []
 where
  check :: Pos -> [Pos] -> [Pos]
  check p ps | d' == Just Nothing || isNothing d' = []
             |                 -- blank or out
               fromJust d' == Just d0             = ps
             |                 -- same disk
               otherwise                          = check p' (p' : ps) -- not same disk
   where
    d' = M.lookup p' b
    d  = M.lookup p b
    p' = step dir p

-- | all disks flipped
flipped :: Player -> Board -> Pos -> [Pos]
flipped (Player d _) b p = concat [ flippedDir d p b dir | dir <- [North ..] ]

-- | check if position of move is valid
isValid :: Player -> Board -> Pos -> Bool
isValid pl b p = not . null $ flipped pl b p

-- TODO
gameOver = undefined

-- TESTS ######################################################################

-- | possibleMoves correct for startingBoard 
prop_possibleMoves_startingBoard :: Bool
prop_possibleMoves_startingBoard = sort actual == sort expected
 where
  expected =
    [ ((3, 2), [(3, 3)])
    , ((2, 3), [(3, 3)])
    , ((4, 5), [(4, 4)])
    , ((5, 4), [(4, 4)])
    ]
  actual = possibleMoves (Player White 0) startingBoard

-- PRINTING ####################################################################

showPossibleMoves :: Player -> Board -> String 
showPossibleMoves pl b = concatMap (\x -> concat x ++ "\n") . chunksOf 8 . map toS $ M.toList b
  where pm = map fst $ possibleMoves pl b
        toS (p, Just x) | x == Black = " B "
                        | otherwise  = " W "
        toS (p , Nothing) | p `elem` pm = " * "
                          | otherwise   = " _ "
printPossibleMoves :: Player -> Board -> IO()
printPossibleMoves pl = putStr . showPossibleMoves pl

showBoard :: Board -> String
showBoard =
  concatMap (\x -> concat x ++ "\n") . chunksOf 8 . map (toS . snd) . M.toList
 where
  toS (Just x) | x == Black = " B "
               | otherwise  = " W "
  toS Nothing = " _ "

printBoard :: Board -> IO ()
printBoard = putStr . showBoard


