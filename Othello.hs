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

points :: Player -> Int
points (Player _ x) = x

disk :: Player -> Disk
disk (Player x _) =  x

startingBoard :: Board
startingBoard =
  M.insert (4, 4) (Just Black)
    . M.insert (4, 3) (Just White)
    . M.insert (3, 4) (Just White)
    $ M.insert (3, 3) (Just Black) blank
  where blank = M.fromList [ (p, Nothing) | p <- positions ]

testBoard :: Board
testBoard = M.fromList [((0,0),Nothing),((0,1),Nothing),((0,2),Nothing),((0,3),Nothing),((0,4),Nothing),((0,5),Nothing),((0,6),Nothing),((0,7),Nothing),((1,0),Nothing),((1,1),Nothing),((1,2),Nothing),((1,3),Nothing),((1,4),Nothing),((1,5),Nothing),((1,6),Nothing),((1,7),Nothing),((2,0),Nothing),((2,1),Nothing),((2,2),Nothing),((2,3),Nothing),((2,4),Nothing),((2,5),Nothing),((2,6),Nothing),((2,7),Nothing),((3,0),Nothing),((3,1),Nothing),((3,2),Nothing),((3,3),Just Black),((3,4),Just Black),((3,5),Just Black),((3,6),Nothing),((3,7),Nothing),((4,0),Nothing),((4,1),Nothing),((4,2),Nothing),((4,3),Just White),((4,4),Just Black),((4,5),Nothing),((4,6),Nothing),((4,7),Nothing),((5,0),Nothing),((5,1),Nothing),((5,2),Nothing),((5,3),Nothing),((5,4),Nothing),((5,5),Nothing),((5,6),Nothing),((5,7),Nothing),((6,0),Nothing),((6,1),Nothing),((6,2),Nothing),((6,3),Nothing),((6,4),Nothing),((6,5),Nothing),((6,6),Nothing),((6,7),Nothing),((7,0),Nothing),((7,1),Nothing),((7,2),Nothing),((7,3),Nothing),((7,4),Nothing),((7,5),Nothing),((7,6),Nothing),((7,7),Nothing)]

-- Gets the given player's disk color
getPlayerCol :: Player -> Disk
getPlayerCol (Player d _) = d

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

-- | disks flipped in direction
flippedDir :: Disk -> Pos -> Board -> Dir -> [Pos]
flippedDir currentDisk p0 b dir = getPositions p0 []
 where
  getPositions :: Pos -> [Pos] -> [Pos]
  getPositions p ps 
        | nextDisk == Just Nothing || isNothing nextDisk = [] -- next position has no disk
        | fromJust nextDisk == Just currentDisk          = ps -- next position is same color as start disk
        | otherwise                                      = getPositions nextPos (nextPos : ps) -- next position is other color                                                                 
   where
    nextDisk = M.lookup nextPos b
    nextPos = step dir p

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

-- | all disks flipped
flipped :: Player -> Board -> Pos -> [Pos]
flipped (Player d _) b p = concat [ flippedDir d p b dir | dir <- [North ..] ]

-- | check if position of move is valid
isValid :: Player -> Board -> Pos -> Bool
isValid pl b p = not . null $ flipped pl b p

-- game is over when whole board is filled with disks or none of the players can play
gameOver :: Player -> Player -> Board -> Bool
gameOver p1 p2 b = noPlays || fullBoard b
      where noPlays = (not $ canPlay p1 b) && (not $ canPlay p2 b)

-- Returns true if all positions on board has a disk
fullBoard :: Board -> Bool
fullBoard b = (bPoints + wPoints) == 64
      where (bPoints,wPoints) = updatePoints b

-- Returns true if player can make a move on the board 
canPlay :: Player -> Board -> Bool
canPlay p b = not $ null $ possibleMoves p b

-- | returns a tuple of the number of black and white disks on the board
updatePoints :: Board -> (Int,Int)
updatePoints b = (countColor Black b, countColor White b)

-- | returns the number of occurences of the given disk in the board
countColor :: Disk -> Board -> Int 
countColor d = length . filter equalsDisk . map snd . M.toList
      where equalsDisk d'= d' == Just d

-- ############################# AI #############################

-- Makes a move and returns the updated board, returns unchanged board if move is not valid
makeMove :: Pos -> Player ->  Board -> Board
makeMove pos player b = if isValid player b pos 
                        then flipAll positions b player
                        else b
      where positions = pos:flipped player b pos -- the chosen position and the flipped positions

-- Calculates the best move for the player using the minimax algorithm
getPositionAI :: Board -> Player -> Pos
getPositionAI b player = snd $ M.findMax $ M.fromList minmaxresult  
    where playerCol = getPlayerCol player
          allValidMoves = map fst (possibleMoves player b)
          minmaxresult = map (\position -> (minimax 3 player (makeMove position player b),position)) allValidMoves
          -- minmaxresult returns [(minmax value, position)]


-- Evaluates how good a play is given the board where the play has been made 
minimax :: Int -> Player -> Board -> Int
minimax depth (Player disk _) board 
    | gameOver (Player disk 0) (Player (opponentDisk disk) 0) board = if winner (Player disk 0) board 
                                                                      then 10000 
                                                                      else -10000
    | depth <= 0 = heuristic (Player disk 0) board
    | otherwise = if nextColor /= disk       
                  then - maxPlayNextColor       -- minimizing
                  else  maxPlayNextColor        -- maximizing 
    where 
    nextColor = if canPlay (Player (opponentDisk disk) 0) board then (opponentDisk disk) else disk
    nextPlayer = Player nextColor 0
    nextPlayerPossibleMoves = map fst $ possibleMoves nextPlayer board 
    maxPlayNextColor = maximum (map (\pos -> minimax (depth-1) nextPlayer (makeMove pos nextPlayer board)) nextPlayerPossibleMoves)

-- checks if player is the winner of the board
winner :: Player -> Board -> Bool
winner (Player disk _) board = playerPoint >= oppPoint
      where playerPoint = countColor disk board 
            oppPoint = countColor (opponentDisk disk) board

-- computes the heuristic which is the player's score substracted with the opponent's score (can be improved to be smarter)
heuristic :: Player -> Board -> Int
heuristic (Player d p) b | d == Black = bScore - wScore
                         | otherwise  = wScore - bScore
      where (bScore,wScore) = updatePoints b


opponentDisk :: Disk -> Disk
opponentDisk Black = White
opponentDisk White = Black

-- TESTS ######################################################################
-- TODO add props

instance Arbitrary Disk where
      arbitrary = elements [Black, White]

instance Arbitrary Player where
      arbitrary = elements [(Player Black 0), (Player White 0)]

-- test that AI chooses positions that are possible moves
prop_getpositionAI_legal :: Board -> Player -> Bool
prop_getpositionAI_legal b p = if noPossibleMoves 
                              then True 
                              else isIn (getPositionAI b p) (map fst (possibleMoves p b))
      where
      isIn :: Pos -> [Pos] -> Bool
      isIn _ [] = False
      isIn aiPos (p:ps) = if aiPos == p then True else isIn aiPos ps
      noPossibleMoves = (length $ possibleMoves p b) == 0

-- check that range is valid of values heuristic can give
prop_heuristics :: Player -> Board -> Bool
prop_heuristics p b = heuristic p b <= 64 && heuristic p b >= -64

-- check that heuristic gives positive value if the player has more disks on the board then the opponent
prop_heuristics_color :: Player -> Board -> Bool
prop_heuristics_color (Player d _) b 
            | playerDisks >= opponentDisks = heuristic (Player d 0) b >= 0
            | otherwise                    = heuristic (Player d 0) b < 0
            where 
            playerDisks = countColor d b
            opponentDisks = countColor (opponentDisk d) b

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

