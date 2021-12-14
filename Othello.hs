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


-- ################################### Data Types ###################################
type Board = M.Map Pos (Maybe Disk)
type Pos = (Int, Int)
data Disk = Black | White deriving (Show, Eq)
data Player = Player Disk Points
  deriving (Show, Eq)
type Points = Int

-- | Returns the points of the player
points :: Player -> Int
points (Player _ x) = x

-- | Returns the disk of the player
disk :: Player -> Disk
disk (Player x _) =  x

opponentDisk :: Disk -> Disk
opponentDisk Black = White
opponentDisk White = Black

-- | Creates a starting board, with two white and two black disks in the middle of the board
startingBoard :: Board
startingBoard =
  M.insert (4, 4) (Just Black)
    . M.insert (4, 3) (Just White)
    . M.insert (3, 4) (Just White)
    $ M.insert (3, 3) (Just Black) blank
  where blank = M.fromList [ (p, Nothing) | p <- positions ]

-- | Creates a testing board
testBoard :: Board
testBoard = M.fromList [((0,0),Nothing),((0,1),Nothing),((0,2),Nothing),((0,3),Nothing),((0,4),Nothing),((0,5),Nothing),((0,6),Nothing),((0,7),Nothing),((1,0),Nothing),((1,1),Nothing),((1,2),Nothing),((1,3),Nothing),((1,4),Nothing),((1,5),Nothing),((1,6),Nothing),((1,7),Nothing),((2,0),Nothing),((2,1),Nothing),((2,2),Nothing),((2,3),Nothing),((2,4),Nothing),((2,5),Nothing),((2,6),Nothing),((2,7),Nothing),((3,0),Nothing),((3,1),Nothing),((3,2),Nothing),((3,3),Just Black),((3,4),Just Black),((3,5),Just Black),((3,6),Nothing),((3,7),Nothing),((4,0),Nothing),((4,1),Nothing),((4,2),Nothing),((4,3),Just White),((4,4),Just Black),((4,5),Nothing),((4,6),Nothing),((4,7),Nothing),((5,0),Nothing),((5,1),Nothing),((5,2),Nothing),((5,3),Nothing),((5,4),Nothing),((5,5),Nothing),((5,6),Nothing),((5,7),Nothing),((6,0),Nothing),((6,1),Nothing),((6,2),Nothing),((6,3),Nothing),((6,4),Nothing),((6,5),Nothing),((6,6),Nothing),((6,7),Nothing),((7,0),Nothing),((7,1),Nothing),((7,2),Nothing),((7,3),Nothing),((7,4),Nothing),((7,5),Nothing),((7,6),Nothing),((7,7),Nothing)]

-- | Returns a list of all positions in an Othello board
positions :: [Pos]
positions = [ (y, x) | y <- [0 .. 7], x <- [0 .. 7] ]

-- ################################### Othello functions ###################################

-- | Makes a move and returns the updated board, returns unchanged board if move is not valid
makeMove :: Pos -> Player ->  Board -> Board
makeMove pos player b = if isValid player b pos 
                        then flipAll positions b player
                        else b
      where positions = pos:flipped player b pos -- the chosen position and the flipped positions

-- | Adds a player's disk on all the given positions, returns the new board
flipAll :: [Pos] -> Board -> Player -> Board
flipAll [] b _            = b 
flipAll ps b (Player d _) = flip ps b
 where flip [] b     = b
       flip (p:ps) b = flip ps (M.insert p (Just d) b) 
  
-- | Returns a list of positions which have no disk on them
emptyPositions :: Board -> [Pos]
emptyPositions = M.keys . M.filter isNothing

-- | Returns all the possible moves a player can make on a board together with the pieces that would be flipped 
--   if move would be made
possibleMoves :: Player -> Board -> [(Pos, [Pos])]
possibleMoves pl b =
  filter wouldFlip . map (\p -> (p, flipped pl b p)) $ emptyPositions b
 where
  wouldFlip (_, []) = False
  wouldFlip _       = True

-- | Returns all positions that would be flipped if player adds disk to the given position on the board
flipped :: Player -> Board -> Pos -> [Pos]
flipped (Player d _) b p = concat [ flippedDir d p b dir | dir <- [North ..] ]

-- | Returns a list of positions in a given direction that would be flipped if a disk would be added to the 
--   specified position on a board
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

-- | All directions of the othello board
data Dir = North
         | NorthEast
         | East
         | SouthEast
         | South
         | SouthWest
         | West
         | NorthWest deriving (Enum, Show)


-- | Defines a step in a given direction (in grid)
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

-- | Check if the player can add a disk to the position according to Othello rules
isValid :: Player -> Board -> Pos -> Bool
isValid pl b p = not . null $ flipped pl b p

-- | Game is over when whole board is filled with disks or none of the players can play
isGameOver :: Player -> Player -> Board -> Bool
isGameOver p1 p2 b = noPlays || fullBoard b
      where noPlays = (not $ canPlay p1 b) && (not $ canPlay p2 b)

-- Checks if player is the winner of the board
winner :: Player -> Board -> Bool
winner (Player disk _) board = playerPoint >= oppPoint
      where playerPoint = countColor disk board 
            oppPoint = countColor (opponentDisk disk) board

-- | Returns true if all positions on board has a disk
fullBoard :: Board -> Bool
fullBoard b = (bPoints + wPoints) == 64
      where (bPoints,wPoints) = updatePoints b

-- | Returns true if player can make a move on the board 
canPlay :: Player -> Board -> Bool
canPlay p b = not $ null $ possibleMoves p b

-- | Returns a tuple of the number of black and white disks on the board
updatePoints :: Board -> (Int,Int)
updatePoints b = (countColor Black b, countColor White b)

-- | Returns the number of occurences of the disk on the board
countColor :: Disk -> Board -> Int 
countColor d = length . filter equalsDisk . map snd . M.toList
      where equalsDisk d'= d' == Just d

-- ################################### AI ###################################

-- | Calculates the best move for the player using the minimax algorithm
getPositionAI :: Board -> Player -> Pos
getPositionAI b player = snd $ M.findMax $ M.fromList minmaxresult  
    where playerCol = getPlayerCol player
          allValidMoves = map fst (possibleMoves player b)
          minmaxresult = map (\position -> (minimax 4 player (makeMove position player b),position)) allValidMoves
          -- minmaxresult returns [(minmax value, position)]


-- | Evaluates how good a play is given the board where the play has been made 
minimax :: Int -> Player -> Board -> Int
minimax depth (Player disk _) board 
    | isGameOver (Player disk 0) (Player (opponentDisk disk) 0) board = if winner (Player disk 0) board then 10000 else -10000
    | depth <= 0 = heuristic (Player disk 0) board
    | otherwise = if nextColor /= disk       
                  then - maxPlayNextColor       -- minimizing
                  else  maxPlayNextColor        -- maximizing 
    where 
    nextColor = if canPlay (Player (opponentDisk disk) 0) board then (opponentDisk disk) else disk
    nextPlayer = Player nextColor 0
    nextPlayerPossibleMoves = map fst $ possibleMoves nextPlayer board 
    maxPlayNextColor = maximum (map (\pos -> minimax (depth-1) nextPlayer (makeMove pos nextPlayer board)) nextPlayerPossibleMoves)

-- | Computes the heuristic which is the player's score substracted with the opponent's score (can be improved to be smarter)
heuristic :: Player -> Board -> Int
heuristic (Player d p) b | d == Black = bScore - wScore
                         | otherwise  = wScore - bScore
      where (bScore,wScore) = updatePoints b

-- ################################### Tests ###################################

instance Arbitrary Disk where
      arbitrary = elements [Black, White]

instance Arbitrary Player where
      arbitrary = elements [(Player Black 0), (Player White 0)]

instance Arbitrary Dir where 
      arbitrary = elements [North ..]

-- | Tests that AI chooses positions that are possible moves
prop_getpositionAI_legal :: Board -> Player -> Bool
prop_getpositionAI_legal b p = if noPossibleMoves 
                              then True 
                              else isIn (getPositionAI b p) (map fst (possibleMoves p b))
      where
      isIn :: Pos -> [Pos] -> Bool
      isIn _ [] = False
      isIn aiPos (p:ps) = if aiPos == p then True else isIn aiPos ps
      noPossibleMoves = (length $ possibleMoves p b) == 0

-- | Checks that range of values heuristic can give is valid
prop_heuristics :: Player -> Board -> Bool
prop_heuristics p b = heuristic p b <= 64 && heuristic p b >= -64

-- | Checks that heuristic gives positive value if the player has more disks on the board then the opponent
prop_heuristics_color :: Player -> Board -> Bool
prop_heuristics_color (Player d _) b 
            | playerDisks >= opponentDisks = heuristic (Player d 0) b >= 0
            | otherwise                    = heuristic (Player d 0) b < 0
            where 
            playerDisks = countColor d b
            opponentDisks = countColor (opponentDisk d) b

-- | Checks that makeMove increases the players points if it is a valid move
prop_makeMove_points :: Pos -> Player -> Board -> Bool
prop_makeMove_points pos pl b = if possibleMove 
                                then pointsAfterMove >= pointsBeforeMove
                                else pointsAfterMove == pointsBeforeMove
      where newBoard = makeMove pos pl b
            pointsAfterMove = countColor (disk pl) newBoard
            pointsBeforeMove = countColor (disk pl) b
            possibleMove = pos `elem` (concat $ map snd $ possibleMoves pl b)

-- | Check that flippedDir only returns at max 6 positions 
prop_flippedDir_amount :: Disk -> Pos -> Board -> Dir -> Bool
prop_flippedDir_amount d pos b dir = (length $ flippedDir d pos b dir) <= 6 

-- | Check that flipped only returns already occupied positions and not empty positions
prop_flipped_occupiedPos :: Player -> Board -> Pos -> Bool
prop_flipped_occupiedPos pl b pos = emptyBefore == emptyAfter
                  where emptyBefore = length $ emptyPositions b
                        emptyAfter = length $ emptyPositions $ flipAll (flipped pl b pos) b pl

-- | PossibleMoves correct for startingBoard 
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

-- ################################### PRINTING ###################################

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

