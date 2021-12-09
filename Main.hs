{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Main where
import           Control.Monad
import qualified Data.Map                      as M
import           Data.Maybe
import           Data.Text                      ( pack )
import           Data.Tuple.Extra               ( both )
import           Data.Vector                    ( Vector
                                                , fromList
                                                )
import           Debug.Trace
import qualified GI.Gtk                        as G
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple
import           GI.Gtk.Declarative.Container.Grid
import           Othello


type Images = M.Map Pos G.Image

data Resources = Resources
  { empty :: Images
  , white :: Images
  , black :: Images
  }

data Screen = StartMenu
          | Play
          | GameOver

data State = State
  { activeP   :: Player
  , playerB   :: Player
  , playerW   :: Player
  , board     :: Board
  , screen    :: Screen
  , resources :: Resources
  }


data Event = Close
           | DiskClicked (Pos, Maybe Disk)

update' :: State -> Event -> Transition State Event
update' s@State {..} e = case e of
  Close              -> Exit

  DiskClicked (p, d) -> Transition
    (if null ps || isJust d
      then s
      else s { activeP = aP
             , playerB = pB
             , playerW = pW
             , board   = b
             , screen  = if gameOver pB pW b then GameOver else Play
             }
    )
    $ return Nothing
   where
    ps       = flipped activeP board p
    b        = flipAll (p : ps) board activeP
    (pB, pW) = both (\d -> Player d $ countColor d b) (Black, White)
    aP | activeP == playerB && canPlay pW b && not (null ps) = pW
       | otherwise = pB

-- TODO add case screen, and add points + whose turn + buttons (restart, exit, show moves)
view' :: State -> AppView G.Window Event
view' s = bin G.Window [#title := "Othello", on #destroy Close] $ grid s

grid :: State -> Widget Event
grid s@State {..} = container G.Grid [] cs
 where
  cs = fromList $ map c $ M.toList board
  c pd@((y, x), d) = GridChild
    defaultGridChildProperties { leftAttach = fromIntegral x
                               , topAttach  = fromIntegral y
                               }
    (widget G.Button [#image := image pd, on #clicked $ DiskClicked pd])
  image :: (Pos, Maybe Disk) -> G.Image
  image (p, d) = fromJust $ M.lookup p $ case d of
    (Just White) -> white resources
    (Just Black) -> black resources
    Nothing      -> empty resources



-- TODO add highlight res
createResources :: IO Resources
createResources = do
  e <- mapM (f "gui/empty.png") positions
  w <- mapM (f "gui/white.png") positions
  b <- mapM (f "gui/black.png") positions
  return Resources { empty = M.fromList e
                   , white = M.fromList w
                   , black = M.fromList b
                   }
 where
  f :: String -> Pos -> IO (Pos, G.Image)
  f s p = (p, ) <$> G.imageNewFromFile s


initState :: IO State
initState = do
  res <- createResources

  return State { activeP   = pB
               , playerB   = pB
               , playerW   = pW
               , board     = startingBoard
               , screen    = StartMenu
               , resources = res
               }
 where
  pB = Player Black 0
  pW = Player White 0


main :: IO ()
main = do
  G.init Nothing
  state <- initState
  -- TODO maybe async?
  void $ run App { view         = view'
                 , update       = update'
                 , inputs       = []
                 , initialState = state
                 }
  G.main
