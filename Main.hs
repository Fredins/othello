{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

{-
-- {-# LANGUAGE KindSignatures #-}
-- {-# LANGUAGE PolyKinds      #-}
-- {-# LANGUAGE RankNTypes     #-}
class IxMonadIO (m :: k -> k -> * -> *) where
  iliftIO :: forall (i :: k) a. IO a -> m i i a
-}

module Main where
import           Control.Monad
import qualified Data.Map                      as M
import           Data.Maybe
import           Data.Text                      ( pack )
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

data State = State
  { activeP   :: Player
  , player0   :: Player
  , player1   :: Player
  , board     :: Board
  , resources :: Resources
  }


data Event = Close
           | DiskClicked (Pos, Maybe Disk)

update' :: State -> Event -> Transition State Event
update' s@State {..} e = case e of
  Close              -> Exit
  DiskClicked (p, d) -> Transition s{activeP = activeP', board = board'} (return Nothing)
    where ps = flipped activeP board p
          board' | null ps   = trace ("wrong position! you are player " ++ show activeP) board
                 | otherwise = flipAll (p:ps) board activeP 
          activeP' | null ps            = activeP 
                   | activeP == player0 = player1
                   | otherwise          = player0


 -- Transition s (return Nothing)
-- update board and update images (via createImages)



view' :: State -> AppView G.Window Event
view' s = bin G.Window [#title := "Othello", on #destroy Close] $ grid s

grid :: State -> Widget Event
grid s@State {..} = container G.Grid [] cs
 where
  cs = fromList $ map c $ M.toList board
  c ((y, x), d) = GridChild
    defaultGridChildProperties { leftAttach = fromIntegral x
                               , topAttach  = fromIntegral y
                               }
    (widget
      G.Button
      [#image := image ((y, x), d), on #clicked $ DiskClicked ((y, x), d)]
    )
  image :: (Pos, Maybe Disk) -> G.Image
  image (p, d) = fromJust $ M.lookup p $ case d of
    (Just White) -> white resources
    (Just Black) -> black resources
    Nothing      -> empty resources




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
  
  return State { activeP   = p0
               , player0   = p0
               , player1   = p1
               , board     = startingBoard
               , resources = res
               }
 where
  p0 = Player White 0
  p1 = Player Black 0


main :: IO ()
main = do
  G.init Nothing
  state <- initState

  void $ run App { view         = view'
                 , update       = update'
                 , inputs       = []
                 , initialState = state
                 }
  G.main
