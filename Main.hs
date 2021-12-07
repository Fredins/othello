{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Main where
import           Control.Monad
import           Data.Map                       ( toList )
import           Data.Text                      ( pack )
import qualified GI.Gtk                        as G
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple
import           GI.Gtk.Declarative.Container.Grid
import           Othello
import           Data.Vector (Vector, fromList)

data State = State
  { activeP :: Player
  , player0 :: Player
  , player1 :: Player
  , board   :: Board
  }

data Images = Images
      { blank :: G.Image
      , white :: G.Image
      , black :: G.Image
      }

data Event = Close
           | DiskClicked

update' :: State -> Event -> Transition State Event
update' s e = case e of
  Close       -> Exit
  DiskClicked -> Transition s (return Nothing)


view' :: State -> AppView G.Window Event
view' s =
  bin G.Window [#title := "hello", on #deleteEvent (const (True, Close))]
    $ w 


grid :: State -> Images  -> Widget Event
grid s i = container G.Grid [] cs
 where
  cs :: Vector (GridChild Event)
  cs = fromList $ map c $ toList $ board s
  c :: (Pos, Maybe Disk) -> GridChild Event
  c ((y, x), _) = GridChild
    defaultGridChildProperties { leftAttach = fromIntegral x
                               , topAttach  = fromIntegral y
                               }
    (widget G.Button [#label := "hello"])

initState = State { activeP = p0
                  , player0 = p0
                  , player1 = p0
                  , board   = startingBoard
                  }
 where
  p0 = Player White 0
  p1 = Player Black 0


main :: IO ()
main = do 
  i <-  G.imageNewFromFile "blank.png"
  iw <- G.imageNewFromFile "white.png"
  ib <- G.imageNewFromFile "black.png"
  void $ run App { view         = view' Images{i, iw, ib}
                 , update       = update'
                 , inputs       = []
                 , initialState = initState
                 }
