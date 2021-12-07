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

data State = State
  { activeP :: Player
  , player0 :: Player
  , player1 :: Player
  , board   :: Board
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
    $ widget G.Button [#label := "world", on #clicked DiskClicked]


grid :: State -> Widget Event
grid s = container G.Grid [] cs
 where
  cs' = [c ((1, 1), Nothing), c ((1, 1), Nothing)]
  cs :: [GridChild Event]
  cs = map c $ toList $ board s
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
main = void $ run App { view         = view'
                      , update       = update'
                      , inputs       = []
                      , initialState = initState
                      }
