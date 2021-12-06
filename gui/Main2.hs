{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad
import qualified GI.Gtk                        as G
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple

type State = ()

data Event = Close

view' :: State -> AppView G.Window Event
view' _ =
  bin G.Window [#title := "Hello World", on #deleteEvent (const (True, Close))]
    $ widget G.Label [#label := "Hello World!"]


update' :: State -> Event -> Transition State Event
update' _ Close = Exit

main :: IO ()
main = void
  $ run App { view = view', update = update', inputs = [], initialState = () }
