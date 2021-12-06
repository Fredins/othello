{-# LANGUAGE OverloadedStrings, OverloadedLabels, ScopedTypeVariables #-}

module Main where

import           Control.Exception
import           Control.Monad                  ( replicateM_ )
import           Data.GI.Base
import           Data.IORef
import           Data.Int
import           Data.Map                      as M
import           Data.Maybe
import           Data.Text
import qualified GI.Gtk                        as G
import           GI.Gtk.Objects.Button          ( toButton )
import           Othello
import           Debug.Trace

data State = State
  { player0 :: Player
  , player1 :: Player
  , activeP :: Player
  }

initGrid :: IORef State -> G.Builder -> IO G.Grid
initGrid refS b = do
  g <- G.builderGetObject b "grid" >>= unsafeCastTo G.Grid . fromJust

  let addBtn ((y, x), v) = do
        i <- G.imageNewFromFile $ iconPath v
        b <- new G.Button [#image := i]
        onClick refS b
        G.gridAttach g b (fromIntegral y) (fromIntegral x) 1 1

  mapM_ addBtn $ M.toList startingBoard
  return g


iconPath :: Maybe Disk -> String
iconPath v = case v of
  (Just White) -> "white.png"
  (Just Black) -> "black.png"
  Nothing      -> "blank.png"


gridMapM_ :: (G.Button -> IO a) -> G.Grid -> IO ()
gridMapM_ f g = do
  mapM_
    (\(y, x) -> do
      b <-
        G.gridGetChildAt g (fromIntegral y) (fromIntegral x)
        >>= unsafeCastTo G.Button
        .   fromJust
      f b
    )
    positions


switchActivePlayer :: IORef State -> IO State 
switchActivePlayer refS = do
  State { activeP = ap, player0 = p0, player1 = p1 } <- readIORef refS
  let ap' = if ap == p0 then p1 else p0
  let s = State { activeP = ap', player0 = p0, player1 = p1 }
  writeIORef refS s
  return s

onClick :: IORef State -> G.Button -> IO ()
onClick refS b = do
  State { activeP = (Player d n), player0 = _, player1 = _ } <- readIORef refS
  putStrLn ("ONCLICK active color: " ++ show d)
  i <- G.imageNewFromFile $ iconPath (Just d)
  on b #clicked $ set b [#image := i]
  switchActivePlayer refS
  return ()

initWin :: G.Builder -> IO G.Window
initWin b = do
  win <- G.builderGetObject b "window" >>= unsafeCastTo G.Window . fromJust
  on win #destroy G.mainQuit
  return win

initState :: IO (IORef State)
initState = do
  let p0 = Player White 0
  let p1 = Player Black 0
  newIORef State { activeP = p0, player0 = p0, player1 = p1 }

main =
  do
    G.init Nothing
    b <- G.builderNew
    G.builderAddFromFile b "othello.glade"

    refS <- initState
    win  <- initWin b
    grid <- initGrid refS b

    #showAll win
    G.main
  `catch` (\(e :: GError) -> gerrorMessage e >>= putStrLn . unpack)
