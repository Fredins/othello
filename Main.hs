{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

module Main where

-- import qualified Data.Text as T
import           Data.Text                      ()
import           Data.Int
import           Data.Maybe
import           Data.GI.Base
import qualified GI.Gtk                        as G
import Control.Monad (replicateM_)

main = do
  G.init Nothing

  builder <- G.builderNew
  G.builderAddFromFile builder "othello.glade"

  win <- G.builderGetObject builder "window" >>= unsafeCastTo G.Window . fromJust
  on win #destroy G.mainQuit
  grid <- G.builderGetObject builder "grid" >>= unsafeCastTo G.Grid . fromJust

-- lär göra "mapM add othelloMap" senare
  let add 8 _ = return ()
      add r c = do
      i <- G.imageNewFromIconName (Just "start-here") 4
      b <- new G.Button [#image := i]
      G.gridAttach grid b r c 1 1 
      add r' c'
        where (r', c') | c < 7     = (r, c + 1)
                       | otherwise = (r+1, 0)
    
  add 0 0 
  

  #showAll win
  G.main
  

          
