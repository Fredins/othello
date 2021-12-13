{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module Main where
import           Control.Concurrent.Async       ( async )
import           Control.Monad
import qualified Data.Map                      as M
import           Data.Maybe
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Data.Tuple.Extra               ( both )
import           Data.Vector                    ( fromList )
import           GI.Gdk.Objects                 ( screenGetDefault )
import           GI.Gio.Interfaces.File
import qualified GI.Gtk                        as G
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple
import           GI.Gtk.Declarative.Container.Grid
import           Othello                 hiding ( makeMove )


data Screen = StartMenu
            | Play
            | GameOver

data Mode = Pvp | Pve deriving (Eq, Show)

-- State of the application which the view is based of the state
data State = State
  { activeP :: Player
  , playerB :: Player
  , playerW :: Player
  , board   :: Board
  , screen  :: Screen
  , mode    :: Mode
  }

-- Possible events 
data Event = Close
           | DiskClicked (Pos, Maybe Disk)
           | Start Mode
           | ComputerMove


-- UPDATE #####################################################################

-- Handle events and modify the state. Called on event trigger
update' :: State -> Event -> Transition State Event
update' s@State {..} e = case e of
  Close   -> Exit
  Start m -> Transition s { screen = Play, mode = m } $ return Nothing

  DiskClicked (p, d)
    | mode == Pve && activeP /= activeP' -> Transition s'
    $  return (Just ComputerMove)
    | otherwise -> Transition s' $ return Nothing
    where s'@State { activeP = activeP' } = makeMove s p d

  ComputerMove -> Transition (makeMove s p d) $ return Nothing
   where
    d = fromJust $ M.lookup p board
    p = getPositionAI board activeP


-- Update state according to move with position p and disk d
makeMove :: State -> Pos -> Maybe Disk -> State
makeMove s@State {..} p d
  | (null ps && length (emptyPositions board) /= 1) || isJust d = s
  | otherwise = s { activeP = aP
                  , playerB = pB
                  , playerW = pW
                  , board   = b
                  , screen  = if isGameOver pB pW b then GameOver else Play
                  }
 where
  ps       = flipped activeP board p
  b        = flipAll (p : ps) board activeP
  (pB, pW) = both (\d -> Player d $ countColor d b) (Black, White)
  aP | null ps              = nextPlayer
     | canPlay nextPlayer b = nextPlayer
     | otherwise            = activeP
  nextPlayer = if activeP == playerW then pB else pW


-- VIEW ########################################################################

-- Creates a view according to the state reactively.
view' :: State -> AppView G.Window Event
view' s@State {..} =
  bin G.Window [#title := "Othello", on #destroy Close] $ case screen of

    StartMenu -> startMenu

    Play      -> container
      G.Box
      [classes ["play"], #orientation := G.OrientationVertical]
      [ BoxChild defaultBoxChildProperties $ header s
      , BoxChild defaultBoxChildProperties $ grid s
      ]

    GameOver -> gameOver s


-- start menu view
startMenu :: Widget Event
startMenu = container
  G.Box
  [ classes ["start_menu"]
  , #halign := G.AlignCenter
  , #valign := G.AlignCenter
  , #orientation := G.OrientationVertical
  ]
  [ BoxChild defaultBoxChildProperties { padding = 50 }
    $ widget G.Label [classes ["h1"], #label := "Othello"]
  , BoxChild defaultBoxChildProperties { padding = 2 }
    $ widget G.Button [#label := "Player vs Player", on #clicked $ Start Pvp]
  , BoxChild defaultBoxChildProperties { padding = 2 }
    $ widget G.Button [#label := "Player vs Computer", on #clicked $ Start Pve]
  ]


-- header view
header :: State -> Widget Event
header State {..} = container
  G.Box
  [ classes ["header"]
  , #orientation := G.OrientationHorizontal
  , #halign := G.AlignCenter
  ]
  [ container
    G.Box
    [#orientation := G.OrientationHorizontal]
    [ BoxChild defaultBoxChildProperties { padding = 10 }
      $ widget G.Image [#file := "gui/white1.png"]
    , widget G.Label [#label := pack (show $ points playerW)]
    ]
  , BoxChild defaultBoxChildProperties { padding = 100 } $ widget
    G.Label
    [ classes ["header_center"]
    , #label := pack (show (disk activeP) ++ "'s turn")
    ]
  , container
    G.Box
    [#orientation := G.OrientationHorizontal]
    [ BoxChild defaultBoxChildProperties { padding = 10 }
      $ widget G.Image [#file := "gui/black1.png"]
    , widget G.Label [#label := pack (show $ points playerB)]
    ]
  ]

-- grid view
grid :: State -> Widget Event
grid State {..} = container G.Grid [classes ["grid"]] cs
 where
  cs = fromList $ map c $ M.toList board
  c pd@((y, x), _) = GridChild
    defaultGridChildProperties { leftAttach = fromIntegral x
                               , topAttach  = fromIntegral y
                               }
    ( bin G.Button [on #clicked $ DiskClicked pd]
    $ widget G.Image [#file := path pd]
    )

  path :: (Pos, Maybe Disk) -> Text
  path (_, Just White) = "gui/white.png"
  path (_, Just Black) = "gui/black.png"
  path (p, _)
    | p `elem` (fst <$> possibleMoves activeP board) = "gui/highlight.png"
    | otherwise = "gui/empty.png"

-- game over view
gameOver :: State -> Widget Event
gameOver State {..} = container
  G.Box
  [classes ["game_over"], #orientation := G.OrientationVertical]
  [ BoxChild defaultBoxChildProperties $ container
    G.Box
    [ classes ["header"]
    , #orientation := G.OrientationHorizontal
    , #halign := G.AlignCenter
    ]
    [ container
      G.Box
      [#orientation := G.OrientationHorizontal]
      [ BoxChild defaultBoxChildProperties { padding = 10 }
        $ widget G.Image [#file := "gui/white1.png"]
      , widget G.Label [#label := pack (show $ points playerW)]
      ]
    , BoxChild defaultBoxChildProperties { padding = 100 }
      $ widget G.Label [classes ["header_center"], #label := pack winner]
    , container
      G.Box
      [#orientation := G.OrientationHorizontal]
      [ BoxChild defaultBoxChildProperties { padding = 10 }
        $ widget G.Image [#file := "gui/black1.png"]
      , widget G.Label [#label := pack (show $ points playerB)]
      ]
    ]
  , BoxChild defaultBoxChildProperties $ container G.Grid [classes ["grid"]] cs
  ]
 where
  cs = fromList $ map c $ M.toList board
  c pd@((y, x), _) = GridChild
    defaultGridChildProperties { leftAttach = fromIntegral x
                               , topAttach  = fromIntegral y
                               }
    (bin G.Button [] $ widget G.Image [#file := path pd])
  path :: (Pos, Maybe Disk) -> Text
  path (_, Just White) = "gui/white.png"
  path (_, Just Black) = "gui/black.png"
  path (p, _)
    | p `elem` (fst <$> possibleMoves activeP board) = "gui/highlight.png"
    | otherwise = "gui/empty.png"
  (pb, pw) = updatePoints board
  winner
    | pb > pw
    = "Black wins with " ++ show pb ++ " vs " ++ show pw ++ " points"
    | pw > pb
    = "White wins with " ++ show pw ++ " vs " ++ show pb ++ " points"
    | otherwise
    = "Draw!"

-- MAIN #######################################################################

-- loads css and starts the program loop with an intial state
main :: IO ()
main = do
  void $ G.init Nothing

  s <- maybe (fail "No screen?!") return =<< screenGetDefault
  p <- G.cssProviderNew
  f <- fileNewForPath "gui/styles.css"
  G.cssProviderLoadFromFile p f
  G.styleContextAddProviderForScreen
    s
    p
    (fromIntegral G.STYLE_PROVIDER_PRIORITY_USER)

  void . async $ do
    void $ runLoop app
    G.mainQuit
  G.main
 where
  app = App
    { view         = view'
    , update       = update'
    , inputs       = []
    , initialState = State { activeP = pB
                           , playerB = pB
                           , playerW = pW
                           , board   = startingBoard
                           , screen  = StartMenu
                           , mode    = error "No mode selected"
                           }
    }
  pB = Player Black 0
  pW = Player White 0
