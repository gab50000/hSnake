module Main where

import Brick
import Brick.BChan (newBChan, readBChan, writeBChan)
import Brick.Widgets.Border
import Control.Concurrent (MVar, forkIO, putMVar, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar)
import Control.Monad (forever, void)
import qualified Graphics.Vty as V
import qualified Snake as AppState.Snk
import qualified Snake as Snk

data Tick = Tick

newtype AppState = AppState {game :: Snk.Game}

drawUI :: AppState -> Widget ()
drawUI state = vBox [hBox [drawChar (i, height - j) | i <- [1 .. width]] | j <- [1 .. height]]
  where
    AppState game = state
    Snk.Game (Snk.SnakePos (ii, jj)) dim _ = game
    Snk.Dimensions (width, height) = dim
    drawChar pos =
        if pos == (ii, jj)
            then str " X "
            else str "   "

handleEvent :: AppState -> BrickEvent () Tick -> EventM () (Next AppState)
handleEvent p@(AppState game) (AppEvent Tick) = continue $ AppState (Snk.advance game)
handleEvent p@(AppState game) (VtyEvent e) = case e of
    V.EvKey (V.KChar 'q') [] -> halt p
    V.EvKey V.KRight [] -> continue $ AppState (Snk.setDir game Snk.SRight)
    V.EvKey V.KLeft [] -> continue $ AppState (Snk.setDir game Snk.SLeft)
    V.EvKey V.KUp [] -> continue $ AppState (Snk.setDir game Snk.SUp)
    V.EvKey V.KDown [] -> continue $ AppState (Snk.setDir game Snk.SDown)
    _ -> continue p
handleEvent p _ = continue p

initApp :: App AppState Tick ()
initApp =
    App
        { appDraw = return . drawUI
        , appChooseCursor = showFirstCursor
        , appHandleEvent = handleEvent
        , appStartEvent = return
        , appAttrMap = const $ attrMap V.defAttr []
        }

main = do
    chan <- newBChan 10
    m <- newEmptyMVar
    forkIO $
        forever $ do
            writeBChan chan Tick
            threadDelay 500000
    let buildVty = V.mkVty V.defaultConfig
    initialVty <- buildVty
    void $ customMain initialVty buildVty (Just chan) initApp (AppState Snk.newGame)
