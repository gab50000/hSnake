module Main where

import Brick
import Brick.Widgets.Border
import qualified Graphics.Vty as V
import Brick.BChan (newBChan, writeBChan)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void)

data Tick = Tick

data AppState n = AppState {text :: String}


transform :: AppState () -> BrickEvent () e -> EventM () (Next (AppState ()))
transform p (VtyEvent e) = case e of
    V.EvKey (V.KChar 'q') [] -> halt p 
    V.EvKey (V.KChar char) [] -> continue $ p { text = [char]}
    _ -> continue p
transform p _ = continue p


drawUI :: AppState () -> [Widget ()]
drawUI = return . str . text


initApp :: App (AppState ()) e ()
initApp = App {
    appDraw = drawUI,
    appChooseCursor = showFirstCursor,
    appHandleEvent = transform,
    appStartEvent = return,
    appAttrMap = const $ attrMap V.defAttr []
}

main = void $ defaultMain initApp (AppState "hello")

