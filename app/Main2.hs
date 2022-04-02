module Main where

import Brick
import Brick.BChan (
    newBChan,
    writeBChan,
 )
import Brick.Widgets.Border
import Control.Concurrent (
    forkIO,
    threadDelay,
 )
import Control.Monad (
    forever,
    void,
 )
import qualified Graphics.Vty as V

data Tick = Tick

newtype AppState n = AppState {text :: String}

handleEvent :: AppState () -> BrickEvent () e -> EventM () (Next (AppState ()))
handleEvent p (VtyEvent e) = case e of
    V.EvKey (V.KChar 'q') [] -> halt p
    V.EvKey (V.KChar char) [] -> continue $ p{text = [char]}
    _ -> continue p
handleEvent p _ = continue p

drawUI :: AppState () -> [Widget ()]
drawUI = return . str . text

initApp :: App (AppState ()) e ()
initApp =
    App
        { appDraw = drawUI
        , appChooseCursor = showFirstCursor
        , appHandleEvent = handleEvent
        , appStartEvent = return
        , appAttrMap = const $ attrMap V.defAttr []
        }

main = void $ defaultMain initApp (AppState "hello")
