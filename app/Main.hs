module Main where

import Brick
import Brick.BChan (newBChan, readBChan, writeBChan)
import Brick.Widgets.Border
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void)
import qualified Graphics.Vty as V

data Tick = Tick

newtype AppState n = AppState {snakePos :: (Int, Int)}

drawUI :: AppState () -> Widget ()
drawUI state = vBox [hBox [drawChar (i, j) | i <- [1 .. width]] | j <- [1 .. height]]
  where
    height = 15
    width = 15
    drawChar pos =
        if pos == snakePos state
            then str " X "
            else str "   "

handleEvent :: AppState () -> BrickEvent () e -> EventM () (Next (AppState ()))
handleEvent p@(AppState (i, j)) (VtyEvent e) = case e of
    V.EvKey (V.KChar 'q') [] -> halt p
    V.EvKey V.KRight [] -> continue $ p{snakePos = (i + 1, j)}
    V.EvKey V.KLeft [] -> continue $ p{snakePos = (i - 1, j)}
    V.EvKey V.KUp [] -> continue $ p{snakePos = (i, j - 1)}
    V.EvKey V.KDown [] -> continue $ p{snakePos = (i, j + 1)}
    _ -> continue p
handleEvent p _ = continue p

initApp :: App (AppState ()) e ()
initApp =
    App
        { appDraw = return . drawUI
        , appChooseCursor = showFirstCursor
        , appHandleEvent = handleEvent
        , appStartEvent = return
        , appAttrMap = const $ attrMap V.defAttr []
        }

main1 :: IO ()
main1 = void $ defaultMain initApp (AppState (0, 0))

main2 :: IO ()
main2 = do
    chan <- newBChan 10
    forkIO $
        forever $ do
            writeBChan chan Tick
            threadDelay 1000000
    forever $ do
        elem <- readBChan chan
        putStrLn "hi"

-- void $ customMain (V.mkVty V.defaultConfig) (Just chan) (App s e n) s

main = main2
