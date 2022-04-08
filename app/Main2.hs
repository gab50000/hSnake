module Main where

import Brick
import Brick.BChan (
    newBChan,
    writeBChan,
 )
import Brick.Widgets.Border
import Control.Concurrent (
    MVar,
    ThreadId,
    forkIO,
    newEmptyMVar,
    putMVar,
    takeMVar,
    threadDelay,
 )
import Control.Monad (
    forever,
    void,
 )
import Data.Traversable (forM)
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

printAndWait :: Int -> IO ()
printAndWait x = print x >> threadDelay 1000000

fillMVar :: MVar Integer -> IO ThreadId
fillMVar m = forkIO $ forever $ mapM_ (\i -> putMVar m i >> threadDelay 500000) [1 .. 10]

drainMVar :: MVar Integer -> IO ()
drainMVar m = forever $ takeMVar m >>= print

setRefreshRate :: Int -> IO (MVar Integer)
setRefreshRate rate = do
    m <- newEmptyMVar
    forkIO $
        forever $ do
            forM [1 .. 10] $ \i -> do
                putMVar m i
                threadDelay rate
    return m

main = do
    m <- setRefreshRate 100000
    drainMVar m
