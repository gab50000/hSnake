module Main where

import Brick
import Brick.Widgets.Border

ui :: Widget ()
ui = vBox [hBox [str "X" | i <- [1..width]] | i <- [1..height]] 
    where
        height = 15
        width = 30

main :: IO ()
main = simpleMain ui