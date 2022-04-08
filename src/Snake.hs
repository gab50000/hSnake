module Snake where

data Direction = SLeft | SRight | SUp | SDown

data Game = Game SnakePos Dimensions Direction

newtype SnakePos = SnakePos (Int, Int)

newtype Dimensions = Dimensions (Int, Int)

setDir :: Game -> Direction -> Game
setDir (Game pos dim _) = Game pos dim

newGame :: Game
newGame = Game (SnakePos (1, 1)) (Dimensions (10, 10)) SRight

advance :: Game -> Game
advance game@(Game (SnakePos (i, j)) dim dir) = Game newPos dim dir
  where
    newPos = case dir of
        SLeft -> SnakePos (i - 1, j)
        SRight -> SnakePos (i + 1, j)
        SUp -> SnakePos (i, j + 1)
        SDown -> SnakePos (i, j - 1)