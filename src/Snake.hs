module Snake (
    LogicCoord,
    Direction (..),
    SnakePosition (..),
    ImpossibleMove (..),
    GameState (..),
    mkLogicCoord,
    mkRectangle,
    nextHeadPosition,
    moveSnake
) where

import System.IO

data Direction = Up
               | Down
               | Left
               | Right
               deriving (Show, Eq)

data ImpossibleMove = CellTaken 
                    | BoundaryHit LogicCoord
                    deriving (Show, Eq)

data Coord = Coord {
    x :: Int,
    y :: Int
} deriving (Show, Eq)

data Rectangle = Rectangle {
    topLeft :: Coord,
    bottomRight :: Coord
} deriving (Show, Eq)

mkRectangle :: (Int, Int) -> (Int, Int) -> Rectangle
mkRectangle (topLeftX, topLeftY) (bottomRightX, bottomRightY) =
    Rectangle 
        (Coord topLeftX topLeftY)
        (Coord bottomRightX bottomRightY)

newtype LogicCoord = LogicCoord {
    coord :: Coord
} deriving (Show, Eq)

mkLogicCoord :: Int -> Int -> LogicCoord
mkLogicCoord x y = LogicCoord $ Coord x y

unLogicCoord :: LogicCoord -> Coord
unLogicCoord (LogicCoord coord) = coord

withinBoundary :: Rectangle -> Coord -> Bool
withinBoundary (Rectangle (Coord topLeftX topLeftY) (Coord bottomRightX bottomRightY)) 
               (Coord x y) =
    x > topLeftX 
    && x < bottomRightX
    && y > topLeftY 
    && y < bottomRightY

newtype SnakePosition = SnakePosition [LogicCoord]
    deriving (Show, Eq)

unSnakePosition :: SnakePosition -> [LogicCoord]
unSnakePosition (SnakePosition coords) = coords

data GameState = GameState {
    gameDimensions :: Rectangle,
    snakePosition :: SnakePosition,
    headDirection :: Direction,
    foodPositions :: [LogicCoord],
    ticksPassed :: Int
} deriving (Show, Eq)

instance Semigroup Coord where
  Coord x_left y_left <> Coord x_right y_right = Coord (x_left + x_right) (y_left + y_right)

instance Semigroup LogicCoord where
    LogicCoord coord_left <> LogicCoord coord_right = LogicCoord (coord_left <> coord_right)

nextHeadPosition :: Direction -> SnakePosition -> LogicCoord
nextHeadPosition direction snakeState =
    let headPosition = head $ unSnakePosition snakeState
        add_term = case direction of
            Snake.Up -> mkLogicCoord 0 (-1)
            Snake.Down -> mkLogicCoord 0 1
            Snake.Left -> mkLogicCoord (-1) 0
            Snake.Right -> mkLogicCoord 1 0
    in headPosition <> add_term

moveSnake :: Direction -> GameState -> Either ImpossibleMove GameState
moveSnake direction currentState =
    let newHeadPosition = nextHeadPosition direction $ snakePosition currentState
        newTailPosition = init (unSnakePosition $ snakePosition currentState)
        isBoundaryHit = not $ withinBoundary (gameDimensions currentState) (unLogicCoord newHeadPosition)
        newGameState =
            if elem newHeadPosition $ foodPositions currentState
            then currentState {
                foodPositions = filter (/= newHeadPosition) $ foodPositions currentState,
                snakePosition = SnakePosition $ newHeadPosition : unSnakePosition (snakePosition currentState)
            }
            else currentState {
                snakePosition = newSnakeState
            } where
                newSnakeState = SnakePosition $ newHeadPosition : newTailPosition
        collision = elem newHeadPosition newTailPosition
    in
        if collision then Prelude.Left CellTaken
        else if isBoundaryHit then Prelude.Left $ BoundaryHit newHeadPosition
        else Prelude.Right newGameState