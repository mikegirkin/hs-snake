module SnakeUI

where
import Snake

newtype GraphCoord = GraphCoord {
    coord :: Coord
} deriving (Eq, Show)

