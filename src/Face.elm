module Face exposing (Face, Direction(..), height, init, reverseDirection, sprite, toSvg, width)

import Grid exposing (Grid)
import Screen exposing (Position)
import Sprite exposing (Sprite)
import Svg exposing (Svg)


type alias Face =
    { position : Position
    , direction : Direction
    }


init : Direction -> Position -> Face
init direction position =
    { direction = direction
    , position = position
    }


type Direction
    = Left
    | Right


width : Int
width =
    48


height : Int
height =
    51


sprite : Sprite
sprite =
    Sprite.init
        [ [ 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0 ]
        , [ 0, 0, 1, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 1, 0, 0 ]
        , [ 0, 1, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 1, 0 ]
        , [ 0, 1, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 1, 0 ]
        , [ 0, 1, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 1, 0 ]
        , [ 1, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 1 ]
        , [ 1, 3, 3, 3, 3, 3, 1, 3, 3, 1, 3, 3, 3, 3, 2, 1 ]
        , [ 1, 3, 3, 3, 3, 3, 1, 3, 3, 1, 3, 3, 3, 3, 2, 1 ]
        , [ 1, 3, 3, 3, 3, 3, 1, 3, 3, 1, 3, 3, 3, 3, 2, 1 ]
        , [ 1, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 1 ]
        , [ 1, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 1 ]
        , [ 1, 3, 3, 3, 3, 1, 3, 3, 3, 3, 1, 3, 3, 2, 2, 1 ]
        , [ 0, 1, 3, 3, 3, 3, 1, 1, 1, 1, 3, 3, 3, 2, 1, 0 ]
        , [ 0, 1, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 1, 0 ]
        , [ 0, 1, 2, 3, 3, 3, 3, 2, 2, 3, 3, 3, 2, 2, 1, 0 ]
        , [ 0, 0, 1, 2, 2, 2, 2, 1, 1, 2, 2, 2, 2, 1, 0, 0 ]
        , [ 0, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 0 ]
        ]


toSvg : Position -> Svg a
toSvg =
    Sprite.toSvg sprite


reverseDirection : Face -> Face
reverseDirection face =
    case face.direction of
        Left ->
            { face | direction = Right }

        Right ->
            { face | direction = Left }
