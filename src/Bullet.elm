module Bullet
    exposing
        ( Bullet
        , animate
        , height
        , shootDown
        , shootUp
        , toSvg
        , width
        )

import Screen exposing (Position)
import Svg exposing (Svg)
import Svg.Attributes


type alias Bullet =
    { direction : Direction
    , position : Position
    }


type Direction
    = Up
    | Down


shootDown : Position -> Bullet
shootDown position =
    { direction = Down
    , position = position
    }


shootUp : Position -> Bullet
shootUp position =
    { direction = Up
    , position = position
    }


height : Float
height =
    3


width : Float
width =
    3


toSvg : Position -> Svg a
toSvg position =
    Svg.image
        [ Svg.Attributes.xlinkHref dataUri
        , Svg.Attributes.width (String.fromFloat width)
        , Svg.Attributes.height (String.fromFloat height)
        , Svg.Attributes.x (String.fromFloat position.x)
        , Svg.Attributes.y (String.fromFloat position.y)
        ]
        []


dataUri : String
dataUri =
    "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAMAAAADCAYAAABWKLW/AAAABmJLR0QA/wD/AP+gvaeTAAAACXBIWXMAAA3XAAAN1wFCKJt4AAAAB3RJTUUH4wMECSkzYIHreQAAADJJREFUCB0BJwDY/wEzMzP/AAAAAAAAAAACAAAAAAAAAAAAAAAAAgAAAAAAAAAAAAAAADmWAZ5om8s5AAAAAElFTkSuQmCC"


animate : Bullet -> Bullet
animate bullet =
    let
        position =
            bullet.position
    in
    case bullet.direction of
        Up ->
            { bullet | position = { position | y = position.y - 6 } }

        Down ->
            { bullet | position = { position | y = position.y + 6 } }
