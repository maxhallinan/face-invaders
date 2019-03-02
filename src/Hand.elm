module Hand exposing (Hand, height, init, move, sprite, toSvg, width)

import Screen exposing (Position)
import Sprite exposing (Sprite)
import Svg exposing (Svg)


type alias Hand =
    { position : Position }


init : Hand
init =
    let
        x =
            -- horizontal center
            (Screen.width // 2) - (width // 2)

        y =
            -- near the bottom of the screen
            Screen.height - (height + 6)
    in
        { position = { x = x, y = y }
        }


height : Int
height =
    51


width : Int
width =
    45


sprite : Sprite
sprite =
    Sprite.init
        [ [ 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0 ]
        , [ 0, 0, 0, 0, 1, 3, 3, 1, 0, 0, 0, 0, 0, 0, 0 ]
        , [ 0, 0, 0, 0, 1, 3, 3, 1, 0, 0, 0, 0, 0, 0, 0 ]
        , [ 0, 0, 0, 0, 1, 3, 3, 1, 0, 0, 0, 0, 0, 0, 0 ]
        , [ 0, 0, 0, 0, 1, 3, 3, 1, 1, 1, 1, 1, 0, 0, 0 ]
        , [ 0, 0, 0, 0, 1, 3, 3, 1, 3, 3, 1, 3, 1, 1, 0 ]
        , [ 0, 0, 0, 0, 1, 3, 3, 1, 3, 3, 1, 3, 1, 3, 1 ]
        , [ 0, 1, 1, 0, 1, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1 ]
        , [ 1, 3, 3, 1, 1, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1 ]
        , [ 1, 3, 3, 3, 1, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1 ]
        , [ 0, 1, 3, 3, 3, 3, 3, 1, 3, 1, 3, 1, 3, 3, 1 ]
        , [ 0, 0, 1, 3, 3, 3, 3, 1, 3, 1, 3, 1, 3, 3, 1 ]
        , [ 0, 0, 1, 3, 3, 3, 3, 1, 3, 1, 3, 1, 3, 1, 0 ]
        , [ 0, 0, 0, 1, 3, 3, 3, 1, 3, 1, 3, 1, 3, 1, 0 ]
        , [ 0, 0, 0, 0, 1, 3, 3, 3, 3, 3, 3, 3, 1, 0, 0 ]
        , [ 0, 0, 0, 0, 0, 1, 3, 3, 3, 3, 1, 3, 1, 0, 0 ]
        , [ 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 1, 1, 0, 0 ]
        ]


toSvg : Position -> Svg a
toSvg =
    Sprite.toSvg sprite


move : Int -> Hand -> Hand
move amount hand =
    let
        leftEdge =
            0

        rightEdge =
            Screen.width - width

        next =
            hand.position.x + amount

        x =
            if next <= leftEdge then
                leftEdge
            else if next >= rightEdge then
                rightEdge
            else
                next

        position =
            { x = x
            , y = hand.position.y
            }
    in
        { hand | position = position }
