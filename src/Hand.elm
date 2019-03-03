module Hand
    exposing
        ( Direction(..)
        , Hand
        , animate
        , height
        , init
        , move
        , sprite
        , toSvg
        , width
        )

import Screen exposing (Position)
import Sprite exposing (Sprite)
import Svg exposing (Svg)


type alias Hand =
    { direction : Direction
    , position : Position
    }


type Direction
    = Left
    | Right
    | Stop


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
        { direction = Stop
        , position = { x = x, y = y }
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


move : Direction -> Hand -> Hand
move direction hand =
    { hand | direction = direction }


animate : Hand -> Hand
animate hand =
    let
        amount =
            15
    in
        case hand.direction of
            Stop ->
                hand

            Left ->
                { hand
                    | position =
                        increaseXPositionBy
                            (negate amount)
                            hand.position
                }

            Right ->
                { hand
                    | position =
                        increaseXPositionBy
                            amount
                            hand.position
                }


increaseXPositionBy : Int -> Position -> Position
increaseXPositionBy amount position =
    let
        leftEdge =
            0

        rightEdge =
            Screen.width - width

        next =
            position.x + amount

        x =
            if next <= leftEdge then
                leftEdge
            else if next >= rightEdge then
                rightEdge
            else
                next
    in
        { x = x
        , y = position.y
        }
