module Bullet exposing (Bullet, height, sprite, toSvg, width)

import Screen exposing (Position)
import Sprite exposing (Sprite)
import Svg exposing (Svg)


type alias Bullet =
    { position : Position }


height : Int
height =
    3


width : Int
width =
    3


sprite : Sprite
sprite =
    Sprite.init
        [ [ 1 ] ]


toSvg : Position -> Svg a
toSvg =
    Sprite.toSvg sprite
