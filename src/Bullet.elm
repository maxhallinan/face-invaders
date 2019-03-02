module Bullet exposing (Bullet, sprite)

import Screen exposing (Position)
import Sprite exposing (Sprite)
import Svg exposing (Svg)


type alias Bullet =
    { position : Position }


sprite : Sprite
sprite =
    Sprite.init
        [ [ 1 ] ]


toSvg : Position -> Svg a
toSvg =
    Sprite.toSvg sprite
