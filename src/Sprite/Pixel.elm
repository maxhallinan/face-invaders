module Sprite.Pixel exposing (Pixel(..), fromInt, size, toRgba, toSvg)

import Screen exposing (Position)
import String
import Svg
import Svg.Attributes


type Pixel
    = Empty
    | White
    | Gray
    | Black


size : Int
size =
    3


fromInt : Int -> Pixel
fromInt int =
    case int of
        0 ->
            Empty

        1 ->
            Black

        2 ->
            Gray

        3 ->
            White

        _ ->
            Empty


toRgba : Pixel -> String
toRgba pixel =
    case pixel of
        Empty ->
            "rgba(255,255,255,0)"

        White ->
            "rgba(255,255,255,1)"

        Gray ->
            "rgba(203,203,203,1)"

        Black ->
            "rgba(51,51,51,1)"


toSvg : Pixel -> Position -> Svg.Svg a
toSvg pixel { x, y } =
    Svg.rect
        [ Svg.Attributes.fill (toRgba pixel)
        , Svg.Attributes.height (String.fromInt size)
        , Svg.Attributes.width (String.fromInt size)
        , Svg.Attributes.x (String.fromInt x)
        , Svg.Attributes.y (String.fromInt y)
        ]
        []
