module Sprite exposing (Graphic, Sprite, init, toGraphic, toSvg)

import Grid exposing (Grid)
import Screen exposing (Position)
import Sprite.Pixel exposing (Pixel(..))
import String
import Svg exposing (Svg)
import Svg.Attributes


type alias Sprite =
    Grid Pixel


init : Grid Int -> Grid Pixel
init =
    Grid.map Sprite.Pixel.fromInt


toSvg : Sprite -> Position -> Svg a
toSvg sprite position =
    Svg.g [] <| toGraphic sprite position


type alias Graphic a =
    List (Svg a)


toGraphic : Sprite -> Position -> Graphic a
toGraphic sprite position =
    let
        render : List Pixel -> ( Graphic a, Position ) -> ( Graphic a, Position )
        render pixelRow ( pixels, { x, y } ) =
            let
                row =
                    renderRow pixelRow { x = x, y = y }
            in
                ( List.append row pixels
                , { x = x, y = y + Sprite.Pixel.size }
                )

        ( svg, _ ) =
            List.foldl render ( [], position ) sprite
    in
        svg


renderRow : List Pixel -> Position -> Graphic a
renderRow row position =
    let
        render : Pixel -> ( Graphic a, Position ) -> ( Graphic a, Position )
        render pixel ( svg, { x, y } ) =
            let
                nextPosition =
                    { x = x + Sprite.Pixel.size, y = y }
            in
                case pixel of
                    Empty ->
                        ( svg, nextPosition )

                    _ ->
                        ( Sprite.Pixel.toSvg pixel { x = x, y = y } :: svg
                        , nextPosition
                        )

        ( graphic, _ ) =
            List.foldl render ( [], position ) row
    in
        graphic
