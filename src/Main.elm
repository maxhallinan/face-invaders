module Main exposing (main)

import Browser
import Html
import String
import Svg
import Svg.Attributes


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Model =
    Int


type Msg
    = Foo


init : () -> ( Model, Cmd Msg )
init _ =
    ( 0, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : a -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )


view : Model -> Html.Html Msg
view model =
    screen model


screenSize : Int
screenSize =
    512


pixelSize : Int
pixelSize =
    3


screen : Model -> Html.Html Msg
screen model =
    Svg.svg
        [ Svg.Attributes.style "background-color: #f1f1f1; border: 3px solid #333"
        , Svg.Attributes.height (String.fromInt screenSize)
        , Svg.Attributes.width (String.fromInt (screenSize + 192))
        ]
        [ Svg.g [] <| spriteGraphic finger { x = 6, y = 6 }
        , Svg.g [] <| spriteGraphic face { x = 100, y = 6 }
        ]



-- Sprites


type alias Position =
    { x : Int, y : Int }


type alias Grid a =
    List (List a)


mapGrid : (a -> b) -> Grid a -> Grid b
mapGrid f =
    List.map (List.map f)


type alias Sprite =
    Grid Pixel


type Pixel
    = Empty
    | White
    | Gray
    | Black


intToPixel : Int -> Pixel
intToPixel int =
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


pixelGrid : Grid Int -> Grid Pixel
pixelGrid =
    mapGrid intToPixel


finger : Sprite
finger =
    pixelGrid
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


face : Sprite
face =
    pixelGrid
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


bullet : Sprite
bullet =
    mapGrid intToPixel
        [ [ 1 ] ]


type alias Graphic =
    List (Svg.Svg Msg)


spriteGraphic : Sprite -> Position -> Graphic
spriteGraphic sprite position =
    let
        render : List Pixel -> ( Graphic, Position ) -> ( Graphic, Position )
        render pixelRow ( pixels, { x, y } ) =
            let
                row =
                    renderRow pixelRow { x = x, y = y }
            in
                ( List.append row pixels
                , { x = x, y = y + pixelSize }
                )

        ( svg, _ ) =
            List.foldl render ( [], position ) sprite
    in
        svg


renderRow : List Pixel -> Position -> Graphic
renderRow row position =
    let
        render : Pixel -> ( Graphic, Position ) -> ( Graphic, Position )
        render pixel ( svg, { x, y } ) =
            let
                nextPosition =
                    { x = x + pixelSize, y = y }
            in
                case pixel of
                    Empty ->
                        ( svg, nextPosition )

                    _ ->
                        ( renderPixel pixel { x = x, y = y } :: svg
                        , nextPosition
                        )

        ( graphic, _ ) =
            List.foldl render ( [], position ) row
    in
        graphic


renderPixel : Pixel -> Position -> Svg.Svg Msg
renderPixel pixel { x, y } =
    Svg.rect
        [ Svg.Attributes.fill (pixelColor pixel)
        , Svg.Attributes.height (String.fromInt pixelSize)
        , Svg.Attributes.width (String.fromInt pixelSize)
        , Svg.Attributes.x (String.fromInt x)
        , Svg.Attributes.y (String.fromInt y)
        ]
        []


pixelColor : Pixel -> String
pixelColor pixel =
    case pixel of
        Empty ->
            "rgba(255,255,255,0)"

        White ->
            "rgba(255,255,255,1)"

        Gray ->
            "rgba(203,203,203,1)"

        Black ->
            "rgba(51,51,51,1)"
