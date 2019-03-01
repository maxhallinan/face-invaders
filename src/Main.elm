module Main exposing (main)

import Browser
import Browser.Events exposing (onKeyDown)
import Html
import Json.Decode
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
    { hand : HandState
    }


type alias HandState =
    { position : Position
    }


type alias Position =
    { x : Int, y : Int }


type Msg
    = KeyPress Key


type Key
    = Left
    | Right
    | Other


init : () -> ( Model, Cmd Msg )
init _ =
    ( { hand = initHandState }
    , Cmd.none
    )


initHandState : HandState
initHandState =
    let
        x =
            -- horizontal center
            (screenWidth // 2) - (handWidth // 2)

        y =
            -- near the bottom of the screen
            screenHeight - (handHeight + 6)
    in
        { position = { x = x, y = y }
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onKeyDown decodeKey
        ]


decodeKey : Json.Decode.Decoder Msg
decodeKey =
    Json.Decode.field "key" Json.Decode.string
        |> Json.Decode.map (KeyPress << toKey)


toKey : String -> Key
toKey k =
    case k of
        "ArrowLeft" ->
            Left

        "ArrowRight" ->
            Right

        _ ->
            Other


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyPress key ->
            handleKeyPress key model


handleKeyPress : Key -> Model -> ( Model, Cmd Msg )
handleKeyPress key model =
    let
        moveBy =
            15
    in
        case key of
            Left ->
                ( moveHand (negate moveBy) model
                , Cmd.none
                )

            Right ->
                ( moveHand moveBy model
                , Cmd.none
                )

            Other ->
                ( model, Cmd.none )


moveHand : Int -> Model -> Model
moveHand increase model =
    let
        leftEdge =
            0

        rightEdge =
            screenWidth - handWidth

        next =
            model.hand.position.x + increase

        x =
            if next <= leftEdge then
                leftEdge
            else if next >= rightEdge then
                rightEdge
            else
                next

        position =
            { x = x
            , y = model.hand.position.y
            }

        handState =
            model.hand
    in
        { model | hand = { handState | position = position } }


view : Model -> Html.Html Msg
view model =
    screen model


screenHeight : Int
screenHeight =
    512


screenWidth : Int
screenWidth =
    704


handHeight : Int
handHeight =
    51


handWidth : Int
handWidth =
    45


pixelSize : Int
pixelSize =
    3


screen : Model -> Html.Html Msg
screen model =
    Svg.svg
        [ Svg.Attributes.style "background-color: #f1f1f1; border: 3px solid #333"
        , Svg.Attributes.height (String.fromInt screenHeight)
        , Svg.Attributes.width (String.fromInt screenWidth)
        ]
        [ Svg.g [] <| spriteGraphic hand model.hand.position
        ]



-- Sprites


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


hand : Sprite
hand =
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
