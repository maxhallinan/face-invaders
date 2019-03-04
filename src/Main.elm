module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onKeyUp)
import Bullet exposing (Bullet)
import Face exposing (Face)
import Face.Grid
import Game
import Grid exposing (Grid)
import Hand exposing (Hand)
import Html
import Html.Attributes
import Json.Decode
import Screen exposing (Position)
import String
import Svg
import Svg.Attributes
import Time
import Tuple
import Util exposing (flip)


main : Program {} Model Msg
main =
    Browser.element
        { init = always init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Model =
    Game.Model


type Msg
    = KeyDown Key
    | KeyUp Key
    | Tick


init : ( Model, Cmd Msg )
init =
    ( Game.init, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onKeyDown <| decodeKey KeyDown
        , onKeyUp <| decodeKey KeyUp
        , onAnimationFrameDelta <| always Tick
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDown key ->
            handleKeyDown key model

        KeyUp key ->
            handleKeyUp key model

        Tick ->
            handleTick model


view : Model -> Html.Html Msg
view model =
    Game.view model


type Key
    = Left
    | Right
    | Shoot
    | Other


decodeKey : (Key -> Msg) -> Json.Decode.Decoder Msg
decodeKey msg =
    Json.Decode.field "key" Json.Decode.string
        |> Json.Decode.map (msg << toKey)


toKey : String -> Key
toKey k =
    case k of
        "ArrowLeft" ->
            Left

        "ArrowRight" ->
            Right

        "Spacebar" ->
            Shoot

        " " ->
            Shoot

        _ ->
            Other


handleKeyDown : Key -> Model -> ( Model, Cmd Msg )
handleKeyDown key model =
    case key of
        Left ->
            ( Game.moveHandLeft model, Cmd.none )

        Right ->
            ( Game.moveHandRight model, Cmd.none )

        Shoot ->
            ( Game.shootUp model, Cmd.none )

        Other ->
            ( model, Cmd.none )


handleKeyUp : Key -> Model -> ( Model, Cmd Msg )
handleKeyUp key model =
    case key of
        Left ->
            ( Game.stopHand model, Cmd.none )

        Right ->
            ( Game.stopHand model, Cmd.none )

        Shoot ->
            ( model, Cmd.none )

        Other ->
            ( model, Cmd.none )


handleTick : Model -> ( Model, Cmd Msg )
handleTick model =
    ( Game.animate model, Cmd.none )
