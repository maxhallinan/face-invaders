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
import Key exposing (Key)
import Random
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
    | DiceRoll Int


init : ( Model, Cmd Msg )
init =
    ( Game.init, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onKeyDown <| Key.decode KeyDown
        , onKeyUp <| Key.decode KeyUp
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

        DiceRoll n ->
            handleDiceRoll n model


view : Model -> Html.Html Msg
view model =
    Game.view model


handleKeyDown : Key -> Model -> ( Model, Cmd Msg )
handleKeyDown key model =
    case key of
        Key.Left ->
            ( Game.moveHandLeft model, Cmd.none )

        Key.Right ->
            ( Game.moveHandRight model, Cmd.none )

        Key.Space ->
            ( Game.shootUp model, Cmd.none )

        Key.Other ->
            ( model, Cmd.none )


handleKeyUp : Key -> Model -> ( Model, Cmd Msg )
handleKeyUp key model =
    case key of
        Key.Left ->
            ( Game.stopHand model, Cmd.none )

        Key.Right ->
            ( Game.stopHand model, Cmd.none )

        Key.Space ->
            ( model, Cmd.none )

        Key.Other ->
            ( model, Cmd.none )


handleTick : Model -> ( Model, Cmd Msg )
handleTick model =
    ( Game.animate model, rollDice )


handleDiceRoll : Int -> Model -> ( Model, Cmd Msg )
handleDiceRoll n model =
    ( Game.dropBomb n model, Cmd.none )


rollDice : Cmd Msg
rollDice =
    Random.generate DiceRoll <| Random.int 0 560
