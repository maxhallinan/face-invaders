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
    { game : Game.Model
    , state : State
    }


type Msg
    = KeyDown Key
    | KeyUp Key
    | Tick
    | DiceRoll Int


init : ( Model, Cmd Msg )
init =
    ( { game = Game.init, state = Start }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        defaultSubs =
            Sub.batch
                [ onKeyDown <| Key.decode KeyDown
                ]
    in
        case model.state of
            Start ->
                defaultSubs

            Pause ->
                defaultSubs

            End ->
                defaultSubs

            Play ->
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
    Html.div
        []
        [ Game.view model.game
        , helpView model
        ]


helpView : Model -> Html.Html Msg
helpView model =
    let
        helpText =
            String.join ", "
                [ "arrows to move"
                , "space to shoot"
                , enterKeyHelp model
                ]
    in
        Html.p [] [ Html.text helpText ]


handleKeyDown : Key -> Model -> ( Model, Cmd Msg )
handleKeyDown key model =
    case model.state of
        Play ->
            case key of
                Key.Left ->
                    ( { model | game = Game.moveHandLeft model.game }, Cmd.none )

                Key.Right ->
                    ( { model | game = Game.moveHandRight model.game }, Cmd.none )

                Key.Space ->
                    ( { model | game = Game.shootUp model.game }, Cmd.none )

                Key.Enter ->
                    ( handleEnterPress model
                    , Cmd.none
                    )

                Key.Other ->
                    ( model, Cmd.none )

        _ ->
            case key of
                Key.Enter ->
                    ( handleEnterPress model
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


handleKeyUp : Key -> Model -> ( Model, Cmd Msg )
handleKeyUp key model =
    case model.state of
        Play ->
            case key of
                Key.Left ->
                    ( { model | game = Game.stopHand model.game }, Cmd.none )

                Key.Right ->
                    ( { model | game = Game.stopHand model.game }, Cmd.none )

                Key.Space ->
                    ( model, Cmd.none )

                Key.Enter ->
                    ( model, Cmd.none )

                Key.Other ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


handleTick : Model -> ( Model, Cmd Msg )
handleTick model =
    case model.state of
        Play ->
            ( { model | game = Game.animate model.game }, rollDice )

        _ ->
            ( model, Cmd.none )


handleDiceRoll : Int -> Model -> ( Model, Cmd Msg )
handleDiceRoll n model =
    case model.state of
        Play ->
            ( { model | game = Game.dropBomb n model.game }, Cmd.none )

        _ ->
            ( model, Cmd.none )


rollDice : Cmd Msg
rollDice =
    Random.generate DiceRoll <| Random.int 0 560


type State
    = Start
    | Play
    | Pause
    | End


handleEnterPress : Model -> Model
handleEnterPress model =
    case model.state of
        Start ->
            { model | state = Play }

        Play ->
            { model | state = Pause }

        Pause ->
            { model | state = Play }

        End ->
            { model | game = Game.init, state = Play }


enterKeyHelp : Model -> String
enterKeyHelp model =
    case model.state of
        Start ->
            "enter to start"

        Play ->
            "enter to pause"

        Pause ->
            "enter to unpause"

        End ->
            "enter for new game"
