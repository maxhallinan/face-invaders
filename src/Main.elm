module Main exposing (main)

import Browser
import Game
import Game.End
import Game.Pause
import Game.Play
import Game.Start
import Html
import Html.Attributes
import String
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


type State
    = Start
    | Play
    | Pause
    | End


type Msg
    = StartMsg Game.Start.Msg
    | PlayMsg Game.Play.Msg
    | PauseMsg Game.Pause.Msg
    | EndMsg Game.End.Msg


init : ( Model, Cmd Msg )
init =
    ( { game = Game.init, state = Start }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.state of
        Start ->
            Game.Start.subscriptions
                |> Sub.map StartMsg

        Pause ->
            Game.Pause.subscriptions
                |> Sub.map PauseMsg

        Play ->
            Game.Play.subscriptions
                |> Sub.map PlayMsg

        End ->
            Game.End.subscriptions
                |> Sub.map EndMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartMsg startMsg ->
            let
                ( ( game, event ), cmd ) =
                    Game.Start.update startMsg model.game
                        |> Tuple.mapSecond (Cmd.map StartMsg)
            in
            case event of
                Game.Start.PlayGame ->
                    ( { model | state = Play, game = game }, cmd )

                Game.Start.None ->
                    ( { model | game = game }, cmd )

        PlayMsg playMsg ->
            let
                ( ( game, event ), cmd ) =
                    Game.Play.update playMsg model.game
                        |> Tuple.mapSecond (Cmd.map PlayMsg)
            in
            case event of
                Game.Play.EndGame ->
                    ( { model | state = End, game = game }, cmd )

                Game.Play.PauseGame ->
                    ( { model | state = Pause, game = game }, cmd )

                Game.Play.None ->
                    ( { model | game = game }, cmd )

        PauseMsg pauseMsg ->
            let
                ( ( game, event ), cmd ) =
                    Game.Pause.update pauseMsg model.game
                        |> Tuple.mapSecond (Cmd.map PauseMsg)
            in
            case event of
                Game.Pause.UnpauseGame ->
                    ( { model | state = Play, game = game }, cmd )

                Game.Pause.None ->
                    ( { model | game = game }, cmd )

        EndMsg endMsg ->
            let
                ( ( game, event ), cmd ) =
                    Game.End.update endMsg model.game
                        |> Tuple.mapSecond (Cmd.map EndMsg)
            in
            case event of
                Game.End.NewGame ->
                    ( { model | state = Play, game = Game.init }, cmd )

                Game.End.None ->
                    ( { model | game = game }, cmd )


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
                , enterKeyHelp model.state
                ]
    in
    Html.p [] [ Html.text helpText ]


enterKeyHelp : State -> String
enterKeyHelp state =
    case state of
        Start ->
            "enter to start"

        Play ->
            "enter to pause"

        Pause ->
            "enter to unpause"

        End ->
            "enter for new game"
