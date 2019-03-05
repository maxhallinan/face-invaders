module Game.Pause exposing (Event(..), Msg(..), subscriptions, update)

import Browser.Events exposing (onKeyDown)
import Game
import Util


type Msg
    = KeyDown Key


type Event
    = UnpauseGame
    | None


type Key
    = Enter
    | Other


update : Msg -> Game.Model -> ( ( Game.Model, Event ), Cmd Msg )
update msg model =
    case msg of
        KeyDown Enter ->
            ( ( model, UnpauseGame ), Cmd.none )

        KeyDown Other ->
            ( ( model, None ), Cmd.none )


subscriptions : Sub Msg
subscriptions =
    onKeyDown (Util.decodeKey <| KeyDown << toKey)


toKey : String -> Key
toKey keyCode =
    case keyCode of
        "Enter" ->
            Enter

        _ ->
            Other
