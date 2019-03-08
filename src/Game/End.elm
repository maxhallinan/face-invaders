module Game.End exposing (Event(..), Msg(..), subscriptions, update)

import Browser.Events exposing (onKeyDown)
import Game exposing (Game)
import Util


type Msg
    = KeyDown Key


type Event
    = NewGame
    | None


type Key
    = Enter
    | Other


update : Msg -> Game -> ( ( Game, Event ), Cmd Msg )
update msg model =
    case msg of
        KeyDown Enter ->
            ( ( model, NewGame ), Cmd.none )

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
