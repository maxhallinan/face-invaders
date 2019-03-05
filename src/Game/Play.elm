module Game.Play exposing (Event(..), Msg(..), subscriptions, update)

import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onKeyUp)
import Game
import Random
import Util


type Msg
    = KeyDown Key
    | KeyUp Key
    | Tick Float
    | DiceRoll Int


type Event
    = PauseGame
    | EndGame
    | None


update : Msg -> Game.Model -> ( ( Game.Model, Event ), Cmd Msg )
update msg game =
    case msg of
        KeyDown key ->
            handleKeyDown key game

        KeyUp key ->
            handleKeyUp key game

        Tick timeSinceLastTick ->
            handleTick game

        DiceRoll randomNum ->
            handleDiceRoll randomNum game


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ onKeyDown <| Util.decodeKey (KeyDown << toKey)
        , onKeyUp <| Util.decodeKey (KeyUp << toKey)
        , onAnimationFrameDelta Tick
        ]


type Key
    = Enter
    | Left
    | Right
    | Space
    | Other


toKey : String -> Key
toKey k =
    case k of
        "ArrowLeft" ->
            Left

        "ArrowRight" ->
            Right

        "Spacebar" ->
            Space

        " " ->
            Space

        "Enter" ->
            Enter

        _ ->
            Other


handleKeyDown : Key -> Game.Model -> ( ( Game.Model, Event ), Cmd Msg )
handleKeyDown key game =
    case key of
        Left ->
            ( ( Game.moveHandLeft game, None ), Cmd.none )

        Right ->
            ( ( Game.moveHandRight game, None ), Cmd.none )

        Space ->
            ( ( Game.shootUp game, None ), Cmd.none )

        Enter ->
            ( ( game, PauseGame ), Cmd.none )

        Other ->
            ( ( game, None ), Cmd.none )


handleKeyUp : Key -> Game.Model -> ( ( Game.Model, Event ), Cmd Msg )
handleKeyUp key game =
    case key of
        Left ->
            ( ( Game.stopHand game, None ), Cmd.none )

        Right ->
            ( ( Game.stopHand game, None ), Cmd.none )

        Space ->
            ( ( game, None ), Cmd.none )

        Enter ->
            ( ( game, None ), Cmd.none )

        Other ->
            ( ( game, None ), Cmd.none )


handleTick : Game.Model -> ( ( Game.Model, Event ), Cmd Msg )
handleTick game =
    ( ( Game.animate game, None ), rollDice )


handleDiceRoll : Int -> Game.Model -> ( ( Game.Model, Event ), Cmd Msg )
handleDiceRoll n game =
    ( ( Game.dropBomb n game, None ), Cmd.none )


rollDice : Cmd Msg
rollDice =
    Random.generate DiceRoll <| Random.int 0 560
