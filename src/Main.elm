module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown)
import Html
import Json.Decode
import String
import Svg
import Svg.Attributes
import Tuple
import Bullet exposing (Bullet)
import Face exposing (Face)
import Face.Grid
import Hand exposing (Hand)
import Grid exposing (Grid)
import Screen exposing (Position)
import Sprite exposing (Sprite)
import Util exposing (flip)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Model =
    { bullets : List Bullet
    , faces : Grid Face
    , hand : Hand
    }


type Msg
    = KeyPress Key
    | Animate Float


type Key
    = Left
    | Right
    | Other


init : () -> ( Model, Cmd Msg )
init _ =
    ( { bullets = []
      , faces = Face.Grid.init Face.Right
      , hand = Hand.init
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onKeyDown decodeKey
        , onAnimationFrameDelta Animate
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

        Animate _ ->
            handleAnimate model


handleKeyPress : Key -> Model -> ( Model, Cmd Msg )
handleKeyPress key model =
    let
        moveAmount =
            15
    in
        case key of
            Left ->
                ( { model | hand = Hand.move (negate moveAmount) model.hand }
                , Cmd.none
                )

            Right ->
                ( { model | hand = Hand.move moveAmount model.hand }
                , Cmd.none
                )

            Other ->
                ( model, Cmd.none )


handleAnimate : Model -> ( Model, Cmd Msg )
handleAnimate model =
    ( { model | faces = Face.Grid.animate model.faces }
    , Cmd.none
    )


view : Model -> Html.Html Msg
view model =
    screen model


screen : Model -> Html.Html Msg
screen model =
    Svg.svg
        [ Svg.Attributes.style "background-color: #f1f1f1; border: 3px solid #333"
        , Svg.Attributes.height (String.fromInt Screen.height)
        , Svg.Attributes.width (String.fromInt Screen.width)
        ]
        [ Hand.toSvg model.hand.position
        , Svg.g [] <| List.map (Face.toSvg << .position) (List.concat model.faces)
        ]
