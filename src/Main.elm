module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onKeyUp)
import Html
import Html.Attributes
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
import Time
import Util exposing (flip)
import WebGL


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
    = KeyDown Key
    | KeyUp Key
    | Tick Float


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
        []



-- [ onKeyDown <| decodeKey KeyDown
-- , onKeyUp <| decodeKey KeyUp
-- , onAnimationFrameDelta Tick
-- ]


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

        _ ->
            Other


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDown key ->
            handleKeyDown key model

        KeyUp key ->
            handleKeyUp key model

        Tick timeSinceLastFrame ->
            handleTick timeSinceLastFrame model


handleKeyDown : Key -> Model -> ( Model, Cmd Msg )
handleKeyDown key model =
    case key of
        Left ->
            ( { model | hand = Hand.move Hand.Left model.hand }
            , Cmd.none
            )

        Right ->
            ( { model | hand = Hand.move Hand.Right model.hand }
            , Cmd.none
            )

        Other ->
            ( model, Cmd.none )


handleKeyUp : Key -> Model -> ( Model, Cmd Msg )
handleKeyUp key model =
    case key of
        Left ->
            ( { model | hand = Hand.move Hand.Stop model.hand }
            , Cmd.none
            )

        Right ->
            ( { model | hand = Hand.move Hand.Stop model.hand }
            , Cmd.none
            )

        Other ->
            ( model, Cmd.none )


handleTick : Float -> Model -> ( Model, Cmd Msg )
handleTick timeSinceLastFrame model =
    ( { model
        | faces = Face.Grid.animate model.faces
        , hand = Hand.animate model.hand
      }
    , Cmd.none
    )


view : Model -> Html.Html Msg
view model =
    renderGame model


renderGame : Model -> Html.Html Msg
renderGame model =
    WebGL.toHtmlWith
        [ WebGL.clearColor 241 241 241 1
        , WebGL.depth 1
        , WebGL.stencil 0
        ]
        [ Html.Attributes.height Screen.height
        , Html.Attributes.style "border" "3px solid #333"
        , Html.Attributes.width Screen.width
        ]
        []
