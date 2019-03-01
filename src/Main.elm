module Main exposing (main)

import Browser
import Html


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Model =
    Int


type Msg
    = Foo


init : () -> ( Model, Cmd Msg )
init _ =
    ( 0, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : a -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )


view : Model -> Html.Html Msg
view model =
    Html.text "Hello World!"
