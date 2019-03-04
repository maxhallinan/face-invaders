module Game
    exposing
        ( Model
        , animate
        , init
        , moveHandLeft
        , moveHandRight
        , shootUp
        , stopHand
        , view
        )

import Bullet exposing (Bullet)
import Face exposing (Face)
import Face.Grid
import Grid exposing (Grid)
import Hand exposing (Hand)
import Html.Attributes
import Screen
import Svg
import Svg.Attributes


type alias Model =
    { bullets : List Bullet
    , faces : Grid Face
    , hand : Hand
    }


init : Model
init =
    { bullets = []
    , faces = Face.Grid.init Face.Right
    , hand = Hand.init
    }


moveHandRight : Model -> Model
moveHandRight =
    moveHand Hand.Right


moveHandLeft : Model -> Model
moveHandLeft =
    moveHand Hand.Left


stopHand : Model -> Model
stopHand =
    moveHand Hand.Stop


moveHand : Hand.Direction -> Model -> Model
moveHand direction model =
    { model | hand = Hand.move direction model.hand }


shootUp : Model -> Model
shootUp model =
    let
        position =
            { x = model.hand.position.x + 15
            , y = model.hand.position.y
            }

        bullet =
            Bullet.shootUp position
    in
    { model | bullets = bullet :: model.bullets }


animate : Model -> Model
animate model =
    { model
        | bullets = List.map Bullet.animate model.bullets
        , faces = Face.Grid.animate model.faces
        , hand = Hand.animate model.hand
    }
        |> collectGarbage


collectGarbage : Model -> Model
collectGarbage model =
    { model
        | bullets = List.filter (not << Screen.isOffScreen << .position) model.bullets
    }


view : Model -> Svg.Svg a
view model =
    Svg.svg
        [ Svg.Attributes.height (String.fromFloat Screen.height)
        , Html.Attributes.style "border" "3px solid #333"
        , Svg.Attributes.width (String.fromFloat Screen.width)
        ]
        [ Hand.toSvg model.hand.position
        , Svg.g [] <| List.map (Face.toSvg << .position) (List.concat model.faces)
        , Svg.g [] <| List.map (Bullet.toSvg << .position) model.bullets
        ]
