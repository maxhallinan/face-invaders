module Game
    exposing
        ( Model
        , animate
        , dropBomb
        , init
        , moveHandLeft
        , moveHandRight
        , shootUp
        , stopHand
        , view
        )

import Array
import Bullet exposing (Bullet)
import Face exposing (Face)
import Face.Grid
import Grid exposing (Grid)
import Hand exposing (Hand)
import Html.Attributes
import Screen exposing (Position)
import Svg
import Svg.Attributes
import Util


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


dropBomb : Int -> Model -> Model
dropBomb n model =
    if n < 9 then
        let
            bombPosition : Maybe Position
            bombPosition =
                getBombPosition n model.faces

            shootDown : Position -> Model -> Model
            shootDown position m =
                { m | bullets = Bullet.shootDown position :: m.bullets }
        in
        Maybe.map (Util.flip shootDown model) bombPosition
            |> Maybe.withDefault model
    else
        model


getBombPosition : Int -> Grid Face -> Maybe Position
getBombPosition index faces =
    let
        toBombOffset : Position -> Position
        toBombOffset position =
            -- drop bomb from the horizontal center and bottom of the face
            { x = position.x + ((Face.width / 2) - (Bullet.width / 2))
            , y = position.y + Face.height
            }
    in
    List.map (Array.get index << Array.fromList) (List.reverse faces)
        |> List.filter Util.isJust
        |> List.head
        |> Maybe.andThen identity
        |> Maybe.map (toBombOffset << .position)


view : Model -> Svg.Svg a
view model =
    Svg.svg
        [ Svg.Attributes.height (String.fromFloat Screen.height)
        , Html.Attributes.style "border" "3px solid #333"
        , Html.Attributes.style "background-color" "#fafafa"
        , Svg.Attributes.width (String.fromFloat Screen.width)
        ]
        [ Hand.toSvg model.hand.position
        , Svg.g [] <| List.map (Face.toSvg << .position) (List.concat model.faces)
        , Svg.g [] <| List.map (Bullet.toSvg << .position) model.bullets
        ]
