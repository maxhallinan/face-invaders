module Game
    exposing
        ( Game
        , animate
        , detectDeadFaces
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


type alias Game =
    { bullets : List Bullet
    , faces : Grid Face
    , hand : Hand
    }


init : Game
init =
    { bullets = []
    , faces = Face.Grid.init Face.Right
    , hand = Hand.init
    }


moveHandRight : Game -> Game
moveHandRight =
    moveHand Hand.Right


moveHandLeft : Game -> Game
moveHandLeft =
    moveHand Hand.Left


stopHand : Game -> Game
stopHand =
    moveHand Hand.Stop


moveHand : Hand.Direction -> Game -> Game
moveHand direction game =
    { game | hand = Hand.move direction game.hand }


shootUp : Game -> Game
shootUp game =
    let
        position =
            { x = game.hand.position.x + 15
            , y = game.hand.position.y
            }

        bullet =
            Bullet.shootUp position
    in
    { game | bullets = bullet :: game.bullets }


animate : Game -> Game
animate game =
    { game
        | bullets = List.map Bullet.animate game.bullets
        , faces = Face.Grid.animate game.faces
        , hand = Hand.animate game.hand
    }
        |> detectDeadFaces
        |> collectGarbage


detectDeadFaces : Game -> Game
detectDeadFaces game =
    let
        bullets =
            List.filter Bullet.isDirectionUp game.bullets

        faces =
            Grid.map (detectDeadFace bullets) game.faces
    in
    { game | faces = faces }


detectDeadFace : List Bullet -> Face -> Face
detectDeadFace bullets face =
    if face.health == Face.Dead then
        face
    else
        List.foldl toDeadIfHit face bullets


toDeadIfHit : Bullet -> Face -> Face
toDeadIfHit bullet face =
    if isFaceHit bullet face then
        { face | health = Face.Dead }
    else
        face


isFaceHit : Bullet -> Face -> Bool
isFaceHit bullet face =
    Screen.isCollision
        ( Bullet.size, bullet.position )
        ( Face.size, face.position )


collectGarbage : Game -> Game
collectGarbage game =
    { game
        | bullets = List.filter (not << Screen.isOffScreen << .position) game.bullets
    }


dropBomb : Int -> Game -> Game
dropBomb n game =
    if n < 9 then
        let
            bombPosition : Maybe Position
            bombPosition =
                getBombPosition n game.faces

            shootDown : Position -> Game -> Game
            shootDown position m =
                { m | bullets = Bullet.shootDown position :: m.bullets }
        in
        Maybe.map (Util.flip shootDown game) bombPosition
            |> Maybe.withDefault game
    else
        game


getBombPosition : Int -> Grid Face -> Maybe Position
getBombPosition index faces =
    let
        toBombOffset : Position -> Position
        toBombOffset position =
            -- drop bomb from the horizontal center and bottom of the face
            { x = position.x + ((Face.size.width / 2) - (Bullet.size.width / 2))
            , y = position.y + Face.size.height
            }
    in
    List.map (Array.get index << Array.fromList) (List.reverse faces)
        |> List.filter (Maybe.withDefault False << Maybe.map Face.isAlive)
        |> List.head
        |> Maybe.andThen identity
        |> Maybe.map (toBombOffset << .position)


view : Game -> Svg.Svg a
view game =
    let
        faces =
            Grid.toList game.faces
                |> List.filter Face.isAlive
    in
    Svg.svg
        [ Svg.Attributes.height (String.fromFloat Screen.size.height)
        , Html.Attributes.style "border" "3px solid #333"
        , Html.Attributes.style "background-color" "#fafafa"
        , Svg.Attributes.width (String.fromFloat Screen.size.width)
        ]
        [ Hand.toSvg game.hand.position
        , Svg.g [] <| List.map (Face.toSvg << .position) faces
        , Svg.g [] <| List.map (Bullet.toSvg << .position) game.bullets
        ]
