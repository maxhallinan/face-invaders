module Game
    exposing
        ( Game
        , animate
        , applyLogic
        , countBulletHits
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
import Html exposing (Html)
import Html.Attributes
import Screen exposing (Location, Position, Size)
import String
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


applyLogic : Game -> Game
applyLogic =
    countBulletHits
        >> detectDeadFaces
        >> detectDeadHand
        >> collectGarbage


detectDeadHand : Game -> Game
detectDeadHand =
    detectHandHitByBullet
        >> detectHandHitByFace
        >> detectFaceTouchGround


detectHandHitByFace : Game -> Game
detectHandHitByFace game =
    let
        faceLocations =
            Grid.toList game.faces
                |> toFaceLocations

        hand =
            detectHandHit faceLocations game.hand
    in
        { game | hand = hand }


detectHandHitByBullet : Game -> Game
detectHandHitByBullet game =
    let
        bulletLocations =
            List.filter Bullet.isDirectionDown game.bullets
                |> toBulletLocations

        hand =
            detectHandHit bulletLocations game.hand
    in
        { game | hand = hand }


detectFaceTouchGround : Game -> Game
detectFaceTouchGround game =
    let
        faceLocations =
            Grid.toList game.faces |> toFaceLocations

        hand =
            game.hand
    in
        if List.any Screen.isOffScreenBottom faceLocations then
            { game | hand = { hand | health = Hand.Dead } }
        else
            game


toBulletLocations : List Bullet -> List Location
toBulletLocations =
    toLocation Bullet.size


toFaceLocations : List Face -> List Location
toFaceLocations =
    toLocation Face.size


toLocation : Size -> List { a | position : Position } -> List Location
toLocation size xs =
    List.map (Screen.toLocation size << .position) xs


detectHandHit : List Location -> Hand -> Hand
detectHandHit locations hand =
    let
        handLocation =
            Hand.toLocation hand.position

        detectHit : Location -> Hand -> Hand
        detectHit location h =
            if Screen.isCollision location handLocation then
                { h | health = Hand.Dead }
            else
                h
    in
        List.foldl detectHit hand locations


countBulletHits : Game -> Game
countBulletHits =
    countBulletFaceHits >> countBulletHandHits


countBulletHandHits : Game -> Game
countBulletHandHits game =
    let
        handLocation =
            Hand.toLocation game.hand.position
    in
        { game | bullets = List.map (countHit [ handLocation ] Bullet.Down) game.bullets }


countBulletFaceHits : Game -> Game
countBulletFaceHits game =
    let
        faceLocations =
            Grid.filter (not << Face.isDead) game.faces
                |> Grid.map (Face.toLocation << .position)
                |> Grid.toList
    in
        { game | bullets = List.map (countHit faceLocations Bullet.Up) game.bullets }


countHit : List Location -> Bullet.Direction -> Bullet -> Bullet
countHit locations direction bullet =
    let
        bulletLocation =
            Bullet.toLocation bullet.position

        isCollision =
            List.any (Screen.isCollision bulletLocation) locations
    in
        if isCollision && direction == bullet.direction then
            { bullet | hits = bullet.hits + 1 }
        else
            bullet


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
    let
        bulletLocation =
            Bullet.toLocation bullet.position

        faceLocation =
            Face.toLocation face.position
    in
        Screen.isCollision bulletLocation faceLocation


collectGarbage : Game -> Game
collectGarbage game =
    { game
        | bullets = List.filter (not << isBulletGarbage) game.bullets
    }


isBulletGarbage : Bullet -> Bool
isBulletGarbage bullet =
    Screen.isOffScreen bullet.position || bullet.hits >= 1


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


toScore : Game -> Int
toScore game =
    Grid.filter Face.isDead game.faces
        |> Grid.toList
        |> List.length


view : Game -> Html a
view game =
    Html.div
        []
        [ screenView game
        , scoreView game
        ]


screenView : Game -> Html a
screenView game =
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


scoreView : Game -> Html a
scoreView game =
    let
        score =
            String.join
                " "
                [ "Score:"
                , String.fromInt <| toScore game
                ]
    in
        Html.p [] [ Html.text score ]
