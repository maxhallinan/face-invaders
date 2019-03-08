module Tests exposing (..)

import Bullet exposing (Bullet)
import Expect exposing (Expectation, equal)
import Face exposing (Face)
import Game exposing (Game)
import Grid exposing (Grid)
import Hand exposing (Hand)
import Screen exposing (Position)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Game" [ testDetectDeadFaces ]



-- Tests


testDetectDeadFaces : Test
testDetectDeadFaces =
    describe "Game.detectDeadFaces"
        [ test "a face is dead when face.position and bullet.position are equal" <|
            \_ ->
                let
                    position =
                        { x = 5, y = 5 }
                in
                expectDeadFace position position
        , test "a face is dead when face.position and bullet.position overlap" <|
            \_ ->
                let
                    facePos =
                        { x = 5, y = 5 }

                    bulletPos =
                        { x = facePos.x + (Face.size.width - 1)
                        , y = facePos.y + (Face.size.height - 1)
                        }
                in
                expectDeadFace facePos bulletPos
        , test "a face is not dead when face.position and bullet.position do not overlap" <|
            \_ ->
                let
                    facePos =
                        { x = 5, y = 5 }

                    bulletPos =
                        { x = facePos.x + (Face.size.width + 1)
                        , y = facePos.y + (Face.size.height + 1)
                        }
                in
                expectAliveFace facePos bulletPos
        ]


expectDeadFace : Position -> Position -> Expectation
expectDeadFace facePos bulletPos =
    isFace Face.isDead facePos bulletPos
        |> Expect.true "Expected the face to be dead"


expectAliveFace : Position -> Position -> Expectation
expectAliveFace facePos bulletPos =
    isFace Face.isAlive facePos bulletPos
        |> Expect.true "Expected the face to be dead"


isFace : (Face -> Bool) -> Position -> Position -> Bool
isFace predicate facePos bulletPos =
    let
        face =
            Face.init Face.Right facePos

        bullet =
            Bullet.shootUp bulletPos

        game1 =
            { faces = [ [ face ] ]
            , bullets = [ bullet ]
            , hand = Hand.init
            }

        game2 =
            Game.detectDeadFaces game1
    in
    List.head game2.faces
        |> Maybe.andThen List.head
        |> Maybe.map predicate
        |> Maybe.withDefault False
