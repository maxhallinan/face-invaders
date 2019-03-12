module Tests exposing (..)

import Bullet exposing (Bullet)
import Expect exposing (Expectation, equal)
import Face exposing (Face)
import Game exposing (Game)
import Grid exposing (Grid)
import Hand exposing (Hand)
import Screen exposing (Position)
import Test exposing (Test, describe, test)


testIsVerticalOverlap : Test
testIsVerticalOverlap =
    describe "Screen.isVerticalOverlap"
        [ test "detects a collision when locations are the same overlaps" <|
            \_ ->
                expectIsVerticalOverlap True ( 1, 1 ) ( 1, 1 )
        , test "detects a collision when location1 overlaps location2" <|
            \_ ->
                expectIsVerticalOverlap True ( 1, 2 ) ( 1, 1 )
        , test "detects a collision when location2 overlaps location1" <|
            \_ ->
                expectIsVerticalOverlap True ( 1, 1 ) ( 1, 2 )
        , test "detects a collision when locations overlap #1" <|
            \_ ->
                expectIsVerticalOverlap False ( 1, 12 ) ( 1, 1 )
        , test "detects a collision when locations overlap #2" <|
            \_ ->
                expectIsVerticalOverlap False ( 1, 1 ) ( 1, 12 )
        , test "detects a collision when locations overlap #3" <|
            \_ ->
                expectIsVerticalOverlap False ( 1, 100 ) ( 1, 1 )
        , test "detects a collision when locations overlap #4" <|
            \_ ->
                expectIsVerticalOverlap False ( 1, 1 ) ( 1, 100 )
        ]


expectIsVerticalOverlap : Bool -> ( Float, Float ) -> ( Float, Float ) -> Expectation
expectIsVerticalOverlap =
    expectCollision Screen.isVerticalOverlap


testIsHorizontalOverlap : Test
testIsHorizontalOverlap =
    describe "Screen.isHorizontalOverlap"
        [ test "detects a collision when locations are the same overlaps" <|
            \_ ->
                expectIsHorizontalOverlap True ( 1, 1 ) ( 1, 1 )
        , test "detects a collision when location1 overlaps location2" <|
            \_ ->
                expectIsHorizontalOverlap True ( 2, 1 ) ( 1, 1 )
        , test "detects a collision when location2 overlaps location1" <|
            \_ ->
                expectIsHorizontalOverlap True ( 1, 1 ) ( 2, 1 )
        , test "does not detect a collision when locations do not overlap" <|
            \_ ->
                expectIsHorizontalOverlap False ( 1, 1 ) ( 100, 1 )
        ]


expectIsHorizontalOverlap : Bool -> ( Float, Float ) -> ( Float, Float ) -> Expectation
expectIsHorizontalOverlap =
    expectCollision Screen.isHorizontalOverlap


testIsCollision : Test
testIsCollision =
    describe "Screen.isCollision"
        [ test "detects a collision when locations are the same" <|
            \_ ->
                expectIsCollision True ( 1, 1 ) ( 1, 1 )
        , test "detects a collision when right edge overlaps" <|
            \_ ->
                expectIsCollision True ( 1, 1 ) ( 9, 1 )
        , test "detects a collision when left edge overlaps" <|
            \_ ->
                expectIsCollision True ( 9, 1 ) ( 1, 1 )
        , test "detects a collision when top edge overlaps" <|
            \_ ->
                expectIsCollision True ( 1, 9 ) ( 1, 1 )
        , test "detects a collision when bottom edge overlaps" <|
            \_ ->
                expectIsCollision True ( 1, 1 ) ( 1, 9 )
        , test "does not detect a collision when positions do not overlap #1" <|
            \_ ->
                expectIsCollision False ( 1, 1 ) ( 100, 100 )
        , test "does not detect a collision when positions do not overlap #2" <|
            \_ ->
                expectIsCollision False ( 1, 1 ) ( 1, 100 )
        , test "does not detect a collision when positions do not overlap #3" <|
            \_ ->
                expectIsCollision False ( 1, 100 ) ( 1, 1 )
        , test "does not detect a collision when positions do not overlap #4" <|
            \_ ->
                expectIsCollision False ( 100, 1 ) ( 1, 1 )
        , test "does not detect a collision when positions do not overlap #5" <|
            \_ ->
                expectIsCollision False ( 1, 1 ) ( 100, 1 )
        ]


expectIsCollision : Bool -> ( Float, Float ) -> ( Float, Float ) -> Expectation
expectIsCollision =
    expectCollision Screen.isCollision


expectCollision : (Screen.Location -> Screen.Location -> a) -> a -> ( Float, Float ) -> ( Float, Float ) -> Expectation
expectCollision toActual expected ( x1, y1 ) ( x2, y2 ) =
    let
        loc1 =
            mockLocation x1 y1

        loc2 =
            mockLocation x2 y2

        actual =
            toActual loc1 loc2
    in
        Expect.equal expected actual


mockLocation : Float -> Float -> Screen.Location
mockLocation x y =
    let
        size =
            { width = 10, height = 10 }
    in
        { size = size, position = { x = x, y = y } }


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
    expectFace Face.isDead facePos bulletPos


expectAliveFace : Position -> Position -> Expectation
expectAliveFace facePos bulletPos =
    expectFace Face.isAlive facePos bulletPos


expectFace : (Face -> Bool) -> Position -> Position -> Expectation
expectFace predicate facePos bulletPos =
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
            |> Expect.true "Expected the face to be dead"
