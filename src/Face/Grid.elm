module Face.Grid exposing (init, animate)

import Face exposing (Face)
import Grid exposing (Grid)
import Screen exposing (Position)


init : Face.Direction -> Grid Face
init direction =
    let
        spacing =
            10

        rowLength =
            9

        rowWidth =
            rowLength * (Face.width + spacing)

        leftOffset =
            (Screen.width - rowWidth) // 2

        xs =
            List.range 0 (rowLength - 1)
                |> List.map ((*) (Face.width + spacing))
                -- center in screen
                |> List.map ((+) leftOffset)

        ys =
            List.range 0 4
                |> List.map ((*) (Face.height + spacing))
                -- start 6 pixels from the top edge of the screen
                |> List.map ((+) 6)

        toPairs x =
            List.map (Tuple.pair x) ys

        toPosition ( x, y ) =
            { x = x, y = y }

        positions =
            List.map toPairs xs
                |> Grid.map toPosition
    in
        Grid.map (Face.init direction) positions


animate : Grid Face -> Grid Face
animate faces =
    let
        sortedFaces =
            List.concat faces
                |> List.sortBy (.x << .position)

        getPosition =
            Maybe.map .position << List.head

        leftMost =
            getPosition sortedFaces

        rightMost =
            getPosition <| List.reverse sortedFaces

        move lMost rMost =
            moveFaces lMost rMost faces
    in
        Maybe.map2 move leftMost rightMost
            |> Maybe.withDefault faces


moveFaces : Position -> Position -> Grid Face -> Grid Face
moveFaces leftMost rightMost faces =
    let
        x =
            1

        y =
            3

        atLeftEdge =
            (leftMost.x + (negate x)) <= 0

        atRightEdge =
            (rightMost.x + Face.width + x) >= Screen.width

        reverse =
            (increasePositionBy ( x, y ) << reverseDirection)
    in
        if atLeftEdge then
            reverse faces
        else if atRightEdge then
            reverse faces
        else
            increasePositionBy ( x, 0 ) faces


reverseDirection : Grid Face -> Grid Face
reverseDirection faces =
    Grid.map Face.reverseDirection faces


increasePositionBy : ( Int, Int ) -> Grid Face -> Grid Face
increasePositionBy ( x, y ) faces =
    let
        increase : Face -> Face
        increase face =
            let
                position =
                    face.position
            in
                case face.direction of
                    Face.Left ->
                        { face
                            | position =
                                { x = position.x + (negate x)
                                , y = position.y + y
                                }
                        }

                    Face.Right ->
                        { face
                            | position =
                                { position
                                    | x = position.x + x
                                    , y = position.y + y
                                }
                        }
    in
        Grid.map increase faces
