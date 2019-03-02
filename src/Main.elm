module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown)
import Html
import Json.Decode
import String
import Svg
import Svg.Attributes
import Tuple


type alias Hand =
    { position : Position }


type alias Face =
    { position : Position
    , direction : FaceDirection
    }


type FaceDirection
    = FaceLeft
    | FaceRight


type alias Bullet =
    { position : Position }


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


type alias Position =
    { x : Int, y : Int }


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
      , faces = initFaces
      , hand = initHand
      }
    , Cmd.none
    )


initHand : Hand
initHand =
    let
        x =
            -- horizontal center
            (screenWidth // 2) - (handWidth // 2)

        y =
            -- near the bottom of the screen
            screenHeight - (handHeight + 6)
    in
        { position = { x = x, y = y }
        }


initFaces : Grid Face
initFaces =
    let
        initFace position =
            { position = position
            , direction = FaceRight
            }
    in
        mapGrid initFace initFacePositions


initFacePositions : Grid Position
initFacePositions =
    let
        spacing =
            10

        rowLength =
            9

        rowWidth =
            rowLength * (faceWidth + spacing)

        leftOffset =
            (screenWidth - rowWidth) // 2

        xs =
            List.range 0 (rowLength - 1)
                |> List.map ((*) (faceWidth + spacing))
                -- center in screen
                |> List.map ((+) leftOffset)

        ys =
            List.range 0 4
                |> List.map ((*) (faceHeight + spacing))
                -- start 6 pixels from the top edge of the screen
                |> List.map ((+) 6)

        toPairs x =
            List.map (Tuple.pair x) ys

        positions =
            List.map toPairs xs
                |> mapGrid toPosition

        toPosition ( x, y ) =
            { x = x, y = y }
    in
        positions


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
        moveBy =
            15
    in
        case key of
            Left ->
                ( { model | hand = moveHand (negate moveBy) model.hand }
                , Cmd.none
                )

            Right ->
                ( { model | hand = moveHand moveBy model.hand }
                , Cmd.none
                )

            Other ->
                ( model, Cmd.none )


handleAnimate : Model -> ( Model, Cmd Msg )
handleAnimate model =
    let
        sortedFaces =
            List.concat model.faces
                |> List.sortBy (.x << .position)

        getPosition =
            Maybe.map .position << List.head

        leftMost =
            getPosition sortedFaces

        rightMost =
            getPosition <| List.reverse sortedFaces

        faces =
            Maybe.map2 (\l r -> animateFaces l r model.faces) leftMost rightMost
                |> Maybe.withDefault model.faces
    in
        ( { model | faces = faces }, Cmd.none )


animateFaces : Position -> Position -> Grid Face -> Grid Face
animateFaces leftMost rightMost faces =
    let
        incX =
            2

        incY =
            6

        atLeftEdge =
            (leftMost.x + (negate incX)) <= 0

        atRightEdge =
            (rightMost.x + faceWidth + incX) >= screenWidth
    in
        if atLeftEdge then
            reverseFaceDirection faces
                |> moveFacesY incY
                |> moveFacesX incX
        else if atRightEdge then
            reverseFaceDirection faces
                |> moveFacesY incY
                |> moveFacesX incX
        else
            moveFacesX incX faces


reverseFaceDirection : Grid Face -> Grid Face
reverseFaceDirection faces =
    let
        reverseDirection : Face -> Face
        reverseDirection face =
            case face.direction of
                FaceLeft ->
                    { face | direction = FaceRight }

                FaceRight ->
                    { face | direction = FaceLeft }
    in
        mapGrid reverseDirection faces


moveFacesX : Int -> Grid Face -> Grid Face
moveFacesX increase faces =
    let
        increaseX face =
            let
                position =
                    face.position
            in
                case face.direction of
                    FaceLeft ->
                        { face | position = { position | x = position.x + (negate increase) } }

                    FaceRight ->
                        { face | position = { position | x = position.x + increase } }
    in
        mapGrid increaseX faces


moveFacesY : Int -> Grid Face -> Grid Face
moveFacesY increase faces =
    let
        increaseY face =
            let
                position =
                    face.position
            in
                { face | position = { position | y = position.y + increase } }
    in
        mapGrid increaseY faces


moveHand : Int -> Hand -> Hand
moveHand moveBy hand =
    let
        leftEdge =
            0

        rightEdge =
            screenWidth - handWidth

        next =
            hand.position.x + moveBy

        x =
            if next <= leftEdge then
                leftEdge
            else if next >= rightEdge then
                rightEdge
            else
                next

        position =
            { x = x
            , y = hand.position.y
            }
    in
        { hand | position = position }


view : Model -> Html.Html Msg
view model =
    screen model


screenHeight : Int
screenHeight =
    512


screenWidth : Int
screenWidth =
    704


handHeight : Int
handHeight =
    51


handWidth : Int
handWidth =
    45


faceWidth : Int
faceWidth =
    48


faceHeight : Int
faceHeight =
    51


pixelSize : Int
pixelSize =
    3


screen : Model -> Html.Html Msg
screen model =
    Svg.svg
        [ Svg.Attributes.style "background-color: #f1f1f1; border: 3px solid #333"
        , Svg.Attributes.height (String.fromInt screenHeight)
        , Svg.Attributes.width (String.fromInt screenWidth)
        ]
        [ handView model.hand.position
        , Svg.g [] <| List.map (faceView << .position) (List.concat model.faces)
        ]


handView : Position -> Html.Html Msg
handView =
    Svg.g [] << spriteGraphic handSprite


faceView : Position -> Html.Html Msg
faceView =
    Svg.g [] << spriteGraphic faceSprite


bulletView : Position -> Html.Html Msg
bulletView =
    Svg.g [] << spriteGraphic bulletSprite



-- Sprites


type alias Grid a =
    List (List a)


mapGrid : (a -> b) -> Grid a -> Grid b
mapGrid f =
    List.map (List.map f)


type alias Sprite =
    Grid Pixel


type Pixel
    = Empty
    | White
    | Gray
    | Black


intToPixel : Int -> Pixel
intToPixel int =
    case int of
        0 ->
            Empty

        1 ->
            Black

        2 ->
            Gray

        3 ->
            White

        _ ->
            Empty


pixelGrid : Grid Int -> Grid Pixel
pixelGrid =
    mapGrid intToPixel


handSprite : Sprite
handSprite =
    pixelGrid
        [ [ 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0 ]
        , [ 0, 0, 0, 0, 1, 3, 3, 1, 0, 0, 0, 0, 0, 0, 0 ]
        , [ 0, 0, 0, 0, 1, 3, 3, 1, 0, 0, 0, 0, 0, 0, 0 ]
        , [ 0, 0, 0, 0, 1, 3, 3, 1, 0, 0, 0, 0, 0, 0, 0 ]
        , [ 0, 0, 0, 0, 1, 3, 3, 1, 1, 1, 1, 1, 0, 0, 0 ]
        , [ 0, 0, 0, 0, 1, 3, 3, 1, 3, 3, 1, 3, 1, 1, 0 ]
        , [ 0, 0, 0, 0, 1, 3, 3, 1, 3, 3, 1, 3, 1, 3, 1 ]
        , [ 0, 1, 1, 0, 1, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1 ]
        , [ 1, 3, 3, 1, 1, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1 ]
        , [ 1, 3, 3, 3, 1, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1 ]
        , [ 0, 1, 3, 3, 3, 3, 3, 1, 3, 1, 3, 1, 3, 3, 1 ]
        , [ 0, 0, 1, 3, 3, 3, 3, 1, 3, 1, 3, 1, 3, 3, 1 ]
        , [ 0, 0, 1, 3, 3, 3, 3, 1, 3, 1, 3, 1, 3, 1, 0 ]
        , [ 0, 0, 0, 1, 3, 3, 3, 1, 3, 1, 3, 1, 3, 1, 0 ]
        , [ 0, 0, 0, 0, 1, 3, 3, 3, 3, 3, 3, 3, 1, 0, 0 ]
        , [ 0, 0, 0, 0, 0, 1, 3, 3, 3, 3, 1, 3, 1, 0, 0 ]
        , [ 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 1, 1, 0, 0 ]
        ]


faceSprite : Sprite
faceSprite =
    pixelGrid
        [ [ 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0 ]
        , [ 0, 0, 1, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 1, 0, 0 ]
        , [ 0, 1, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 1, 0 ]
        , [ 0, 1, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 1, 0 ]
        , [ 0, 1, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 1, 0 ]
        , [ 1, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 1 ]
        , [ 1, 3, 3, 3, 3, 3, 1, 3, 3, 1, 3, 3, 3, 3, 2, 1 ]
        , [ 1, 3, 3, 3, 3, 3, 1, 3, 3, 1, 3, 3, 3, 3, 2, 1 ]
        , [ 1, 3, 3, 3, 3, 3, 1, 3, 3, 1, 3, 3, 3, 3, 2, 1 ]
        , [ 1, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 1 ]
        , [ 1, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 1 ]
        , [ 1, 3, 3, 3, 3, 1, 3, 3, 3, 3, 1, 3, 3, 2, 2, 1 ]
        , [ 0, 1, 3, 3, 3, 3, 1, 1, 1, 1, 3, 3, 3, 2, 1, 0 ]
        , [ 0, 1, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 1, 0 ]
        , [ 0, 1, 2, 3, 3, 3, 3, 2, 2, 3, 3, 3, 2, 2, 1, 0 ]
        , [ 0, 0, 1, 2, 2, 2, 2, 1, 1, 2, 2, 2, 2, 1, 0, 0 ]
        , [ 0, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 0 ]
        ]


bulletSprite : Sprite
bulletSprite =
    mapGrid intToPixel
        [ [ 1 ] ]


type alias Graphic =
    List (Svg.Svg Msg)


spriteGraphic : Sprite -> Position -> Graphic
spriteGraphic sprite position =
    let
        render : List Pixel -> ( Graphic, Position ) -> ( Graphic, Position )
        render pixelRow ( pixels, { x, y } ) =
            let
                row =
                    renderRow pixelRow { x = x, y = y }
            in
                ( List.append row pixels
                , { x = x, y = y + pixelSize }
                )

        ( svg, _ ) =
            List.foldl render ( [], position ) sprite
    in
        svg


renderRow : List Pixel -> Position -> Graphic
renderRow row position =
    let
        render : Pixel -> ( Graphic, Position ) -> ( Graphic, Position )
        render pixel ( svg, { x, y } ) =
            let
                nextPosition =
                    { x = x + pixelSize, y = y }
            in
                case pixel of
                    Empty ->
                        ( svg, nextPosition )

                    _ ->
                        ( renderPixel pixel { x = x, y = y } :: svg
                        , nextPosition
                        )

        ( graphic, _ ) =
            List.foldl render ( [], position ) row
    in
        graphic


renderPixel : Pixel -> Position -> Svg.Svg Msg
renderPixel pixel { x, y } =
    Svg.rect
        [ Svg.Attributes.fill (pixelColor pixel)
        , Svg.Attributes.height (String.fromInt pixelSize)
        , Svg.Attributes.width (String.fromInt pixelSize)
        , Svg.Attributes.x (String.fromInt x)
        , Svg.Attributes.y (String.fromInt y)
        ]
        []


pixelColor : Pixel -> String
pixelColor pixel =
    case pixel of
        Empty ->
            "rgba(255,255,255,0)"

        White ->
            "rgba(255,255,255,1)"

        Gray ->
            "rgba(203,203,203,1)"

        Black ->
            "rgba(51,51,51,1)"


flip : (a -> b -> c) -> (b -> a -> c)
flip f x y =
    f y x
