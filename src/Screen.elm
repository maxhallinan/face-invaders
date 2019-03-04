module Screen
    exposing
        ( Position
        , height
        , isOffScreen
        , width
        )


type alias Position =
    { x : Float, y : Float }


height : Float
height =
    512


width : Float
width =
    704


isOffScreen : Position -> Bool
isOffScreen { x, y } =
    x <= 0 || x >= width || y <= 0 || y >= height
