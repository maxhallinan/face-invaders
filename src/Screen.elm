module Screen
    exposing
        ( Position
        , Size
        , isCollision
        , isOffScreen
        , isOffScreenBottom
        , size
        )


type alias Position =
    { x : Float, y : Float }


type alias Size =
    { height : Float, width : Float }


size : Size
size =
    { height = height, width = width }


height : Float
height =
    512


width : Float
width =
    704


isOffScreen : Position -> Bool
isOffScreen { x, y } =
    x <= 0 || x >= width || y <= 0 || y >= height


isOffScreenBottom : ( Size, Position ) -> Bool
isOffScreenBottom ( s, p ) =
    p.y >= height || (p.y + s.height) >= height


isCollision : ( Size, Position ) -> ( Size, Position ) -> Bool
isCollision ( s1, p1 ) ( s2, p2 ) =
    let
        l1 =
            p1.x

        l2 =
            p2.x

        r1 =
            p1.x + s1.width

        r2 =
            p2.x + s2.width

        t1 =
            p1.y

        t2 =
            p2.y

        b1 =
            p1.y + s1.height

        b2 =
            p2.y + s2.height
    in
    ((l1 >= l2 && l1 <= r2) || (r1 >= l2 && r1 <= r2)) && ((t1 >= t2 && t1 <= b2) || (b1 <= t2 && b1 >= b2))
