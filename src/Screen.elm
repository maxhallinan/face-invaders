module Screen
    exposing
        ( Location
        , Position
        , Size
        , isCollision
        , isOffScreen
        , isOffScreenBottom
        , size
        , toLocation
        )


type alias Location =
    { position : Position, size : Size }


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


toLocation : Size -> Position -> Location
toLocation s p =
    { position = p, size = s }


isOffScreen : Position -> Bool
isOffScreen { x, y } =
    x <= 0 || x >= width || y <= 0 || y >= height


isOffScreenBottom : Location -> Bool
isOffScreenBottom location =
    (location.position.y >= height)
        || (location.position.y + location.size.height >= height)


isCollision : Location -> Location -> Bool
isCollision loc1 loc2 =
    let
        s1 =
            loc1.size

        p1 =
            loc1.position

        s2 =
            loc2.size

        p2 =
            loc2.position

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


isCollisionLeft : Location -> Location -> Bool
isCollisionLeft loc1 loc2 =
    -- location1 is within the area of location2 when the x coordinate is >= the left edge and <= the right edge
    (loc1.position.x >= loc2.position.x)
        && (loc1.position.x <= loc2.position.x + loc2.size.width)


isCollisionTop : Location -> Location -> Bool
isCollisionTop loc1 loc2 =
    -- location1 is within the area of location2 when the y coordinate is >= the top edge and <= the bottom edge
    (loc1.position.y >= loc2.position.y)
        && (loc1.position.y <= loc2.position.y + loc2.size.height)


isCollisionBottom : Location -> Location -> Bool
isCollisionBottom loc1 loc2 =
    True


isCollisionRight : Location -> Location -> Bool
isCollisionRight loc1 loc2 =
    True
