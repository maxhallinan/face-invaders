module Screen
    exposing
        ( Location
        , Position
        , Size
        , isCollision
        , isVerticalOverlap
        , isHorizontalOverlap
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
    (isHorizontalOverlap loc1 loc2)
        && (isVerticalOverlap loc1 loc2)


isHorizontalOverlap : Location -> Location -> Bool
isHorizontalOverlap loc1 loc2 =
    let
        leftEdge1 =
            loc1.position.x

        leftEdge2 =
            loc2.position.x

        rightEdge1 =
            leftEdge1 + loc1.size.width

        rightEdge2 =
            leftEdge2 + loc2.size.width
    in
        (leftEdge1 >= leftEdge2 && leftEdge1 <= rightEdge2)
            || (leftEdge2 >= leftEdge1 && leftEdge2 <= rightEdge1)


isVerticalOverlap : Location -> Location -> Bool
isVerticalOverlap loc1 loc2 =
    let
        top1 =
            loc1.position.y

        top2 =
            loc2.position.y

        bottom1 =
            top1 + loc1.size.height

        bottom2 =
            top2 + loc2.size.height
    in
        (top1 >= top2 && top1 <= bottom2)
            || (top2 >= top1 && top2 <= bottom1)
