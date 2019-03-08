module Bullet
    exposing
        ( Bullet
        , animate
        , isDirection
        , isDirectionDown
        , isDirectionUp
        , shootDown
        , shootUp
        , size
        , toSvg
        )

import Screen exposing (Position, Size)
import Svg exposing (Svg)
import Svg.Attributes


type alias Bullet =
    { direction : Direction
    , hits : Int
    , position : Position
    }


type Direction
    = Up
    | Down


shootDown : Position -> Bullet
shootDown position =
    { direction = Down
    , hits = 0
    , position = position
    }


shootUp : Position -> Bullet
shootUp position =
    { direction = Up
    , hits = 0
    , position = position
    }


isDirectionDown : Bullet -> Bool
isDirectionDown =
    isDirection Down


isDirectionUp : Bullet -> Bool
isDirectionUp =
    isDirection Up


isDirection : Direction -> Bullet -> Bool
isDirection direction bullet =
    direction == bullet.direction


height : Float
height =
    6


width : Float
width =
    6


size : Size
size =
    { height = height, width = width }


toSvg : Position -> Svg a
toSvg position =
    Svg.image
        [ Svg.Attributes.xlinkHref bigDataUri
        , Svg.Attributes.width (String.fromFloat width)
        , Svg.Attributes.height (String.fromFloat height)
        , Svg.Attributes.x (String.fromFloat position.x)
        , Svg.Attributes.y (String.fromFloat position.y)
        ]
        []


smallDataUri : String
smallDataUri =
    "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAMAAAADCAYAAABWKLW/AAAABmJLR0QA/wD/AP+gvaeTAAAACXBIWXMAAA3XAAAN1wFCKJt4AAAAB3RJTUUH4wMECSkzYIHreQAAADJJREFUCB0BJwDY/wEzMzP/AAAAAAAAAAACAAAAAAAAAAAAAAAAAgAAAAAAAAAAAAAAADmWAZ5om8s5AAAAAElFTkSuQmCC"


bigDataUri : String
bigDataUri =
    "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAYAAAAGCAYAAADgzO9IAAAC2HpUWHRSYXcgcHJvZmlsZSB0eXBlIGV4aWYAAHja7ZdNkhshDIX3nCJHaEkIiePQ0FTlBjl+HjRujz3OVOVnkYWbMWAZnkCfwJ5w/Pjewzc8lGULUc1TTmnDE3PMXNDx7XzOlrY46/NJq6VHe7g+YJgErazxxxpfYNf7BIvLvj/ag9Wl40vo5nkJyvDM6KxxvoSETzut9yGveSV+2M569dsW7Wye30dDMJpCTzjwISQb6ji8CFYgWQpamXXkYVH046r5dezC1X0K3tV7it1Wll0eQ3EPdnqK0bKTvo7djNDHFdHd88MHJpeLz7HrzXs/zt2VmBCpFNambluZPQzcEUqZ0xKK4aXo2ywZxbHFCmINNHeUGigTI9qdIjUq1OmYbaWKJUY+2NAyV5ZpczHOXCeUOAp1NuBpQRysKqjJIHKthabfPP1VcnhuhJFMECPM+FTCK+OflEuo95G6RJtfscK6eGQNljHIjRqjAIT6iqnO+M4SPuTN9gGsgKDOMDs2WLb9lNiV7rklk7NgnG4xbOfRIGtLACGCb8ViSEBgS0hsSrQZsxEhjg4+BStnpP0OAqTKjUIHG5EEOM7DN+YYzbGsfJpxtcwDksSABgcIsGJU5I9FRw4VFY1BVZOaumYtSVJMmlKyNO6oYmLR1JKZuWUrLh5dPbm5e/aSOQuuMM0pW8iecy4FTgukC2YXjChl5132uOuedtt9z3upSJ8aq9ZUrXrNtTRu0nD8W2oWmrfcykEHUumIhx7psMOPfJSOXOvSY9eeunXvuZeL2qL6SI2eyH1NjRa1QSzOcXanBrPZTYLGdaKDGYhxJBC3QQAJzYPZ5hQjD3KD2ZYZh0IZ1EgHnEaDGAjGg1g7Xezu5L7kFjT+Fjf+Fbkw0P0LcmGgW+Q+c3tBrZX5jSIT0DiFI6abdFxsXbiw4w/38Z+34W8F3kJvobfQW+gt9BZ6C/03QtLx4wH/aoafSmWQ2TH6fzQAAAAGYktHRAD/AP8A/6C9p5MAAAAJcEhZcwAADdcAAA3XAUIom3gAAAAHdElNRQfjAwQOLSyMqjUNAAAAFklEQVQI12M0Njb+z4AFMDHgAPSQAADt/AGkO93DggAAAABJRU5ErkJggg=="


animate : Bullet -> Bullet
animate bullet =
    let
        position =
            bullet.position
    in
    case bullet.direction of
        Up ->
            { bullet | position = { position | y = position.y - 6 } }

        Down ->
            { bullet | position = { position | y = position.y + 6 } }
