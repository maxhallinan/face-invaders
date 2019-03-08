module Grid exposing (Grid, filter, foldl, map, toList)

import Array


type alias Grid a =
    List (List a)


map : (a -> b) -> Grid a -> Grid b
map f =
    List.map (List.map f)


foldl : (a -> b -> b) -> b -> Grid a -> b
foldl f x grid =
    toList grid
        |> List.foldl f x


filter : (a -> Bool) -> Grid a -> Grid a
filter f grid =
    List.map (List.filter f) grid


toList : Grid a -> List a
toList =
    List.concat
