module Grid exposing (Grid, map)


type alias Grid a =
    List (List a)


map : (a -> b) -> Grid a -> Grid b
map f =
    List.map (List.map f)
