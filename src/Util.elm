module Util exposing (flip, isJust, isNothing)


flip : (a -> b -> c) -> (b -> a -> c)
flip f x y =
    f y x


isNothing : Maybe a -> Bool
isNothing m =
    m == Nothing


isJust : Maybe a -> Bool
isJust =
    not << isNothing
