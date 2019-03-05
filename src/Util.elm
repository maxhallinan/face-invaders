module Util exposing (decodeKey, flip, isJust, isNothing)

import Json.Decode


flip : (a -> b -> c) -> (b -> a -> c)
flip f x y =
    f y x


isNothing : Maybe a -> Bool
isNothing m =
    m == Nothing


isJust : Maybe a -> Bool
isJust =
    not << isNothing


decodeKey : (String -> a) -> Json.Decode.Decoder a
decodeKey toKey =
    Json.Decode.field "key" Json.Decode.string
        |> Json.Decode.map toKey
