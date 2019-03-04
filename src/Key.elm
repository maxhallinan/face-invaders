module Key exposing (Key(..), decode)

import Json.Decode


type Key
    = Left
    | Right
    | Space
    | Enter
    | Other


decode : (Key -> a) -> Json.Decode.Decoder a
decode msg =
    Json.Decode.field "key" Json.Decode.string
        |> Json.Decode.map (msg << toKey)


toKey : String -> Key
toKey k =
    case k of
        "ArrowLeft" ->
            Left

        "ArrowRight" ->
            Right

        "Spacebar" ->
            Space

        " " ->
            Space

        "Enter" ->
            Enter

        _ ->
            Other
