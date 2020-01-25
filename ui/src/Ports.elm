port module Ports exposing (grainTitle, syncFrame)


port syncFrame : () -> Cmd msg


port grainTitle : (String -> msg) -> Sub msg
