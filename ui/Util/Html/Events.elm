module Util.Html.Events exposing (onChange)

import Html
import Html.Events exposing (on)
import Json.Decode as D


onChange : (a -> msg) -> D.Decoder a -> Html.Attribute msg
onChange toMsg decoder =
    on "change" (D.map toMsg (D.field "target" (D.field "value" decoder)))
