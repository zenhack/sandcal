module TimeZone exposing
    ( Label
    , decodeLabel
    , encodeLabel
    , labelToString
    )

import Json.Decode as D
import Json.Encode as E


type Label
    = Label String


decodeLabel : D.Decoder Label
decodeLabel =
    D.map Label D.string


encodeLabel : Label -> E.Value
encodeLabel (Label s) =
    E.string s


labelToString : Label -> String
labelToString (Label s) =
    s
