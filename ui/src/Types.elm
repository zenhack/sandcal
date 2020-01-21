module Types exposing (Event, decodeEvent)

import Json.Decode as D
import Json.Encode as E
import Time


type alias Event =
    { summary : String
    , start : Time.Posix
    , end : Time.Posix
    }


decodeEvent : D.Decoder Event
decodeEvent =
    D.map3 Event
        (D.field "summary" D.string)
        (D.field "start" decodeTime)
        (D.field "end" decodeTime)


decodeTime : D.Decoder Time.Posix
decodeTime =
    let
        secondsToPosix n =
            Time.millisToPosix (n * 1000)
    in
    D.map secondsToPosix D.int


encodeEvent : Event -> E.Value
encodeEvent { summary, start, end } =
    E.object
        [ ( "summary", E.string summary )
        , ( "start", encodeTime start )
        , ( "end", encodeTime end )
        ]


encodeTime : Time.Posix -> E.Value
encodeTime t =
    E.int (Time.posixToMillis t // 1000)
