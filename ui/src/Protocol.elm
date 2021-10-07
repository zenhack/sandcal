module Protocol exposing (Frequency)

import Json.Decode as D
import Json.Encode as E



-- TODO: make this a union:


type alias Frequency =
    String


encodeFrequency : Frequency -> E.Value
encodeFrequency =
    E.string


decodeFrequency : D.Decoder Frequency
decodeFrequency =
    D.string


type alias Repeat =
    { frequency : Frequency
    , interval : Int
    }


encodeRepeat : Repeat -> E.Value
encodeRepeat r =
    E.object
        [ ( "frequency", encodeFrequency r.frequency )
        , ( "interval", E.int r.interval )
        ]


decodeRepeat : D.Decoder Repeat
decodeRepeat =
    D.map2 Repeat
        (D.field "frequency" decodeFrequency)
        (D.field "interval" D.int)


type EventTime
    = AllDay
    | StartEnd
        { startTime : String
        , endTime : String
        , timeZone : String
        }


encodeEventTime : EventTime -> E.Value
encodeEventTime t =
    case t of
        AllDay ->
            E.object [ ( "tag", E.string "AllDay" ) ]

        StartEnd { startTime, endTime, timeZone } ->
            E.object
                [ ( "tag", E.string "StartEnd" )
                , ( "startTime", E.string startTime )
                , ( "endTime", E.string endTime )
                , ( "timeZone", E.string timeZone )
                ]
