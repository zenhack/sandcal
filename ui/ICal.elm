module ICal exposing (..)

import Iso8601
import Json.Decode as D
import Json.Encode as E
import Time


type alias UID =
    { uidValue : String
    }


decodeUID : D.Decoder UID
decodeUID =
    D.map UID (D.field "uidValue" D.string)


encodeUID : UID -> E.Value
encodeUID uid =
    E.object
        [ ( "uidValue", E.string uid.uidValue )
        ]


type alias DTStamp =
    { dtStampValue : Time.Posix
    , dtStampOther : List E.Value
    }


decodeDTStamp : D.Decoder DTStamp
decodeDTStamp =
    D.map2 DTStamp
        (D.field "dtStampValue" Iso8601.decoder)
        (D.field "dtStampOther" (D.list D.value))


encodeDTStamp : DTStamp -> E.Value
encodeDTStamp dt =
    E.object
        [ ( "dtStampValue", Iso8601.encode dt.dtStampValue )
        , ( "dtStampOther", E.list identity dt.dtStampOther )
        ]


type alias VEvent =
    { veDTStamp : DTStamp
    , veUID : UID
    }


decodeVEvent : D.Decoder VEvent
decodeVEvent =
    D.map2 VEvent
        (D.field "veDTStamp" decodeDTStamp)
        (D.field "veUID" decodeUID)


encodeVEvent : VEvent -> E.Value
encodeVEvent ev =
    E.object
        [ ( "veDTStamp", encodeDTStamp ev.veDTStamp )
        , ( "veUID", encodeUID ev.veUID )
        ]


type alias Summary =
    { summaryValue : String
    }


decodeSummary : D.Decoder Summary
decodeSummary =
    D.map Summary
        (D.field "summaryValue" D.string)
