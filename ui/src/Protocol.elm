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
    | StartEnd StartEndFields


type alias StartEndFields =
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


decodeEventTime : D.Decoder EventTime
decodeEventTime =
    D.field "tag" D.string
        |> D.andThen
            (\tag ->
                case tag of
                    "AllDay" ->
                        D.succeed AllDay

                    "StartEnd" ->
                        D.map StartEnd <|
                            D.map3 StartEndFields
                                (D.field "startTime" D.string)
                                (D.field "endTime" D.string)
                                (D.field "timeZone" D.string)

                    _ ->
                        D.fail ("Unknown tag: " ++ tag)
            )


type alias NewEvent =
    { summary : String
    , description : String
    , location : String
    , date : String
    , time : EventTime
    , repeats : List Repeat
    }


encodeNewEvent : NewEvent -> E.Value
encodeNewEvent v =
    E.object
        [ ( "summary", E.string v.summary )
        , ( "description", E.string v.description )
        , ( "location", E.string v.location )
        , ( "date", E.string v.date )
        , ( "time", encodeEventTime v.time )
        , ( "repeats", E.list encodeRepeat v.repeats )
        ]


decodeNewEvent : D.Decoder NewEvent
decodeNewEvent =
    D.map6 NewEvent
        (D.field "summary" D.string)
        (D.field "description" D.string)
        (D.field "location" D.string)
        (D.field "date" D.string)
        (D.field "time" decodeEventTime)
        (D.field "repeats" (D.list decodeRepeat))


type alias EditTemplate =
    { title : String
    , submitText : String
    , userTz : Maybe String
    , action : String
    , formData : Maybe NewEvent
    , csrfToken : String
    }


decodeEditTemplate : D.Decoder EditTemplate
decodeEditTemplate =
    D.map6 EditTemplate
        (D.field "title" D.string)
        (D.field "submitText" D.string)
        (D.field "userTz" (D.maybe D.string))
        (D.field "action" D.string)
        (D.field "formData" (D.nullable decodeNewEvent))
        (D.field "csrfToken" D.string)
