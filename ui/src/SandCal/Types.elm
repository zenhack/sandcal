module SandCal.Types exposing (Event, decodeEvent, encodeEvent)

import Json.Decode as D
import Json.Encode as E
import Time


type Frequency
    = Secondly
    | Minutely
    | Hourly
    | Daily
    | Weekly
    | Monthly
    | Yearly


type alias Event =
    { summary : String
    , start : Time.Posix
    , end : Time.Posix
    , recurs : List Recur
    }


type alias Recur =
    { frequency : Frequency
    , until : Maybe Time.Posix
    }


decodeFrequency =
    D.string
        |> D.andThen
            (\s ->
                case s of
                    "Secondly" ->
                        D.succeed Secondly

                    "Minutely" ->
                        D.succeed Minutely

                    "Hourly" ->
                        D.succeed Hourly

                    "Daily" ->
                        D.succeed Daily

                    "Weekly" ->
                        D.succeed Weekly

                    "Monthly" ->
                        D.succeed Monthly

                    "Yearly" ->
                        D.succeed Yearly

                    _ ->
                        D.fail "Unrecognized frequency"
            )


encodeFrequency : Frequency -> E.Value
encodeFrequency freq =
    E.string <|
        case freq of
            Secondly ->
                "Secondly"

            Minutely ->
                "Minutely"

            Hourly ->
                "Hourly"

            Daily ->
                "Daily"

            Weekly ->
                "Weekly"

            Monthly ->
                "Monthly"

            Yearly ->
                "Yearly"


encodeRecur : Recur -> E.Value
encodeRecur { frequency, until } =
    E.object
        [ ( "frequency", encodeFrequency frequency )
        , ( "until"
          , Maybe.map encodeTime until
                |> Maybe.withDefault E.null
          )
        ]


decodeRecur : D.Decoder Recur
decodeRecur =
    D.map2 Recur
        (D.field "frequency" decodeFrequency)
        (D.maybe (D.field "until" decodeTime))


decodeEvent : D.Decoder Event
decodeEvent =
    D.map4 Event
        (D.field "summary" D.string)
        (D.field "start" decodeTime)
        (D.field "end" decodeTime)
        (D.field "recurs" <| D.list decodeRecur)


decodeTime : D.Decoder Time.Posix
decodeTime =
    let
        secondsToPosix n =
            Time.millisToPosix (n * 1000)
    in
    D.map secondsToPosix D.int


encodeEvent : Event -> E.Value
encodeEvent { summary, start, end, recurs } =
    E.object
        [ ( "summary", E.string summary )
        , ( "start", encodeTime start )
        , ( "end", encodeTime end )
        , ( "recurs", E.list encodeRecur recurs )
        ]


encodeTime : Time.Posix -> E.Value
encodeTime t =
    E.int (Time.posixToMillis t // 1000)
