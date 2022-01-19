module FormValues exposing
    ( Accessor
    , Flags
    , Model
    , Msg(..)
    , Range
    , RepeatOption
    , chooseTz
    , decodeFlags
    , init
    , makeProtocolNewEvent
    , repeatOptions
    , valid
    )

import Accessors
import GenAccessors as GA
import Http
import Json.Decode as D
import List.Extra
import Protocol
import Protocol.Rpc as Rpc


type alias Range a =
    { start : a
    , stop : a
    }


type alias RepeatOption =
    { freq : String
    , noun : String
    }


repeatOptions =
    [ { freq = "Daily", noun = "day" }
    , { freq = "Weekly", noun = "week" }
    , { freq = "Monthly", noun = "month" }
    , { freq = "Yearly", noun = "year" }
    ]


type alias Model =
    { summary : String
    , date : String
    , description : String
    , location : String
    , allDay : Bool

    {- N.B. it would be natural to make time a type like:

       type Time = AllDay | PartOfDay (Range String)

       ...since time isn't meaningful for all day events, but we don't do
       that, because if the user checks 'All Day' and then unchecks it, we
       want the settings to revert to what they were before the check -- which
       means we can't forget about them.

       So, `time` isn't meaningful if `all_day = true` from the perspective of
       the specified event, but we track it anyway for UI purposes.
    -}
    , time : Range String
    , timeZone : String
    , repeat : List Protocol.Repeat
    }


type alias Accessor sub super =
    Accessors.Relation sub sub sub -> Accessors.Relation super sub sub


type Msg
    = InputChanged (Accessor String Model) String
    | SetAllDay Bool
    | NewRepeat
    | DeleteRepeat Int
    | Submit
    | SubmitResult (Result Http.Error String)


type alias Flags =
    { tpl : Protocol.EditTemplate
    , browserTz : String
    , now : Date
    }


type alias Date =
    { year : Int
    , month : Int
    , day : Int
    }


decodeFlags : D.Decoder Flags
decodeFlags =
    D.map3 Flags
        (D.field "tpl" Protocol.decodeEditTemplate)
        (D.field "browserTz" D.string)
        (D.field "now" decodeDate)


decodeDate : D.Decoder Date
decodeDate =
    D.map3 Date
        (D.field "year" D.int)
        (D.field "month" D.int)
        (D.field "day" D.int)


formatDate : Date -> String
formatDate d =
    let
        str n =
            let
                ret =
                    String.fromInt n
            in
            if n < 10 then
                "0" ++ ret

            else
                ret
    in
    str d.year ++ "-" ++ str (d.month + 1) ++ "-" ++ str d.day


valid m =
    let
        mem s =
            s /= ""
    in
    mem m.summary
        && mem m.date
        && (m.allDay || (mem m.time.start && mem m.time.stop))


makeProtocolNewEvent fv =
    { summary = fv.summary
    , date = fv.date
    , time =
        if fv.allDay then
            Protocol.AllDay

        else
            Protocol.StartEnd
                { startTime = fv.time.start
                , endTime = fv.time.stop
                , timeZone = fv.timeZone
                }
    , description = fv.description
    , location = fv.location
    , repeats = fv.repeat
    }


chooseTz : Flags -> String
chooseTz flags =
    flags.tpl.userTz |> Maybe.withDefault flags.browserTz


init flags =
    let
        userTz =
            chooseTz flags
    in
    let
        defaultTime =
            { start = "12:00"
            , stop = "13:00"
            }
    in
    case flags.tpl.formData of
        Nothing ->
            { summary = ""
            , description = ""
            , location = ""
            , date = formatDate flags.now
            , allDay =
                case flags.tpl.formData of
                    Nothing ->
                        False

                    Just ev ->
                        case ev.time of
                            Protocol.AllDay ->
                                True

                            Protocol.StartEnd _ ->
                                False
            , time = defaultTime
            , timeZone = chooseTz flags
            , repeat = []
            }

        Just fd ->
            let
                timeDependent =
                    case fd.time of
                        Protocol.AllDay ->
                            { allDay = True
                            , time = defaultTime
                            , timeZone = userTz
                            }

                        Protocol.StartEnd { startTime, endTime, timeZone } ->
                            { allDay = False
                            , time = { start = startTime, stop = endTime }
                            , timeZone = timeZone
                            }
            in
            { summary = fd.summary
            , description = fd.description
            , location = fd.location
            , date = fd.date
            , allDay = timeDependent.allDay
            , time = timeDependent.time
            , timeZone = timeDependent.timeZone
            , repeat = fd.repeats
            }
