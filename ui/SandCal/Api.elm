module SandCal.Api exposing
    ( addEvent
    , allEvents
    , getEvent
    , getTimeZone
    , importICalendar
    , setTimeZone
    )

import File exposing (File)
import Http
import Json.Decode as D
import SandCal.Types as Types
import TimeZone


allEvents : (Result Http.Error (List Types.Event) -> msg) -> Cmd msg
allEvents mkMsg =
    Http.get
        { url = "/api/all-events.json"
        , expect = Http.expectJson mkMsg (D.list Types.decodeEvent)
        }


addEvent : (Result Http.Error String -> msg) -> Types.Event -> Cmd msg
addEvent mkMsg event =
    Http.post
        { url = "/api/event/new"
        , body = Http.jsonBody (Types.encodeEvent event)
        , expect = Http.expectJson mkMsg D.string
        }


getEvent : (Result Http.Error Types.Event -> msg) -> Int -> Cmd msg
getEvent mkMsg id =
    Http.get
        { url = "/api/event/" ++ String.fromInt id
        , expect = Http.expectJson mkMsg Types.decodeEvent
        }


importICalendar : (Result Http.Error () -> msg) -> File -> Cmd msg
importICalendar mkMsg file =
    Http.post
        { url = "/api/import.ics"
        , body = Http.fileBody file
        , expect = Http.expectWhatever mkMsg
        }


getTimeZone : (Result Http.Error TimeZone.Label -> msg) -> Cmd msg
getTimeZone mkMsg =
    Http.get
        { url = "/api/timezone"
        , expect = Http.expectJson mkMsg TimeZone.decodeLabel
        }


setTimeZone : TimeZone.Label -> (Result Http.Error () -> msg) -> Cmd msg
setTimeZone label mkMsg =
    Http.post
        { url = "/api/timezone"
        , body = Http.jsonBody (TimeZone.encodeLabel label)
        , expect = Http.expectWhatever mkMsg
        }
