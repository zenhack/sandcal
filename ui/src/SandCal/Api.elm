module SandCal.Api exposing (addEvent, allEvents)

import Http
import Json.Decode as D
import SandCal.Types as Types


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
