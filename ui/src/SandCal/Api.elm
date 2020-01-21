module SandCal.Api exposing (allEvents)

import Http
import Json.Decode as D
import SandCal.Types as Types


allEvents : (Result Http.Error (List Types.Event) -> msg) -> Cmd msg
allEvents mkMsg =
    Http.get
        { url = "/api/all-events.json"
        , expect = Http.expectJson mkMsg (D.list Types.decodeEvent)
        }
