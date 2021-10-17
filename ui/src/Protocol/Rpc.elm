module Protocol.Rpc exposing (postEvent)

import Dict
import Http
import Protocol


postEvent : String -> String -> Protocol.NewEvent -> (Result Http.Error String -> msg) -> Cmd msg
postEvent csrfToken action event handle =
    Http.request
        { method = "POST"
        , headers = [ Http.header "X-CSRF-Token" csrfToken ]
        , url = action
        , body = Http.jsonBody (Protocol.encodeNewEvent event)
        , timeout = Nothing
        , tracker = Nothing
        , expect =
            Http.expectBytesResponse
                handle
                (\response ->
                    case response of
                        Http.GoodStatus_ meta _ ->
                            Ok meta.url

                        Http.BadStatus_ meta _ ->
                            Err (Http.BadStatus meta.statusCode)

                        Http.NetworkError_ ->
                            Err Http.NetworkError

                        Http.Timeout_ ->
                            Err Http.Timeout

                        Http.BadUrl_ url ->
                            Err (Http.BadUrl url)
                )
        }
