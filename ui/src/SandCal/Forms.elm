module SandCal.Forms exposing
    ( NewEvent
    , NewEventMsg
    , initNewEvent
    , updateNewEvent
    , viewNewEvent
    )

import Html exposing (..)
import Html.Attributes exposing (for, name, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as D
import Json.Encode as E


type NewEventMsg
    = UpdateNewEvent (NewEvent -> NewEvent)
    | SubmitEvent
    | EventSubmitResult (Result Http.Error String)


type alias NewEvent =
    { summary : String
    , startDate : Maybe String
    , startTime : Maybe String
    , endTime : Maybe String
    , error : Maybe Http.Error
    }


encodeNewEvent : NewEvent -> E.Value
encodeNewEvent ev =
    E.object
        [ ( "summary", E.string ev.summary )
        , ( "startDate", ev.startDate |> Maybe.withDefault "" |> E.string )
        , ( "startTime", ev.startTime |> Maybe.withDefault "" |> E.string )
        , ( "endTime", ev.endTime |> Maybe.withDefault "" |> E.string )
        ]


initNewEvent : NewEvent
initNewEvent =
    { summary = ""
    , startDate = Nothing
    , startTime = Nothing
    , endTime = Nothing
    , error = Nothing
    }


updateNewEvent : NewEventMsg -> NewEvent -> ( NewEvent, Cmd NewEventMsg )
updateNewEvent msg ev =
    case msg of
        UpdateNewEvent f ->
            ( f ev, Cmd.none )

        SubmitEvent ->
            ( ev
            , Http.post
                { url = "/event/new"
                , body = Http.jsonBody (encodeNewEvent ev)
                , expect = Http.expectJson EventSubmitResult D.string
                }
            )

        EventSubmitResult (Err e) ->
            ( { ev | error = Just e }
            , Cmd.none
            )

        EventSubmitResult (Ok _) ->
            -- TODO: redirect to the event's page.
            ( ev
            , Cmd.none
            )


viewNewEvent : NewEvent -> Html NewEventMsg
viewNewEvent form =
    let
        maybeValue viewVal =
            Maybe.map (List.singleton << viewVal)
                >> Maybe.withDefault []

        inputField nam ty val update =
            div []
                [ label [ for nam ] [ text nam ]
                , input
                    (onInput (\s -> UpdateNewEvent (update s))
                        :: type_ ty
                        :: name nam
                        :: maybeValue value val
                    )
                    []
                ]
    in
    div []
        [ inputField "Summary"
            "text"
            (Just form.summary)
            (\s f -> { f | summary = s })
        , inputField "Date"
            "date"
            form.startDate
            (\s f -> { f | startDate = Just s })
        , inputField "Start Time"
            "time"
            form.startTime
            (\s f -> { f | startTime = Just s })
        , inputField "End Time"
            "time"
            form.endTime
            (\s f -> { f | endTime = Just s })
        , button [ onClick SubmitEvent ] [ text "Create" ]
        ]
