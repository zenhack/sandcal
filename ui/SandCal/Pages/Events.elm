module SandCal.Pages.Events exposing (Model, Msg, init, update, view)

import File exposing (File)
import File.Select
import Html exposing (..)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Http
import SandCal.Api as Api
import SandCal.Types as Types


type Model
    = Model { events : Maybe EventsResult }


type alias EventsResult =
    Result Http.Error (List Types.Event)


type Msg
    = AllEventsResult (Result Http.Error (List Types.Event))
    | SelectFile
    | FileChosen File
    | UploadResult (Result Http.Error ())


init : ( Model, Cmd Msg )
init =
    ( Model { events = Nothing }
    , Api.allEvents AllEventsResult
    )


view : Model -> Html Msg
view (Model { events }) =
    div []
        [ viewEvents events
        , a [ href "/event/new" ] [ text "New Event" ]
        , div []
            [ button
                [ onClick SelectFile ]
                [ text "Import ICalendar File" ]
            ]
        ]


viewEvents : Maybe EventsResult -> Html msg
viewEvents events =
    case events of
        Nothing ->
            text "Loading..."

        Just (Err err) ->
            -- TODO: more descriptive
            text "An error occurred communicating with the server."

        Just (Ok evs) ->
            ul []
                (evs
                    |> List.filterMap
                        (\ev ->
                            -- TODO: these should never actually be `Nothing`; the server always
                            -- includes the types in all-events.json; tweak the types to rule
                            -- this out.
                            Maybe.map
                                (\id ->
                                    li []
                                        [ a [ href ("/event/" ++ String.fromInt id) ] [ text ev.summary ]
                                        ]
                                )
                                ev.id
                        )
                )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model m) =
    case msg of
        AllEventsResult res ->
            ( Model { m | events = Just res }
            , Cmd.none
            )

        SelectFile ->
            ( Model m
            , File.Select.file [ "text/calendar" ] FileChosen
            )

        FileChosen file ->
            ( Model m
            , Api.importICalendar UploadResult file
            )

        UploadResult (Ok _) ->
            ( Model m
            , Api.allEvents AllEventsResult
            )

        UploadResult (Err err) ->
            -- XXX: clean this up:
            ( Model { m | events = Just (Err err) }
            , Cmd.none
            )
