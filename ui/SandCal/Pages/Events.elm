module SandCal.Pages.Events exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (href)
import Http
import SandCal.Api as Api
import SandCal.Types as Types


type Model
    = Model { events : Maybe EventsResult }


type alias EventsResult =
    Result Http.Error (List Types.Event)


type Msg
    = AllEventsResult (Result Http.Error (List Types.Event))


init : ( Model, Cmd Msg )
init =
    ( Model { events = Nothing }
    , Api.allEvents AllEventsResult
    )


view : Model -> Html msg
view (Model { events }) =
    div []
        [ viewEvents events
        , a [ href "/event/new" ] [ text "New Event" ]
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


update : Msg -> Model -> Model
update (AllEventsResult res) (Model m) =
    Model { m | events = Just res }
