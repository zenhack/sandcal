module Main exposing (main)

import Api
import Browser
import Html exposing (..)
import Http
import Time
import Types


type Model
    = Model
        { events : Maybe (Result Http.Error (List Types.Event))
        }


type Msg
    = AllEventsResult (Result Http.Error (List Types.Event))


init : {} -> ( Model, Cmd Msg )
init _ =
    ( Model { events = Nothing }
    , Api.allEvents AllEventsResult
    )


view : Model -> Html msg
view (Model { events }) =
    case events of
        Nothing ->
            text "Loading..."

        Just (Err err) ->
            -- TODO: more descriptive
            text "An error occurred communicating with the server."

        Just (Ok evs) ->
            ul []
                (evs
                    |> List.map
                        (\ev ->
                            li [] [ text ev.summary ]
                        )
                )


update : Msg -> Model -> ( Model, Cmd Msg )
update (AllEventsResult res) (Model m) =
    ( Model { m | events = Just res }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
