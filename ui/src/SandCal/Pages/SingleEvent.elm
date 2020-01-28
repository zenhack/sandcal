module SandCal.Pages.SingleEvent exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import Html exposing (..)
import Http
import SandCal.Api as Api
import SandCal.Types as Types


type Model
    = NotLoaded Int
    | Loaded Types.Event
    | LoadFailed Http.Error


type Msg
    = Load (Result Http.Error Types.Event)


init : Int -> ( Model, Cmd Msg )
init id =
    ( NotLoaded id
    , Api.getEvent Load id
    )


update : Msg -> Model -> Model
update (Load res) model =
    case res of
        Ok ev ->
            Loaded ev

        Err err ->
            LoadFailed err


view : Model -> Html msg
view model =
    case model of
        NotLoaded _ ->
            text "Loading..."

        LoadFailed _ ->
            text "Failed to load."

        Loaded ev ->
            text ev.summary
