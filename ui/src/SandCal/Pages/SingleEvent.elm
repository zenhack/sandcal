module SandCal.Pages.SingleEvent exposing
    ( Model
    , Msg
    , init
    , update
    , view
    , viewTitle
    )

import DTUtil
import DTUtil.Month
import Html exposing (..)
import Html.Attributes exposing (datetime)
import Http
import SandCal.Api as Api
import SandCal.Types as Types
import Time


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


viewTitle : Model -> String
viewTitle model =
    case model of
        NotLoaded _ ->
            "Event: (loading)"

        Loaded ev ->
            "Event: " ++ ev.summary

        LoadFailed err ->
            "Event: (error loading event)"


{-| TODO: this doesn't belong here; move it into a util module.
-}
viewTime : Time.Posix -> Html msg
viewTime posixTime =
    let
        dt =
            DTUtil.fromPosix Time.utc posixTime

        nnString n =
            if n < 10 then
                "0" ++ String.fromInt n

            else
                String.fromInt n

        dateString =
            String.fromInt dt.date.year
                ++ "-"
                ++ nnString (DTUtil.Month.toInt dt.date.month)
                ++ "-"
                ++ nnString dt.date.day

        timeString =
            nnString dt.time.hour
                ++ ":"
                ++ nnString dt.time.minute

        dtString =
            dateString ++ " " ++ timeString
    in
    time [ datetime dtString ] [ text dtString ]


view : Model -> Html msg
view model =
    case model of
        NotLoaded _ ->
            text "Loading..."

        LoadFailed _ ->
            text "Failed to load."

        Loaded ev ->
            div []
                [ h2 [] [ text ev.summary ]
                , p [] [ text "Start: ", viewTime ev.start ]
                , p [] [ text "End: ", viewTime ev.end ]
                ]
