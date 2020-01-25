module SandCal.Forms exposing
    ( NewEvent
    , NewEventMsg
    , initNewEvent
    , updateNewEvent
    , viewNewEvent
    )

import Browser.Navigation as Nav
import DTUtil
import Html exposing (..)
import Html.Attributes exposing (for, name, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Parser exposing (Parser)
import SandCal.Api as Api
import SandCal.Types as Types
import Set exposing (Set)
import Time


type NewEventMsg
    = UpdateNewEvent (NewEvent -> NewEvent)
    | SubmitEvent
    | EventSubmitResult (Result Http.Error String)


type alias NewEvent =
    { summary : String
    , startDate : Maybe String
    , startTime : Maybe String
    , endTime : Maybe String
    , error : Maybe Error
    }


type Error
    = HttpError Http.Error
    | ParseError FieldId -- TODO: record the parse error itself?
    | MissingPart FieldId


type FieldId
    = StartDate
    | StartTime
    | EndTime


convertEvent : NewEvent -> Result Error Types.Event
convertEvent ev =
    Result.map3
        (\startDate startTime endTime ->
            { summary = ev.summary
            , start = DTUtil.partsToPosix Time.utc startDate startTime
            , end = DTUtil.partsToPosix Time.utc startDate endTime
            , recurs = []
            }
        )
        (parseField DTUtil.dateParser StartDate ev.startDate)
        (parseField DTUtil.timeOfDayParser StartTime ev.startTime)
        (parseField DTUtil.timeOfDayParser EndTime ev.endTime)


parseField : Parser a -> FieldId -> Maybe String -> Result Error a
parseField p fieldId field =
    case field of
        Nothing ->
            Err (MissingPart fieldId)

        Just input ->
            Parser.run p input
                |> Result.mapError (\_ -> ParseError fieldId)


initNewEvent : NewEvent
initNewEvent =
    { summary = ""
    , startDate = Nothing
    , startTime = Nothing
    , endTime = Nothing
    , error = Nothing
    }


updateNewEvent : Nav.Key -> NewEventMsg -> NewEvent -> ( NewEvent, Cmd NewEventMsg )
updateNewEvent navKey msg ev =
    case msg of
        UpdateNewEvent f ->
            ( f ev, Cmd.none )

        SubmitEvent ->
            case convertEvent ev of
                Err err ->
                    ( { ev | error = Just err }
                    , Cmd.none
                    )

                Ok apiEv ->
                    ( ev
                    , Api.addEvent EventSubmitResult apiEv
                    )

        EventSubmitResult (Err e) ->
            ( { ev | error = Just (HttpError e) }
            , Cmd.none
            )

        EventSubmitResult (Ok eid) ->
            -- TODO: redirect to the event's page.
            ( ev
            , Nav.pushUrl navKey eid
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
