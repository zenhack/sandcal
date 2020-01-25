module SandCal.Pages.NewEvent exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import Browser.Navigation as Nav
import DTUtil
import Html exposing (..)
import Html.Attributes exposing (for, name, selected, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Http
import Parser exposing (Parser)
import SandCal.Api as Api
import SandCal.Types as Types
import Set exposing (Set)
import Time


type Msg
    = UpdateNewEvent (Model -> Model)
    | SubmitEvent
    | EventSubmitResult (Result Http.Error String)
    | SetRecurring Bool


type alias Model =
    { summary : String
    , startDate : Maybe String
    , startTime : Maybe String
    , endTime : Maybe String
    , error : Maybe Error

    -- We store wether we're specifying a recurring event and what the
    -- recurrence is separately. This may seem like an obivous use case
    -- for Maybe, but we actually *do* want to track recur even when
    -- isRecurring = False, because if the user unchecks and then re-checks
    -- the box, we want to remember what the value was.
    , isRecurring : Bool
    , recur : Types.Recur
    }


type Error
    = HttpError Http.Error
    | ParseError FieldId -- TODO: record the parse error itself?
    | MissingPart FieldId


type FieldId
    = StartDate
    | StartTime
    | EndTime


convertEvent : Model -> Result Error Types.Event
convertEvent ev =
    Result.map3
        (\startDate startTime endTime ->
            { summary = ev.summary
            , start = DTUtil.partsToPosix Time.utc startDate startTime
            , end = DTUtil.partsToPosix Time.utc startDate endTime
            , recurs =
                if ev.isRecurring then
                    [ ev.recur ]

                else
                    []
            , id = Nothing
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


init : Model
init =
    { summary = ""
    , startDate = Nothing
    , startTime = Nothing
    , endTime = Nothing
    , error = Nothing
    , isRecurring = False
    , recur =
        { frequency = Types.Weekly
        , until = Nothing
        }
    }


update : Nav.Key -> Msg -> Model -> ( Model, Cmd Msg )
update navKey msg ev =
    case msg of
        UpdateNewEvent f ->
            ( f ev, Cmd.none )

        SetRecurring isRecurring ->
            ( { ev | isRecurring = isRecurring }
            , Cmd.none
            )

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


labeledInput : String -> List (Attribute msg) -> List (Html msg) -> Html msg
labeledInput nam attrs kids =
    div []
        [ label [ for nam ] [ text nam ]
        , input (name nam :: attrs) kids
        ]


view : Model -> Html Msg
view form =
    let
        maybeValue viewVal =
            Maybe.map (List.singleton << viewVal)
                >> Maybe.withDefault []

        inputField nam ty val updateFn =
            labeledInput nam
                (onInput (\s -> UpdateNewEvent (updateFn s))
                    :: type_ ty
                    :: maybeValue value val
                )
                []
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
        , viewRecurInput form
        , button [ onClick SubmitEvent ] [ text "Create" ]
        ]


viewRecurInput : { a | isRecurring : Bool, recur : Types.Recur } -> Html Msg
viewRecurInput { isRecurring, recur } =
    div []
        (labeledInput "Recurring?"
            [ type_ "checkbox"
            , onCheck SetRecurring
            ]
            []
            :: (if not isRecurring then
                    []

                else
                    [ select []
                        (frequencyChoices
                            |> List.map
                                (\choice ->
                                    option
                                        [ selected (choice == recur.frequency) ]
                                        -- TODO: get rid of this use of Debug,
                                        -- so we can --optimize.
                                        [ text (Debug.toString choice) ]
                                )
                        )
                    ]
               )
        )


frequencyChoices =
    [ Types.Daily
    , Types.Weekly
    , Types.Monthly
    , Types.Yearly
    ]
