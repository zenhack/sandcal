module Main exposing (main)

import Accessors
import Browser
import FormValues
import GenAccessors as GA
import GenTz
import Html exposing (..)
import Html.Attributes as Attributes exposing (class, for, name, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Json.Decode as D
import Json.Encode as E
import Protocol



-- MODEL


type alias Model =
    { userTz : String
    , formValues : FormValues.Model
    , formValuesInit : FormValues.Model
    , action : String
    , submitText : String
    , csrfToken : String
    }


type alias Msg =
    FormValues.Msg


type alias Flags =
    E.Value


init : Flags -> ( Model, Cmd Msg )
init flagsValue =
    case D.decodeValue FormValues.decodeFlags flagsValue of
        Err e ->
            Debug.todo <| "Failed to parse flags: " ++ Debug.toString e

        Ok flags ->
            let
                formValues =
                    FormValues.init flags
            in
            ( { formValues = formValues
              , formValuesInit = formValues
              , userTz = FormValues.chooseTz flags
              , action = flags.tpl.action
              , submitText = flags.tpl.submitText
              , csrfToken = flags.tpl.csrfToken
              }
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    let
        trackedTextArea key accessor =
            let
                content =
                    Accessors.get accessor model.formValues

                event =
                    onInput (FormValues.InputChanged accessor)
            in
            labeledElem textarea key [ event, value content ] []

        trackedInput typ attrs key accessor =
            let
                event =
                    onInput (FormValues.InputChanged accessor)
            in
            labeledInput typ
                key
                (value (Accessors.get accessor model.formValues)
                    :: event
                    :: attrs
                )
    in
    div
        [ class "form" ]
        [ formBlock <|
            [ input [ type_ "hidden", name "csrfToken", value model.csrfToken ] []
            , trackedInput "text" [] "Summary" GA.summary []
            , trackedInput "date" [] "Date" GA.date []
            , labeledInput "checkbox" "All Day" [ onCheck FormValues.SetAllDay ] []
            ]
                ++ (if model.formValues.allDay then
                        []

                    else
                        [ trackedInput "time" [] "Start Time" (GA.time << GA.start) []
                        , trackedInput "time" [] "End Time" (GA.time << GA.stop) []
                        , labeledTzSelect "Time Zone" (Just model.formValues.timeZone)
                        ]
                   )
                ++ [ trackedInput "text" [] "Location" GA.location []
                   , trackedTextArea "Description" GA.description
                   , h2 [] [ text "Repeats" ]
                   ]
                ++ List.indexedMap
                    (\i r ->
                        viewRepeatRule (GA.repeat << nth i) i r
                    )
                    model.formValues.repeat
                ++ [ button [ onClick FormValues.NewRepeat ] [ text "New repeat rule" ] ]
        , button
            [ Attributes.disabled (not (FormValues.valid model.formValues))
            , onClick FormValues.Submit
            ]
            [ text model.submitText ]
        ]


nth _ =
    Debug.todo


viewRepeatRule accessor i r =
    text "TODO"


formBlock =
    div [ class "formBlock" ]


labeledTzSelect selectName userTz =
    labeledSelect selectName
        (List.map
            (\tzName ->
                ( tzName
                , userTz
                    |> Maybe.map (\s -> s == tzName)
                    |> Maybe.withDefault False
                )
            )
            GenTz.tzLabels
        )


labeledSelect selectName options =
    labeledElem select
        selectName
        []
        (List.map
            (\( name, selected ) ->
                option
                    [ value name
                    , Attributes.selected selected
                    ]
                    [ text name ]
            )
            options
        )


labeledInput typ labelName attrs =
    labeledElem input labelName (type_ typ :: attrs)


labeledElem elem labelName attrs kids =
    div
        [ class "labeledInput" ]
        [ label [ for labelName ] [ text labelName ]
        , elem (name labelName :: attrs) kids
        ]



-- UPDATE (TODO)


update _ model =
    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions _ =
    Sub.none


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
