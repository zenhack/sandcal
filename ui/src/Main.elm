module Main exposing (main)

import Accessors
import Browser
import FormValues
import Html exposing (..)
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
        Err _ ->
            Debug.todo "Failed to parse flags"

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



-- VIEW (TODO)


view _ =
    text "Hello, World!"



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
