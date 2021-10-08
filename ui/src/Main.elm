module Main exposing (main)

import Browser
import Html exposing (..)



-- MODEL


type Model
    = Model


type Msg
    = Msg


init : {} -> ( Model, Cmd Msg )
init _ =
    ( Model, Cmd.none )



-- VIEW


view _ =
    text "Hello, World!"



-- UPDATE


update Msg Model =
    ( Model, Cmd.none )



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
