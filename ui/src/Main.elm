module Main exposing (main)

import Accessors
import Browser
import Html exposing (..)



-- MODEL


type Model
    = Model


type Msg
    = InputChanged (Accessors.Relation Model String String) String
    | SetAllDay Bool
    | NewRepeat
    | DeleteRepeat Int
    | Submit


init : {} -> ( Model, Cmd Msg )
init _ =
    ( Model, Cmd.none )



-- VIEW


view _ =
    text "Hello, World!"



-- UPDATE


update _ Model =
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
