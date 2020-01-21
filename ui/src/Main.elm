module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Http
import SandCal.Api as Api
import SandCal.Types as Types
import Time
import Url


type Model
    = Model
        { page : Page
        , navKey : Nav.Key
        }


type alias EventsResult =
    Result Http.Error (List Types.Event)


type Page
    = EventsPage { events : Maybe EventsResult }


type Msg
    = ClickedLink Browser.UrlRequest
    | UrlChange Url.Url
    | PageMsg PageMsg


type PageMsg
    = EventsPageMsg EventsPageMsg


type EventsPageMsg
    = AllEventsResult (Result Http.Error (List Types.Event))


init : {} -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ _ navKey =
    ( Model
        { navKey = navKey
        , page = EventsPage { events = Nothing }
        }
    , Api.allEvents (AllEventsResult >> EventsPageMsg >> PageMsg)
    )


view : Model -> Browser.Document msg
view (Model { page }) =
    case page of
        EventsPage { events } ->
            { title = "SandCal"
            , body = [ viewEvents events ]
            }


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
                    |> List.map
                        (\ev ->
                            li [] [ text ev.summary ]
                        )
                )


onUrlRequest : Browser.UrlRequest -> Msg
onUrlRequest =
    ClickedLink


onUrlChange : Url.Url -> Msg
onUrlChange =
    UrlChange


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model m) =
    case msg of
        PageMsg pageMsg ->
            let
                ( newPage, cmd ) =
                    updatePage pageMsg m.page
            in
            ( Model { m | page = newPage }
            , Cmd.map PageMsg cmd
            )

        ClickedLink (Browser.Internal url) ->
            ( Model m
            , Nav.pushUrl m.navKey (Url.toString url)
            )

        ClickedLink (Browser.External url) ->
            ( Model m
            , Nav.load url
            )

        UrlChange _ ->
            ( Model m
            , Cmd.none
            )


updatePage : PageMsg -> Page -> ( Page, Cmd PageMsg )
updatePage (EventsPageMsg msg) (EventsPage p) =
    case msg of
        AllEventsResult res ->
            ( EventsPage { p | events = Just res }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main =
    Browser.application
        { init = init
        , view = view
        , onUrlRequest = onUrlRequest
        , onUrlChange = onUrlChange
        , update = update
        , subscriptions = subscriptions
        }
