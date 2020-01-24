module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (for, href, name, type_, value)
import Http
import Ports
import SandCal.Api as Api
import SandCal.Forms as Forms
import SandCal.Types as Types
import Time
import Url
import Url.Parser exposing ((</>))


type Model
    = Model
        { page : Page
        , navKey : Nav.Key
        }


type alias EventsResult =
    Result Http.Error (List Types.Event)


type Page
    = EventsPage { events : Maybe EventsResult }
    | NewEventPage Forms.NewEvent
    | NotFoundPage


type Msg
    = ClickedLink Browser.UrlRequest
    | UrlChange Url.Url
    | PageMsg PageMsg


type PageMsg
    = EventsPageMsg EventsPageMsg
    | NewEventPageMsg Forms.NewEventMsg


type EventsPageMsg
    = AllEventsResult (Result Http.Error (List Types.Event))


init : {} -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url navKey =
    let
        ( page, cmd ) =
            initPage url
    in
    ( Model
        { navKey = navKey
        , page = page
        }
    , Cmd.map PageMsg cmd
    )


initPage : Url.Url -> ( Page, Cmd PageMsg )
initPage url =
    case Url.Parser.parse urlParser url of
        Nothing ->
            ( NotFoundPage
            , Cmd.none
            )

        Just Root ->
            ( EventsPage { events = Nothing }
            , Api.allEvents (AllEventsResult >> EventsPageMsg)
            )

        Just NewEvent ->
            ( NewEventPage Forms.initNewEvent
            , Cmd.none
            )


view : Model -> Browser.Document Msg
view (Model { page }) =
    case page of
        EventsPage { events } ->
            { title = "SandCal"
            , body =
                [ viewEvents events
                , a [ href "/event/new" ] [ text "New Event" ]
                ]
            }

        NewEventPage form ->
            { title = "SandCal - New Event"
            , body =
                [ Forms.viewNewEvent form
                    |> Html.map (NewEventPageMsg >> PageMsg)
                ]
            }

        NotFoundPage ->
            { title = "SandCal - Not Found"
            , body = [ text "404 - not found" ]
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

        UrlChange url ->
            let
                ( page, cmd ) =
                    initPage url
            in
            ( Model { m | page = page }
            , Cmd.batch
                [ Cmd.map PageMsg cmd
                , Ports.syncFrame ()
                ]
            )


updatePage : PageMsg -> Page -> ( Page, Cmd PageMsg )
updatePage pageMsg page =
    case ( page, pageMsg ) of
        ( EventsPage p, EventsPageMsg (AllEventsResult res) ) ->
            ( EventsPage { p | events = Just res }
            , Cmd.none
            )

        ( NewEventPage form, NewEventPageMsg msg ) ->
            let
                ( newForm, cmd ) =
                    Forms.updateNewEvent msg form
            in
            ( NewEventPage newForm
            , Cmd.map NewEventPageMsg cmd
            )

        _ ->
            ( page
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


type Route
    = Root
    | NewEvent


urlParser : Url.Parser.Parser (Route -> a) a
urlParser =
    Url.Parser.oneOf
        [ Url.Parser.map Root Url.Parser.top
        , Url.Parser.map NewEvent (Url.Parser.s "event" </> Url.Parser.s "new")
        ]
