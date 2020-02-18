module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (for, href, name, type_, value)
import Http
import ICal
import Ports
import SandCal.Api as Api
import SandCal.Pages.Events as Events
import SandCal.Pages.NewEvent as NewEvent
import SandCal.Pages.SingleEvent as SingleEvent
import SandCal.Types as Types
import Time
import Url
import Url.Parser exposing ((</>))


type Model
    = Model
        { page : Page
        , navKey : Nav.Key
        , grainTitle : String
        }


type Page
    = EventsPage Events.Model
    | NewEventPage NewEvent.Model
    | SingleEventPage SingleEvent.Model
    | NotFoundPage


type Msg
    = ClickedLink Browser.UrlRequest
    | UrlChange Url.Url
    | PageMsg PageMsg
    | GrainTitleChange String


type PageMsg
    = EventsPageMsg Events.Msg
    | NewEventPageMsg NewEvent.Msg
    | SingleEventPageMsg SingleEvent.Msg


init : {} -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url navKey =
    let
        ( page, cmd ) =
            initPage url
    in
    ( Model
        { navKey = navKey
        , grainTitle = ""
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
            let
                ( page, cmd ) =
                    Events.init
            in
            ( EventsPage page
            , Cmd.map EventsPageMsg cmd
            )

        Just NewEvent ->
            ( NewEventPage NewEvent.init
            , Cmd.none
            )

        Just (SingleEvent id) ->
            let
                ( page, cmd ) =
                    SingleEvent.init id
            in
            ( SingleEventPage page
            , Cmd.map SingleEventPageMsg cmd
            )


viewTitle : String -> String -> String
viewTitle grainTitle pageTitle =
    let
        mainTitle =
            case grainTitle of
                "" ->
                    "SandCal"

                _ ->
                    grainTitle

        subTitle =
            case pageTitle of
                "" ->
                    ""

                _ ->
                    " - " ++ pageTitle
    in
    mainTitle ++ subTitle


viewPage : List (Html Msg) -> List (Html Msg)
viewPage content =
    [ h1 [] [ a [ href "/" ] [ text "SandCal" ] ]
    , div [] content
    ]


view : Model -> Browser.Document Msg
view (Model { page, grainTitle }) =
    case page of
        EventsPage events ->
            { title = viewTitle grainTitle ""
            , body =
                viewPage
                    [ Events.view events
                        |> Html.map (EventsPageMsg >> PageMsg)
                    ]
            }

        NewEventPage form ->
            { title = viewTitle grainTitle "New Event"
            , body =
                viewPage
                    [ NewEvent.view form
                        |> Html.map (NewEventPageMsg >> PageMsg)
                    ]
            }

        SingleEventPage eventPg ->
            { title = viewTitle grainTitle <| SingleEvent.viewTitle eventPg
            , body = viewPage [ SingleEvent.view eventPg ]
            }

        NotFoundPage ->
            { title = viewTitle grainTitle "Not Found"
            , body = viewPage [ text "404 - not found" ]
            }


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
                    updatePage m.navKey pageMsg m.page
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

        GrainTitleChange newTitle ->
            ( Model { m | grainTitle = newTitle }
            , Cmd.none
            )


updatePage : Nav.Key -> PageMsg -> Page -> ( Page, Cmd PageMsg )
updatePage navKey pageMsg page =
    case ( page, pageMsg ) of
        ( EventsPage p, EventsPageMsg msg ) ->
            let
                ( newPage, cmd ) =
                    Events.update msg p
            in
            ( EventsPage newPage
            , Cmd.map EventsPageMsg cmd
            )

        ( NewEventPage form, NewEventPageMsg msg ) ->
            let
                ( newForm, cmd ) =
                    NewEvent.update navKey msg form
            in
            ( NewEventPage newForm
            , Cmd.map NewEventPageMsg cmd
            )

        ( SingleEventPage eventPg, SingleEventPageMsg msg ) ->
            ( SingleEventPage (SingleEvent.update msg eventPg)
            , Cmd.none
            )

        _ ->
            ( page
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Ports.grainTitle GrainTitleChange


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
    | SingleEvent Int


urlParser : Url.Parser.Parser (Route -> a) a
urlParser =
    Url.Parser.oneOf
        [ Url.Parser.map Root Url.Parser.top
        , Url.Parser.map NewEvent (Url.Parser.s "event" </> Url.Parser.s "new")
        , Url.Parser.map SingleEvent (Url.Parser.s "event" </> Url.Parser.int)
        ]
