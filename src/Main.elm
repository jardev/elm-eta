module Main exposing (..)

import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Navbar as Navbar
import Browser exposing (UrlRequest)
import Browser.Navigation as Navigation
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode exposing (Value)
import Url exposing (Url)
import Url.Parser as UrlParser exposing ((</>), Parser, top)


type alias Flags =
    {}


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = \model -> { title = "ETA", body = [ view model ] }
        , onUrlRequest = ClickedLink
        , onUrlChange = UrlChange
        }


type alias Model =
    { page : Page
    , navKey : Navigation.Key
    , navState : Navbar.State
    }


type Page
    = Dashboard
    | Help
    | NotFound


init : Flags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( navState, navCmd ) =
            Navbar.initialState NavMsg

        ( model, urlCmd ) =
            urlUpdate url { navKey = key, navState = navState, page = Dashboard }
    in
    ( model, Cmd.batch [ urlCmd, navCmd ] )


type Msg
    = UrlChange Url
    | ClickedLink UrlRequest
    | NavMsg Navbar.State


subscriptions : Model -> Sub Msg
subscriptions model =
    Navbar.subscriptions model.navState NavMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedLink req ->
            case req of
                Browser.Internal url ->
                    ( model, Navigation.pushUrl model.navKey <| Url.toString url )

                Browser.External href ->
                    ( model, Navigation.load href )

        UrlChange url ->
            urlUpdate url model

        NavMsg state ->
            ( { model | navState = state }
            , Cmd.none
            )


urlUpdate : Url -> Model -> ( Model, Cmd Msg )
urlUpdate url model =
    case decode url of
        Nothing ->
            ( { model | page = NotFound }, Cmd.none )

        Just route ->
            ( { model | page = route }, Cmd.none )


decode : Url -> Maybe Page
decode url =
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> UrlParser.parse routeParser


routeParser : Parser (Page -> a) a
routeParser =
    UrlParser.oneOf
        [ UrlParser.map Dashboard top
        , UrlParser.map Help (UrlParser.s "help")
        ]


view : Model -> Html Msg
view model =
    div []
        [ menu model
        , mainContent model
        ]


menu : Model -> Html Msg
menu model =
    Navbar.config NavMsg
        |> Navbar.withAnimation
        |> Navbar.dark
        |> Navbar.primary
        |> Navbar.brand [ href "#" ] [ text "ETA" ]
        |> Navbar.items
            [ Navbar.itemLink [ href "#" ] [ text "Dashboard" ]
            , Navbar.itemLink [ href "#help" ] [ text "Help" ]
            ]
        |> Navbar.view model.navState


mainContent : Model -> Html Msg
mainContent model =
    Grid.container [] <|
        case model.page of
            Dashboard ->
                pageDashboard model

            Help ->
                pageHelp

            NotFound ->
                pageNotFound


pageDashboard : Model -> List (Html Msg)
pageDashboard model =
    [ h1 [] [ text "ETA Dashboard" ]
    , Grid.row []
        [ Grid.col []
            [ Card.config [ Card.outlinePrimary ]
                |> Card.headerH4 [] [ text "Your Expectations" ]
                |> Card.block []
                    [ Block.text [] [ text "Eta list goes here" ]
                    , Block.custom <|
                        Button.linkButton
                            [ Button.primary, Button.attrs [ href "#dashboard" ] ]
                            [ text "Dasboard" ]
                    ]
                |> Card.view
            ]
        , Grid.col []
            [ Card.config [ Card.outlineSecondary ]
                |> Card.headerH4 [] [ text "Team Expectations" ]
                |> Card.block []
                    [ Block.text [] [ text "Team etas go here" ]
                    ]
                |> Card.view
            ]
        ]
    ]


pageHelp : List (Html Msg)
pageHelp =
    [ h1 [] [ text "Help" ]
    , text "Here goes help page"
    ]


pageNotFound : List (Html Msg)
pageNotFound =
    [ h1 [] [ text "Not Found" ]
    , text "The page is not found"
    ]
