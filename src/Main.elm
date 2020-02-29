port module Main exposing (..)

import Base64.Encode as Base64
import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Navbar as Navbar
import Bootstrap.Spinner as Spinner
import Bootstrap.Utilities.Spacing as Spacing
import Browser exposing (UrlRequest)
import Browser.Navigation as Navigation
import Bytes exposing (Bytes)
import Bytes.Encode as Bytes
import Debug
import Delay exposing (TimeUnit(..), after)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json exposing (Value)
import OAuth
import OAuth.Implicit as OAuth
import Url exposing (Protocol(..), Url)
import Url.Parser as UrlParser exposing ((</>), Parser, top)


main : Program (Maybe (List Int)) Model Msg
main =
    Browser.application
        { init =
            Maybe.map convertBytes >> init
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
    , redirectUri : Url
    , flow : Flow
    }


type Page
    = Dashboard
    | Help
    | NotFound


type alias UserInfo =
    { name : String
    , email : String
    , picture : String
    }


type alias Configuration =
    { authorizationEndpoint : Url
    , userInfoEndpoint : Url
    , userInfoDecoder : Json.Decoder UserInfo
    , clientId : String
    , scope : List String
    }


type Flow
    = Idle
    | Authorized OAuth.Token
    | Done UserInfo
    | Errored Error


type Error
    = ErrStateMismatch
    | ErrAuthorization OAuth.AuthorizationError
    | ErrHTTPGetUserInfo



--
-- HELPERS
--


toBytes : List Int -> Bytes
toBytes =
    List.map Bytes.unsignedInt8 >> Bytes.sequence >> Bytes.encode


base64 : Bytes -> String
base64 =
    Base64.bytes >> Base64.encode


convertBytes : List Int -> { state : String }
convertBytes =
    toBytes >> base64 >> (\state -> { state = state })


oauthErrorToString : { error : OAuth.ErrorCode, errorDescription : Maybe String } -> String
oauthErrorToString { error, errorDescription } =
    let
        desc =
            errorDescription |> Maybe.withDefault "" |> String.replace "+" " "
    in
    OAuth.errorCodeToString error ++ ": " ++ desc


defaultHttpsUrl : Url
defaultHttpsUrl =
    { protocol = Https
    , host = ""
    , path = ""
    , port_ = Nothing
    , query = Nothing
    , fragment = Nothing
    }


configuration : Configuration
configuration =
    { authorizationEndpoint =
        { defaultHttpsUrl | host = "jardev.auth0.com", path = "/authorize" }
    , userInfoEndpoint =
        { defaultHttpsUrl | host = "jardev.auth0.com", path = "/userinfo" }
    , userInfoDecoder =
        Json.map3 UserInfo
            (Json.field "name" Json.string)
            (Json.field "email" Json.string)
            (Json.field "picture" Json.string)
    , clientId =
        "B24wma3KZIYZojn2ANrMzsOAqIOzwVD6"
    , scope =
        [ "openid", "profile", "email" ]
    }


init : Maybe { state : String } -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init mflags url key =
    let
        redirectUri =
            { url | query = Nothing, fragment = Nothing }

        clearUrl =
            Navigation.replaceUrl key (Url.toString redirectUri)

        ( navState, navCmd ) =
            Navbar.initialState NavMsg

        ( model, urlCmd ) =
            urlUpdate url { navKey = key, navState = navState, page = Dashboard, flow = Idle, redirectUri = redirectUri }
    in
    case OAuth.parseToken url of
        OAuth.Empty ->
            ( { model | flow = Idle, redirectUri = redirectUri }
            , Cmd.none
            )

        OAuth.Success { token, state } ->
            case mflags of
                Nothing ->
                    ( { model | flow = Errored ErrStateMismatch, redirectUri = redirectUri }
                    , clearUrl
                    )

                Just flags ->
                    if state /= Just flags.state then
                        ( { model | flow = Errored ErrStateMismatch, redirectUri = redirectUri }
                        , clearUrl
                        )

                    else
                        ( { model | flow = Authorized token, redirectUri = redirectUri }
                        , Cmd.batch [ after 350 Millisecond UserInfoRequested, clearUrl ]
                        )

        OAuth.Error error ->
            ( { model | flow = Errored <| ErrAuthorization error, redirectUri = redirectUri }
            , clearUrl
            )



--    ( model, Cmd.batch [ urlCmd, navCmd ] )


port genRandomBytes : Int -> Cmd msg


port randomBytes : (List Int -> msg) -> Sub msg


getUserInfo : Configuration -> OAuth.Token -> Cmd Msg
getUserInfo { userInfoDecoder, userInfoEndpoint } token =
    Http.request
        { method = "GET"
        , body = Http.emptyBody
        , headers = OAuth.useToken token []
        , url = Url.toString userInfoEndpoint
        , expect = Http.expectJson GotUserInfo userInfoDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


type Msg
    = NoOp
    | UrlChange Url
    | ClickedLink UrlRequest
    | NavMsg Navbar.State
    | UserInfoRequested
    | GotUserInfo (Result Http.Error UserInfo)
    | SignInRequested
    | SignOutRequsted
    | GotRandomBytes (List Int)
    | GotAccessToken (Result Http.Error OAuth.AuthorizationSuccess)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Navbar.subscriptions model.navState NavMsg
        , randomBytes GotRandomBytes
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model.flow, msg ) of
        ( Idle, SignInRequested ) ->
            signInRequested model

        ( Idle, GotRandomBytes bytes ) ->
            gotRandomBytes model bytes

        ( Authorized token, UserInfoRequested ) ->
            userInfoRequested model token

        ( Authorized _, GotUserInfo userInfoResponse ) ->
            gotUserInfo model userInfoResponse

        ( Done _, SignOutRequsted ) ->
            signOutRequested model

        ( Done _, ClickedLink req ) ->
            case req of
                Browser.Internal url ->
                    ( model, Navigation.pushUrl model.navKey <| Url.toString url )

                Browser.External href ->
                    ( model, Navigation.load href )

        ( Done _, UrlChange url ) ->
            urlUpdate url model

        ( Done _, NavMsg state ) ->
            ( { model | navState = state }
            , Cmd.none
            )

        _ ->
            noOp model


noOp : Model -> ( Model, Cmd Msg )
noOp model =
    ( model, Cmd.none )


signInRequested : Model -> ( Model, Cmd Msg )
signInRequested model =
    ( { model | flow = Idle }
    , genRandomBytes 16
    )


signOutRequested : Model -> ( Model, Cmd Msg )
signOutRequested model =
    ( { model | flow = Idle }
    , Navigation.load (Url.toString model.redirectUri)
    )


gotRandomBytes : Model -> List Int -> ( Model, Cmd Msg )
gotRandomBytes model bytes =
    let
        { state } =
            convertBytes bytes

        authorization =
            { clientId = configuration.clientId
            , redirectUri = model.redirectUri
            , scope = configuration.scope
            , state = Just state
            , url = configuration.authorizationEndpoint
            }
    in
    ( { model | flow = Idle }
    , authorization
        |> OAuth.makeAuthorizationUrl
        |> Url.toString
        |> Navigation.load
    )


userInfoRequested : Model -> OAuth.Token -> ( Model, Cmd Msg )
userInfoRequested model token =
    ( { model | flow = Authorized token }
    , getUserInfo configuration token
    )


gotUserInfo : Model -> Result Http.Error UserInfo -> ( Model, Cmd Msg )
gotUserInfo model userInfoResponse =
    case userInfoResponse of
        Err _ ->
            ( { model | flow = Errored ErrHTTPGetUserInfo }
            , Cmd.none
            )

        Ok userInfo ->
            ( { model | flow = Done userInfo }
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
    url
        |> UrlParser.parse routeParser


routeParser : Parser (Page -> a) a
routeParser =
    UrlParser.oneOf
        [ UrlParser.map Dashboard top
        , UrlParser.map Help (UrlParser.s "help")
        ]


view : Model -> Html Msg
view model =
    case model.flow of
        Idle ->
            viewSignIn model

        Authorized _ ->
            viewAuthorized

        Done userInfo ->
            viewApp model userInfo

        Errored err ->
            viewErrored err


viewSignIn : Model -> Html Msg
viewSignIn model =
    div []
        [ Navbar.config NavMsg
            |> Navbar.dark
            |> Navbar.primary
            |> Navbar.brand [ href "/" ] [ text "ETA" ]
            |> Navbar.items
                [ Navbar.itemLink [ href "/" ] [ text "Sign In" ]
                ]
            |> Navbar.view model.navState
        , p [] []
        , Grid.container []
            [ Grid.row [ Row.centerXs ]
                [ Grid.col [ Col.xs6 ]
                    [ Card.config [ Card.outlinePrimary ]
                        |> Card.headerH4 [] [ text "Sign In" ]
                        |> Card.block []
                            [ Block.text [] [ text "In order to use ETA you have to sign in first" ]
                            , Block.custom <|
                                Button.button
                                    [ Button.success, Button.attrs [ onClick SignInRequested ] ]
                                    [ text "Sign In with OAuth0" ]
                            ]
                        |> Card.view
                    ]
                ]
            ]
        ]


viewAuthorized : Html Msg
viewAuthorized =
    div [ class "d-flex min-vh-100 align-items-center justify-content-center" ]
        [ div [] [ text "Loading..." ]
        , Spinner.spinner [ Spinner.grow ] []
        ]


viewErrored : Error -> Html Msg
viewErrored error =
    span [ class "span-error" ] [ viewError error ]


viewError : Error -> Html Msg
viewError e =
    text <|
        case e of
            ErrStateMismatch ->
                "'state' doesn't match, the request has likely been forged by an adversary!"

            ErrAuthorization error ->
                oauthErrorToString { error = error.error, errorDescription = error.errorDescription }

            ErrHTTPGetUserInfo ->
                "Unable to retrieve user info: HTTP request failed."


viewApp : Model -> UserInfo -> Html Msg
viewApp model userInfo =
    div []
        [ menu model userInfo
        , mainContent model
        ]


menu : Model -> UserInfo -> Html Msg
menu model userInfo =
    Navbar.config NavMsg
        |> Navbar.dark
        |> Navbar.primary
        |> Navbar.brand [ href "/" ] [ text "ETA" ]
        |> Navbar.items
            [ Navbar.itemLink [ href "/" ] [ text "Dashboard" ]
            , Navbar.itemLink [ href "/help" ] [ text "Help" ]
            ]
        |> Navbar.customItems
            [ Navbar.textItem [ Spacing.ml2Sm ] [ text ("Welcome, " ++ userInfo.name ++ "!") ]
            , Navbar.formItem []
                [ Button.button
                    [ Button.primary
                    , Button.attrs
                        [ Spacing.ml2Sm
                        , onClick SignOutRequsted
                        ]
                    ]
                    [ text "Log Out" ]
                ]
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
