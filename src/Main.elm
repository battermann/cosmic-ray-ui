module Main exposing (Model, Msg(..), init, main, update, view)

import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Grid as Grid
import Bootstrap.Navbar as Navbar
import Bootstrap.Utilities.Flex as Flex
import Bootstrap.Utilities.Size as Size
import Bootstrap.Utilities.Spacing as Spacing
import Browser
import Browser.Navigation as Nav
import Html
import Html.Attributes
import MyGames
import Page.Game as Game
import Page.Games as Games
import Page.Play as Play
import Result
import Types.ClientId exposing (ClientId(..))
import Types.CmdEndpoint exposing (CmdEndpoint(..))
import Types.GameId exposing (GameId(..))
import Types.QueryEndpoint exposing (QueryEndpoint(..))
import UUID
import Url
import Url.Parser exposing ((</>), Parser, map, oneOf, s, top)



---- MODEL ----


type alias Model =
    { navbarState : Navbar.State
    , key : Nav.Key
    , url : Url.Url
    , page : Page
    , clientId : Maybe ClientId
    , myGames : Maybe MyGames.Model
    , cmdEndpoint : CmdEndpoint
    , queryEndpoint : QueryEndpoint
    }


type Page
    = HomePage
    | GamesPage Games.Model
    | GamePage Game.Model
    | PlayPage Play.Model
    | NotFound


type Route
    = Home
    | Games
    | Play
    | Game GameId



---- ROUTING ----


routeParser : Parser (Route -> a) a
routeParser =
    let
        uuid =
            Url.Parser.custom "" (UUID.fromString >> Result.toMaybe >> Maybe.map GameId)
    in
    oneOf
        [ map Home top
        , map Games (s "games")
        , map Play (s "play")
        , map Game (s "games" </> uuid)
        ]


urlUpdate : Url.Url -> Model -> ( Model, Cmd Msg )
urlUpdate url model =
    case Url.Parser.parse routeParser url of
        Nothing ->
            ( { model | page = NotFound }, Cmd.none )

        Just Home ->
            ( { model | page = HomePage }, Cmd.none )

        Just Games ->
            let
                ( page, cmd ) =
                    case model.clientId of
                        Just clientId ->
                            Games.init model.queryEndpoint clientId
                                |> Tuple.mapFirst GamesPage
                                |> Tuple.mapSecond (Cmd.map GamesPageMsg)

                        Nothing ->
                            ( NotFound, Cmd.none )
            in
            ( { model | page = page }, cmd )

        Just (Game gameId) ->
            let
                ( page, cmd ) =
                    case model.clientId of
                        Just clientId ->
                            Game.init model.queryEndpoint gameId clientId
                                |> Tuple.mapFirst GamePage
                                |> Tuple.mapSecond (Cmd.map GamePageMsg)

                        Nothing ->
                            ( NotFound, Cmd.none )
            in
            ( { model | page = page }, cmd )

        Just Play ->
            let
                ( page, cmd ) =
                    case model.clientId of
                        Just clientId ->
                            Play.init model.queryEndpoint clientId
                                |> Tuple.mapFirst PlayPage
                                |> Tuple.mapSecond (Cmd.map PlayPageMsg)

                        Nothing ->
                            ( NotFound, Cmd.none )
            in
            ( { model | page = page }, cmd )


type alias Flags =
    { clientId : String, queryEndpoint : String, commandEndpoint : String }



---- UPDATE ----


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( navbarState, navbarCmd ) =
            Navbar.initialState NavbarMsg

        clientId =
            UUID.fromString flags.clientId |> Result.toMaybe |> Maybe.map ClientId

        ( myGamesModel, myGamesCmd ) =
            clientId
                |> Maybe.map MyGames.init
                |> Maybe.map (Tuple.mapFirst Just)
                |> Maybe.map (Tuple.mapSecond (Cmd.map MyGamesMsg))
                |> Maybe.withDefault ( Nothing, Cmd.none )

        ( model, urlCmd ) =
            urlUpdate url
                { navbarState = navbarState
                , key = key
                , url = url
                , page = HomePage
                , clientId = clientId
                , myGames = myGamesModel
                , queryEndpoint = QueryEndpoint flags.queryEndpoint
                , cmdEndpoint = CmdEndpoint flags.commandEndpoint
                }
    in
    ( model, Cmd.batch [ navbarCmd, urlCmd, myGamesCmd ] )


type Msg
    = NavbarMsg Navbar.State
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GamesPageMsg Games.Msg
    | GamePageMsg Game.Msg
    | PlayPageMsg Play.Msg
    | MyGamesMsg MyGames.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NavbarMsg state ->
            ( { model | navbarState = state }, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            urlUpdate url model

        GamesPageMsg gamesPageMsg ->
            case model.page of
                GamesPage gamesPageModel ->
                    let
                        ( m, cmd ) =
                            Games.update gamesPageMsg gamesPageModel
                    in
                    ( { model | page = GamesPage m }, cmd |> Cmd.map GamesPageMsg )

                _ ->
                    ( model, Cmd.none )

        GamePageMsg gamePageMsg ->
            case model.page of
                GamePage gamePageModel ->
                    let
                        ( m, cmd ) =
                            Game.update model.cmdEndpoint gamePageMsg gamePageModel
                    in
                    ( { model | page = GamePage m }, cmd |> Cmd.map GamePageMsg )

                _ ->
                    ( model, Cmd.none )

        PlayPageMsg playPageMsg ->
            case model.page of
                PlayPage playPageModel ->
                    let
                        ( m, cmd ) =
                            Play.update model.cmdEndpoint playPageMsg playPageModel
                    in
                    ( { model | page = PlayPage m }, cmd |> Cmd.map PlayPageMsg )

                _ ->
                    ( model, Cmd.none )

        MyGamesMsg myGamesMsg ->
            model.myGames
                |> Maybe.map
                    (\myGamesModel ->
                        let
                            ( m, cmd ) =
                                MyGames.update myGamesMsg myGamesModel
                        in
                        ( { model | myGames = Just m }, cmd |> Cmd.map MyGamesMsg )
                    )
                |> Maybe.withDefault
                    ( model, Cmd.none )



---- VIEW ----


viewCardList : List (Card.Config Msg)
viewCardList =
    [ Card.config []
        |> Card.headerH4 [] [ Html.text "Observe Games" ]
        |> Card.block []
            [ Block.text []
                [ Html.text "View the list of live games to observe."
                ]
            ]
        |> Card.footer []
            [ Button.linkButton
                [ Button.attrs [ Size.w100 ]
                , Button.primary
                , Button.attrs [ Html.Attributes.href "/games" ]
                ]
                [ Html.div [] [ Html.i [ Html.Attributes.class "far fa-eye" ] [], Html.text " Games" ] ]
            ]
    , Card.config []
        |> Card.headerH4 [] [ Html.text "Play Connect 4" ]
        |> Card.block []
            [ Block.text [] [ Html.text "You can either create a new game or accept a challenge." ]
            ]
        |> Card.footer [ Flex.block, Flex.row, Flex.alignItemsCenter, Flex.justifyAround ]
            [ Button.linkButton
                [ Button.attrs [ Size.w100 ]
                , Button.primary
                , Button.attrs [ Html.Attributes.href "/play" ]
                ]
                [ Html.div [] [ Html.i [ Html.Attributes.class "fas fa-gamepad" ] [], Html.text " Play" ] ]
            ]
    ]


viewMenu : Model -> Html.Html Msg
viewMenu model =
    Navbar.config NavbarMsg
        |> Navbar.light
        |> Navbar.withAnimation
        |> Navbar.container
        |> Navbar.brand [ Html.Attributes.href "/" ] [ Html.i [ Html.Attributes.class "fas fa-home" ] [] ]
        |> Navbar.items
            [ Navbar.itemLink [ Html.Attributes.href "/games" ] [ Html.i [ Html.Attributes.class "far fa-eye" ] [], Html.text " Games" ]
            , Navbar.itemLink [ Html.Attributes.href "/play" ] [ Html.i [ Html.Attributes.class "fas fa-gamepad" ] [], Html.text " Play" ]
            ]
        |> Navbar.view model.navbarState


viewPageNotFound : Html.Html Msg
viewPageNotFound =
    Html.div []
        [ Html.h1 [] [ Html.text "Not found" ]
        , Html.text "Sorry couldn't find that page"
        ]


viewMainContent : Model -> Html.Html Msg
viewMainContent model =
    case model.page of
        HomePage ->
            Card.deck viewCardList

        GamesPage gamesModel ->
            Games.view gamesModel |> Html.map GamesPageMsg

        GamePage gameModel ->
            Game.view gameModel |> Html.map GamePageMsg

        PlayPage playModel ->
            Play.view playModel |> Html.map PlayPageMsg

        NotFound ->
            viewPageNotFound


view : Model -> Browser.Document Msg
view model =
    { title = "Connect 4"
    , body =
        [ viewMenu model
        , Grid.container [] <|
            [ Html.div [ Spacing.mt3 ]
                [ model.myGames
                    |> Maybe.map MyGames.view
                    |> Maybe.withDefault (Html.text "")
                    |> Html.map MyGamesMsg
                ]
            , Html.div [ Spacing.mt3 ] [ viewMainContent model ]
            ]
        ]
    }



---- SUBSCRIPTION ----


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        pageSub =
            case model.page of
                GamesPage gamesPageModel ->
                    Games.subscriptions gamesPageModel
                        |> Sub.map GamesPageMsg

                GamePage gamePageModel ->
                    Game.subscriptions gamePageModel
                        |> Sub.map GamePageMsg

                PlayPage playPageModel ->
                    Play.subscriptions playPageModel
                        |> Sub.map PlayPageMsg

                _ ->
                    Sub.none
    in
    Sub.batch
        [ Navbar.subscriptions model.navbarState NavbarMsg
        , pageSub
        , model.myGames |> Maybe.map (MyGames.subscriptions >> Sub.map MyGamesMsg) |> Maybe.withDefault Sub.none
        ]



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }
