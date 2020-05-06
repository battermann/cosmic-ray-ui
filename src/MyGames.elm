port module MyGames exposing (Model, Msg, init, subscriptions, update, view)

import Bootstrap.Alert as Alert
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Spinner as Spinner exposing (spinner)
import Bootstrap.Utilities.Flex as Flex
import Bootstrap.Utilities.Spacing as Spacing
import Graphql.Document
import Graphql.Operation exposing (RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Html
import Html.Attributes
import Json.Decode as Decode
import Mappings as M
import ReadModel.Enum.Color_enum as ColorEnum exposing (Color_enum)
import ReadModel.Enum.Game_state_enum as GsEnum
import ReadModel.InputObject exposing (Game_state_enum_comparison_exp, Games_bool_exp(..), buildGame_state_enum_comparison_exp, buildGames_bool_exp)
import ReadModel.Object
import ReadModel.Object.Challenges
import ReadModel.Object.Games
import ReadModel.Subscription as Subscription
import RemoteData exposing (RemoteData)
import Svg
import Svg.Attributes
import Types.ClientId exposing (ClientId(..))
import Types.Color as Color exposing (Color)
import Types.Column exposing (Column(..))
import Types.Game as Game exposing (Game)
import Types.GameId exposing (GameId(..))
import Types.GameState exposing (GameState(..))
import Url.Builder



---- MODEL ----


type alias Challenge =
    { gameId : GameId
    , serialId : Int
    , color : Color
    }


type alias Model =
    { challenges : RemoteData () (List Challenge)
    , games : RemoteData () (List Game)
    , clientId : ClientId
    }



---- GRAPHQL ----
---- challenges ----


mapColor : Color_enum -> Color
mapColor enum =
    case enum of
        ColorEnum.Red ->
            Color.Red

        ColorEnum.Yellow ->
            Color.Yellow


challengesSelection : SelectionSet Challenge ReadModel.Object.Challenges
challengesSelection =
    SelectionSet.map3
        Challenge
        (SelectionSet.map GameId ReadModel.Object.Challenges.id)
        ReadModel.Object.Challenges.serial_id
        (SelectionSet.map mapColor ReadModel.Object.Challenges.color)


challegesSub : ClientId -> SelectionSet (List Challenge) RootSubscription
challegesSub (ClientId clientId) =
    Subscription.my_challenges identity { args = { my_id = Present clientId } } challengesSelection


makeChallengesSubscription : ClientId -> Cmd msg
makeChallengesSubscription clientId =
    createMyChallengesSubscription (challegesSub clientId |> Graphql.Document.serializeSubscription)



---- game ----


gameSelection : SelectionSet Game ReadModel.Object.Games
gameSelection =
    SelectionSet.map5
        Game
        (SelectionSet.map GameId ReadModel.Object.Games.id)
        ReadModel.Object.Games.serial_id
        (SelectionSet.map (List.map Column) ReadModel.Object.Games.moves)
        (SelectionSet.map M.mapGameState ReadModel.Object.Games.game_state)
        (SelectionSet.map M.mapPlayer ReadModel.Object.Games.player)


inProgress : OptionalArgument Game_state_enum_comparison_exp
inProgress =
    Present (buildGame_state_enum_comparison_exp (\args -> { args | eq_ = Present GsEnum.In_progress }))


gamesSub : ClientId -> SelectionSet (List Game) RootSubscription
gamesSub (ClientId clientId) =
    Subscription.my_games
        (\optionals ->
            { optionals
                | where_ =
                    Present (buildGames_bool_exp (\fields -> { fields | game_state = inProgress }))
            }
        )
        { args = { my_id = Present clientId } }
        gameSelection


makeGamesSubscription : ClientId -> Cmd msg
makeGamesSubscription clientId =
    createMyGamesSubscription (gamesSub clientId |> Graphql.Document.serializeSubscription)



---- UPDATE ----


type Msg
    = ChallengesSubscriptionResponse Decode.Value
    | GamesSubscriptionResponse Decode.Value


init : ClientId -> ( Model, Cmd Msg )
init clientId =
    ( { challenges = RemoteData.Loading
      , games = RemoteData.Loading
      , clientId = clientId
      }
    , Cmd.batch [ makeChallengesSubscription clientId, makeGamesSubscription clientId ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChallengesSubscriptionResponse value ->
            ( { model
                | challenges =
                    Decode.decodeValue (challegesSub model.clientId |> Graphql.Document.decoder) value
                        |> RemoteData.fromResult
                        |> RemoteData.mapError (always ())
              }
            , Cmd.none
            )

        GamesSubscriptionResponse value ->
            ( { model
                | games =
                    Decode.decodeValue (gamesSub model.clientId |> Graphql.Document.decoder) value
                        |> RemoteData.fromResult
                        |> RemoteData.mapError (always ())
              }
            , Cmd.none
            )



---- VIEW ----


coloredCircle : Color -> Html.Html Msg
coloredCircle color =
    case color of
        Color.Red ->
            red

        Color.Yellow ->
            yellow


viewCircle : String -> Html.Html Msg
viewCircle color =
    Svg.svg
        [ Svg.Attributes.width "30"
        , Svg.Attributes.height "30"
        , Svg.Attributes.viewBox "0 0 100 100"
        ]
        [ Svg.circle
            [ Svg.Attributes.cx "50"
            , Svg.Attributes.cy "50"
            , Svg.Attributes.r "50"
            , Svg.Attributes.fill color
            ]
            []
        ]


red : Html.Html Msg
red =
    viewCircle "red"


yellow : Html.Html Msg
yellow =
    viewCircle "yellow"


viewBlock : String -> Html.Html Msg -> Card.Config Msg
viewBlock title content =
    Card.config [ Card.attrs [] ]
        |> Card.headerH4 []
            [ Html.text title ]
        |> Card.block []
            [ Block.text []
                [ content ]
            ]


muted : Html.Html Msg -> Html.Html Msg
muted =
    List.singleton >> Html.p [ Html.Attributes.class "text-muted" ]


viewChallenges : RemoteData () (List Challenge) -> Html.Html Msg
viewChallenges remoteData =
    case remoteData of
        RemoteData.Success challenges ->
            if List.isEmpty challenges then
                Html.text "You currently have no challenges" |> muted

            else
                ListGroup.custom
                    (challenges |> List.map viewChallenge)

        RemoteData.Loading ->
            Spinner.spinner [] []

        RemoteData.NotAsked ->
            Html.text "No data" |> muted

        RemoteData.Failure _ ->
            Alert.simpleDanger [] [ Html.text "Failed to load data" ]


viewChallenge : Challenge -> ListGroup.CustomItem Msg
viewChallenge challenge =
    ListGroup.anchor
        [ ListGroup.attrs [ Html.Attributes.href (Url.Builder.absolute [ "games", Types.GameId.toString challenge.gameId ] []), Spacing.mt2 ], ListGroup.light ]
        [ Html.div [ Flex.block, Flex.row, Flex.justifyBetween ]
            [ Html.text <| "Game #" ++ String.fromInt challenge.serialId ++ " - Waiting for opponent..."
            , coloredCircle challenge.color
            ]
        ]


viewGame : Game -> ListGroup.CustomItem Msg
viewGame game =
    ListGroup.anchor
        [ ListGroup.attrs [ Html.Attributes.href (Url.Builder.absolute [ "games", Types.GameId.toString game.id ] []), Spacing.mt2 ], ListGroup.light ]
        [ Html.div [ Flex.block, Flex.row, Flex.justifyBetween ]
            [ Html.div []
                [ [ "Game #" ++ String.fromInt game.serialId
                  , "(Moves: " ++ String.fromInt (List.length game.moves) ++ ")"
                  , Game.info game
                  ]
                    |> String.join " - "
                    |> Html.text
                ]
            , Html.i [ Html.Attributes.class "fas fa-gamepad" ] []
            ]
        ]


viewGames : RemoteData () (List Game) -> Html.Html Msg
viewGames remoteData =
    case remoteData of
        RemoteData.Success games ->
            if List.isEmpty games then
                Html.text "You currently have no running games" |> muted

            else
                ListGroup.custom
                    (games |> List.map viewGame)

        RemoteData.Loading ->
            Spinner.spinner [] []

        RemoteData.NotAsked ->
            Html.text "No data" |> muted

        RemoteData.Failure _ ->
            Html.text "Failed to load data" |> muted


view : Model -> Html.Html Msg
view model =
    Card.deck
        [ viewBlock "Your Challenges" (viewChallenges model.challenges)
        , viewBlock "Your Running Games" (viewGames model.games)
        ]



---- SUBSCRIPTION ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [ myChallengesReceived ChallengesSubscriptionResponse, myGamesReceived GamesSubscriptionResponse ]



---- PORTS ----


port createMyChallengesSubscription : String -> Cmd msg


port myChallengesReceived : (Decode.Value -> msg) -> Sub msg


port createMyGamesSubscription : String -> Cmd msg


port myGamesReceived : (Decode.Value -> msg) -> Sub msg
