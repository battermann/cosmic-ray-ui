port module Page.Games exposing (Model, Msg, init, subscriptions, update, view)

import Bootstrap.Alert as Alert
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Spinner as Spinner exposing (spinner)
import Bootstrap.Utilities.Flex as Flex
import Bootstrap.Utilities.Spacing as Spacing
import Graphql.Document
import Graphql.Http
import Graphql.Operation exposing (RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Html
import Html.Attributes
import Json.Decode as Decode
import Mappings as M
import ReadModel.Enum.Game_state_enum as GsEnum
import ReadModel.InputObject exposing (Game_state_enum_comparison_exp, Games_bool_exp(..), buildGame_state_enum_comparison_exp, buildGames_bool_exp)
import ReadModel.Object
import ReadModel.Object.Games
import ReadModel.Query as Query
import ReadModel.Scalar exposing (Uuid(..))
import ReadModel.Subscription as Subscription
import RemoteData exposing (RemoteData)
import Types.ClientId exposing (ClientId(..))
import Types.Column exposing (Column(..))
import Types.Game as Game exposing (Game)
import Types.GameId exposing (GameId(..))
import Types.GameState exposing (GameState(..))
import Types.Player exposing (Player(..))
import Types.QueryEndpoint exposing (QueryEndpoint(..))
import Url.Builder



---- MODEL ----


type alias Model =
    { games : RemoteData () (List Game)
    , clientId : ClientId
    }



---- GRAPHQL ----


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


query : ClientId -> SelectionSet (List Game) RootQuery
query (ClientId clientId) =
    Query.live_games
        (\optionals ->
            { optionals
                | where_ =
                    Present (buildGames_bool_exp (\fields -> { fields | game_state = inProgress }))
            }
        )
        { args = { my_id = Present clientId } }
        gameSelection


sub : ClientId -> SelectionSet (List Game) RootSubscription
sub (ClientId clientId) =
    Subscription.live_games
        (\optionals ->
            { optionals
                | where_ =
                    Present (buildGames_bool_exp (\fields -> { fields | game_state = inProgress }))
            }
        )
        { args = { my_id = Present clientId } }
        gameSelection


makeRequest : QueryEndpoint -> ClientId -> Cmd Msg
makeRequest (QueryEndpoint url) clientId =
    query clientId
        |> Graphql.Http.queryRequest url
        |> Graphql.Http.send (RemoteData.fromResult >> QueryResponse)



---- UPDATE ----


init : QueryEndpoint -> ClientId -> ( Model, Cmd Msg )
init queryEndpoint clientId =
    ( { games = RemoteData.Loading
      , clientId = clientId
      }
    , Cmd.batch [ makeRequest queryEndpoint clientId, createGamesSubscription (sub clientId |> Graphql.Document.serializeSubscription) ]
    )


type Msg
    = QueryResponse (RemoteData (Graphql.Http.Error (List Game)) (List Game))
    | SubscriptionResponse Decode.Value


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        QueryResponse response ->
            ( { model
                | games =
                    response
                        |> RemoteData.mapError (always ())
              }
            , Cmd.none
            )

        SubscriptionResponse value ->
            ( { model
                | games =
                    Decode.decodeValue (sub model.clientId |> Graphql.Document.decoder) value
                        |> RemoteData.fromResult
                        |> RemoteData.mapError (always ())
              }
            , Cmd.none
            )



---- VIEW ----


viewGame : Game -> ListGroup.CustomItem Msg
viewGame game =
    ListGroup.anchor
        [ ListGroup.attrs [ Html.Attributes.href (Url.Builder.absolute [ "games", Types.GameId.toString game.id ] []), Spacing.mt2 ], ListGroup.dark ]
        [ Html.div [ Flex.block, Flex.justifyBetween ]
            [ Html.div []
                [ [ "Game #" ++ String.fromInt game.serialId
                  , "(Moves: " ++ String.fromInt (List.length game.moves) ++ ")"
                  , Game.info game
                  ]
                    |> String.join " - "
                    |> Html.text
                ]
            , Html.i [ Html.Attributes.class "far fa-eye" ] []
            ]
        ]


viewBlock : String -> Html.Html Msg -> Html.Html Msg
viewBlock title content =
    Card.config [ Card.attrs [ Spacing.mt3 ] ]
        |> Card.headerH4 [] [ Html.text title ]
        |> Card.block []
            [ Block.text []
                [ content ]
            ]
        |> Card.view


viewGames : List Game -> Html.Html Msg
viewGames games =
    ListGroup.custom
        (games |> List.map viewGame)
        |> viewBlock (String.fromInt (List.length games) ++ " Live Games")


view : Model -> Html.Html Msg
view model =
    case model.games of
        RemoteData.NotAsked ->
            viewBlock "0 Live Games" <| Html.text "No data"

        RemoteData.Loading ->
            viewBlock "0 Live Games" <| Spinner.spinner [] []

        RemoteData.Failure _ ->
            viewBlock "0 Live Games" <| Alert.simpleDanger [] [ Html.text "Failed to load games" ]

        RemoteData.Success games ->
            viewGames games



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    gamesReceived SubscriptionResponse



---- PORTS ----


port createGamesSubscription : String -> Cmd msg


port gamesReceived : (Decode.Value -> msg) -> Sub msg
