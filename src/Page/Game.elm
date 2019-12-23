port module Page.Game exposing (Model, Msg, init, subscriptions, update, view)

import Array exposing (Array)
import Array.Extra
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Grid as Grid
import Bootstrap.Modal as Modal
import Bootstrap.Spinner as Spinner exposing (spinner)
import Bootstrap.Utilities.Flex as Flex
import Bootstrap.Utilities.Size as Size
import Bootstrap.Utilities.Spacing as Spacing
import Graphql.Document
import Graphql.Http
import Graphql.Operation exposing (RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Html exposing (Attribute, Html)
import Html.Attributes
import Html.Events
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Mappings as M
import Maybe.Extra
import ReadModel.Object
import ReadModel.Object.Games
import ReadModel.Query as Query
import ReadModel.Scalar exposing (Uuid(..))
import ReadModel.Subscription as Subscription
import RemoteData exposing (RemoteData)
import Svg
import Svg.Attributes
import Types.ClientId exposing (ClientId(..))
import Types.CmdEndpoint exposing (CmdEndpoint(..))
import Types.Column as Player exposing (Column(..))
import Types.Game as Game exposing (Game)
import Types.GameId exposing (GameId(..))
import Types.GameState exposing (GameState(..))
import Types.Player as Player exposing (Player(..))
import Types.QueryEndpoint exposing (QueryEndpoint(..))
import UUID exposing (toString)
import Url.Builder



---- MODEL ----


type alias Board =
    Array (Array Player)


empty : Board
empty =
    Array.repeat 7 Array.empty


type EvaluationState
    = Loading
    | Valid


type alias Model =
    { game : RemoteData () Game
    , selectedColumn : Maybe Column
    , gameId : GameId
    , clientId : ClientId
    , evalState : EvaluationState
    , modalVisibility : Modal.Visibility
    }


isLegalMove : Column -> Game -> Board -> Bool
isLegalMove (Column column) game board =
    let
        isColumnFull =
            Array.get column board
                |> Maybe.map (Array.length >> (==) 6)
                |> Maybe.withDefault False
    in
    case ( game.color, game.state ) of
        ( Yellow, InProgress ) ->
            modBy 2 (List.length game.moves) == 0 && not isColumnFull

        ( Red, InProgress ) ->
            modBy 2 (List.length game.moves) == 1 && not isColumnFull

        _ ->
            False


mkBoard : List Column -> Board
mkBoard =
    List.indexedMap Tuple.pair
        >> List.foldl
            (\( index, Column col ) board ->
                if (index |> modBy 2) == 0 then
                    board |> Array.Extra.update col (Array.push Player.Yellow)

                else
                    board |> Array.Extra.update col (Array.push Player.Red)
            )
            empty



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


query : GameId -> ClientId -> SelectionSet (List Game) RootQuery
query (GameId gameId) (ClientId clientId) =
    Query.game identity { args = { game_id = Present gameId, my_id = Present clientId } } gameSelection


sub : GameId -> ClientId -> SelectionSet (List Game) RootSubscription
sub (GameId gameId) (ClientId clientId) =
    Subscription.game identity { args = { game_id = Present gameId, my_id = Present clientId } } gameSelection


makeRequest : QueryEndpoint -> GameId -> ClientId -> Cmd Msg
makeRequest (QueryEndpoint url) gameId clientId =
    query gameId clientId
        |> Graphql.Http.queryRequest url
        |> Graphql.Http.send (RemoteData.fromResult >> QueryResponse)



---- HTPP ----


type alias PlayRequest =
    { clientId : ClientId
    , column : Column
    }


playRequestEncoder : PlayRequest -> Encode.Value
playRequestEncoder req =
    let
        (ClientId clientId) =
            req.clientId

        (Column column) =
            req.column
    in
    Encode.object
        [ ( "clientId", Encode.string (UUID.toString clientId) )
        , ( "column", Encode.int column )
        ]


play : CmdEndpoint -> GameId -> Column -> ClientId -> Cmd Msg
play (CmdEndpoint url) (GameId gameId) column clientId =
    Http.post
        { url = Url.Builder.crossOrigin url [ "games", UUID.toString gameId, "play" ] []
        , body = Http.jsonBody (playRequestEncoder (PlayRequest clientId column))
        , expect = Http.expectWhatever PlayResult
        }



---- UPDATE ----


type Msg
    = QueryResponse (RemoteData (Graphql.Http.Error (List Game)) (List Game))
    | Played Column
    | ColumnHover (Maybe Column)
    | SubscriptionResponse Decode.Value
    | PlayResult (Result Http.Error ())
    | CloseModal


init : QueryEndpoint -> GameId -> ClientId -> ( Model, Cmd Msg )
init queryEndpoint gameId clientId =
    ( { game = RemoteData.Loading
      , selectedColumn = Nothing
      , gameId = gameId
      , clientId = clientId
      , evalState = Valid
      , modalVisibility = Modal.shown
      }
    , Cmd.batch [ makeRequest queryEndpoint gameId clientId, createGameSubscription (sub gameId clientId |> Graphql.Document.serializeSubscription) ]
    )


modalVisibility : Game -> Game -> Modal.Visibility
modalVisibility stateA stateB =
    case ( stateA.state, stateB.state ) of
        ( InProgress, Draw ) ->
            Modal.shown

        ( InProgress, RedWon ) ->
            Modal.shown

        ( InProgress, YellowWon ) ->
            Modal.shown

        _ ->
            Modal.hidden


update : CmdEndpoint -> Msg -> Model -> ( Model, Cmd Msg )
update cmdEndpoint msg model =
    case msg of
        QueryResponse response ->
            let
                updatedGame =
                    response
                        |> RemoteData.mapError (always ())
                        |> RemoteData.andThen
                            (List.head >> Maybe.map RemoteData.Success >> Maybe.withDefault (RemoteData.Failure ()))
            in
            ( { model
                | game = updatedGame
                , selectedColumn = Nothing
                , modalVisibility = RemoteData.map2 modalVisibility model.game updatedGame |> RemoteData.withDefault Modal.hidden
              }
            , Cmd.none
            )

        Played column ->
            case model.game of
                RemoteData.Success game ->
                    if isLegalMove column game (mkBoard game.moves) then
                        ( { model
                            | selectedColumn = Nothing
                            , game = RemoteData.Success <| { game | moves = game.moves ++ [ column ] }
                            , evalState = Loading
                          }
                        , play cmdEndpoint model.gameId column model.clientId
                        )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ColumnHover column ->
            ( { model | selectedColumn = column }, Cmd.none )

        SubscriptionResponse value ->
            let
                updatedGame =
                    Decode.decodeValue (sub model.gameId model.clientId |> Graphql.Document.decoder) value
                        |> RemoteData.fromResult
                        |> RemoteData.mapError
                            (always ())
                        |> RemoteData.andThen
                            (List.head >> Maybe.map RemoteData.Success >> Maybe.withDefault (RemoteData.Failure ()))
            in
            ( { model
                | game = updatedGame
                , evalState = Valid
                , modalVisibility = RemoteData.map2 modalVisibility model.game updatedGame |> RemoteData.withDefault Modal.hidden
              }
            , Cmd.none
            )

        PlayResult (Err _) ->
            ( model, Cmd.none )

        PlayResult (Ok _) ->
            ( model, Cmd.none )

        CloseModal ->
            ( { model | modalVisibility = Modal.hidden }, Cmd.none )



---- VIEW ----


viewMarker : Svg.Svg msg
viewMarker =
    Svg.circle
        [ Svg.Attributes.cx "21"
        , Svg.Attributes.cy "21"
        , Svg.Attributes.r "8"
        , Svg.Attributes.fillOpacity "0"
        , Svg.Attributes.stroke "black"
        , Svg.Attributes.strokeWidth "3"
        ]
        []


viewCircle : List (Attribute Msg) -> String -> String -> Bool -> Html Msg
viewCircle corner backgroundColor color marked =
    Svg.svg
        ([ Svg.Attributes.width "100%"
         , Svg.Attributes.height "100%"
         , Svg.Attributes.viewBox "0 0 42 42"
         , Html.Attributes.style "background" backgroundColor
         ]
            ++ corner
        )
        [ Svg.circle
            [ Svg.Attributes.cx "21"
            , Svg.Attributes.cy "21"
            , Svg.Attributes.r "16"
            , Svg.Attributes.fill color
            ]
            []
        , if marked then
            viewMarker

          else
            Svg.text ""
        ]


viewBlock : Html.Html Msg -> Html.Html Msg -> Html.Html Msg
viewBlock titleContent content =
    Card.config [ Card.attrs [ Spacing.mt3 ] ]
        |> Card.header [] [ titleContent ]
        |> Card.block []
            [ Block.text []
                [ content ]
            ]
        |> Card.view


viewCell : List (Attribute Msg) -> Bool -> Bool -> Player -> Html Msg
viewCell corner selected marked player =
    let
        backgroundColor =
            if selected then
                "rgba(0,0,255,0.75)"

            else
                "rgba(0,0,255,1)"
    in
    case player of
        Player.Yellow ->
            viewCircle corner backgroundColor "yellow" marked

        Player.Red ->
            viewCircle corner backgroundColor "red" marked

        Player.None ->
            viewCircle corner backgroundColor "lightgray" marked


cornersAttributes : Int -> Int -> List (Attribute Msg)
cornersAttributes col row =
    let
        corner =
            case ( col, row ) of
                ( 0, 0 ) ->
                    Just "border-top-left-radius"

                ( 0, 5 ) ->
                    Just "border-bottom-left-radius"

                ( 6, 0 ) ->
                    Just "border-top-right-radius"

                ( 6, 5 ) ->
                    Just "border-bottom-right-radius"

                _ ->
                    Nothing
    in
    corner
        |> Maybe.map (\key -> [ Html.Attributes.style key "15px" ])
        |> Maybe.withDefault []


columnPlayableAttributes : Column -> Game -> Board -> List (Html.Attribute Msg)
columnPlayableAttributes column board game =
    if isLegalMove column board game then
        [ Html.Events.onMouseEnter (ColumnHover (Just column))
        , Html.Events.onMouseLeave (ColumnHover Nothing)
        , Html.Attributes.style "cursor" "pointer"
        ]

    else
        []


viewBoard : Board -> Maybe Column -> Game -> List (Html Msg)
viewBoard board selectedColumn game =
    board
        |> Array.map (\col -> col |> Array.toList |> List.reverse |> List.append (List.repeat (6 - Array.length col) None))
        |> Array.toList
        |> List.indexedMap
            (\i col ->
                Html.div
                    ([ Flex.block
                     , Flex.col
                     , Flex.justifyCenter
                     , Flex.alignItemsCenter
                     , Size.h100
                     , Html.Events.onClick (Played (Column i))
                     ]
                        ++ columnPlayableAttributes (Column i) game board
                    )
                    (col
                        |> List.indexedMap
                            (\j cell ->
                                Html.div
                                    [ Html.Attributes.style "width" "10vmin"
                                    , Html.Attributes.style "height" "10vmin"
                                    , Html.Attributes.style "user-select" "none"
                                    , Flex.block
                                    , Flex.col
                                    , Flex.justifyAround
                                    , Flex.alignItemsCenter
                                    ]
                                    [ viewCell
                                        (cornersAttributes i j)
                                        (selectedColumn
                                            |> Maybe.Extra.filter ((==) (Column i))
                                            |> Maybe.Extra.unwrap
                                                False
                                                (always True)
                                        )
                                        (Game.mostRecent game == Just ( i, j ))
                                        cell
                                    ]
                            )
                    )
            )


coloredCircle : Player -> Html.Html Msg
coloredCircle color =
    case color of
        Red ->
            red

        Yellow ->
            yellow

        None ->
            Html.text ""


viewMyColor : String -> Html.Html Msg
viewMyColor color =
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
    viewMyColor "red"


yellow : Html.Html Msg
yellow =
    viewMyColor "yellow"


viewInfo : Game -> Html.Html Msg
viewInfo =
    Game.info >> Html.text >> List.singleton >> Html.h5 []


viewModal : Model -> Html Msg
viewModal model =
    case model.game of
        RemoteData.Success game ->
            Modal.config CloseModal
                |> Modal.small
                |> Modal.h4 [] [ Html.text "Game finished" ]
                |> Modal.body [] [ viewInfo game ]
                |> Modal.view model.modalVisibility

        _ ->
            Html.text ""


view : Model -> Html.Html Msg
view model =
    Html.div [] [ viewGame model, viewModal model ]


viewGame : Model -> Html.Html Msg
viewGame model =
    case model.game of
        RemoteData.NotAsked ->
            Html.text "No data"

        RemoteData.Loading ->
            Html.text "Loading ..."

        RemoteData.Failure _ ->
            Html.text "Loading ..."

        RemoteData.Success game ->
            viewBlock
                (Html.div [ Flex.block, Flex.row, Flex.justifyBetween ]
                    [ if Game.isMine game then
                        Html.h4 [] [ Html.i [ Html.Attributes.class "fas fa-gamepad" ] [], Html.text (" You Are Playing Game #" ++ String.fromInt game.serialId) ]

                      else
                        Html.h4 [] [ Html.i [ Html.Attributes.class "far fa-eye" ] [], Html.text (" Watching Game #" ++ String.fromInt game.serialId) ]
                    , coloredCircle game.color
                    ]
                )
                (Grid.container []
                    [ Grid.row []
                        [ Grid.col []
                            [ Html.div [ Spacing.mb4, Flex.block, Flex.row, Flex.justifyCenter, Flex.alignItemsCenter ]
                                (viewBoard (mkBoard game.moves) model.selectedColumn game)
                            ]
                        , Grid.col []
                            [ case model.evalState of
                                Valid ->
                                    Html.div [] [ viewInfo game ]

                                Loading ->
                                    Spinner.spinner [] []
                            ]
                        ]
                    ]
                )



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    gameReceived SubscriptionResponse



---- PORTS ----


port createGameSubscription : String -> Cmd msg


port gameReceived : (Decode.Value -> msg) -> Sub msg
