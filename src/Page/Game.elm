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
import List.Extra
import Mappings as M
import Maybe.Extra
import ReadModel.Enum.Color_enum as ColorEnum exposing (Color_enum)
import ReadModel.Object
import ReadModel.Object.Challenges
import ReadModel.Object.Games
import ReadModel.Query as Query
import ReadModel.Scalar exposing (Uuid(..))
import ReadModel.Subscription as Subscription
import RemoteData exposing (RemoteData)
import Svg
import Svg.Attributes
import Types.ClientId exposing (ClientId(..))
import Types.CmdEndpoint exposing (CmdEndpoint(..))
import Types.Color as Color exposing (Color)
import Types.Column as Player exposing (Column(..))
import Types.Game as Game exposing (Game)
import Types.GameId exposing (GameId(..))
import Types.GameState as GS exposing (GameState(..))
import Types.Player as Player exposing (Player(..))
import Types.QueryEndpoint exposing (QueryEndpoint(..))
import UUID exposing (toString)
import Url.Builder



---- MODEL ----


type alias Board =
    Array (Array Player)


type alias Challenge =
    { gameId : GameId
    , serialId : Int
    , color : Color
    }


empty : Board
empty =
    Array.repeat 7 Array.empty


size : Board -> Int
size =
    Array.map Array.length >> Array.foldl (+) 0


type alias GameModel =
    { board : Board
    , selectedColumn : Maybe Column
    , lastMove : Maybe Column
    , player : Player
    }


type GameState
    = Draw GameModel
    | YellowWon GameModel
    | RedWon GameModel
    | InProgress GameModel
    | WaitingForOpponent Player
    | Observing Board (Maybe Column)


type alias Model =
    { gameId : GameId
    , clientId : ClientId
    , queryEndpoint : QueryEndpoint
    , gameStateData : RemoteData () GameState
    , modalVisibility : Modal.Visibility
    , info : Maybe String
    , title : String
    }


isLegalMove : Column -> Player -> Board -> Bool
isLegalMove (Column column) player board =
    let
        isColumnFull =
            Array.get column board
                |> Maybe.map (Array.length >> (==) 6)
                |> Maybe.withDefault False
    in
    case player of
        Yellow ->
            modBy 2 (size board) == 0 && not isColumnFull

        Red ->
            modBy 2 (size board) == 1 && not isColumnFull

        None ->
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


addMove : Column -> Board -> Board
addMove (Column col) board =
    if (size board |> modBy 2) == 0 then
        board |> Array.Extra.update col (Array.push Player.Yellow)

    else
        board |> Array.Extra.update col (Array.push Player.Red)


toPlayer : Color -> Player
toPlayer color =
    case color of
        Color.Red ->
            Red

        Color.Yellow ->
            Yellow



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


challengesSubscription : ClientId -> SelectionSet (List Challenge) RootSubscription
challengesSubscription (ClientId clientId) =
    Subscription.my_challenges identity { args = { my_id = Present clientId } } challengesSelection



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


query : GameId -> ClientId -> SelectionSet (List Game) RootQuery
query (GameId gameId) (ClientId clientId) =
    Query.game identity { args = { game_id = Present gameId, my_id = Present clientId } } gameSelection


gamesSubscription : GameId -> ClientId -> SelectionSet (List Game) RootSubscription
gamesSubscription (GameId gameId) (ClientId clientId) =
    Subscription.game identity { args = { game_id = Present gameId, my_id = Present clientId } } gameSelection


makeRequest : QueryEndpoint -> GameId -> ClientId -> Cmd Msg
makeRequest (QueryEndpoint url) gameId clientId =
    query gameId clientId
        |> Graphql.Http.queryRequest url
        |> Graphql.Http.send GameQueryResponse



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
    = GameQueryResponse (Result (Graphql.Http.Error (List Game)) (List Game))
    | ChallengesSubscriptionResponse (Result Decode.Error (List Challenge))
    | GameSubscriptionResponse (Result Decode.Error (List Game))
    | Played Column
    | ColumnHover (Maybe Column)
    | PlayResult (Result Http.Error ())
    | CloseModal


init : QueryEndpoint -> GameId -> ClientId -> ( Model, Cmd Msg )
init queryEndpoint gameId clientId =
    ( { gameStateData = RemoteData.NotAsked
      , gameId = gameId
      , clientId = clientId
      , queryEndpoint = queryEndpoint
      , modalVisibility = Modal.hidden
      , info = Just "Loading …"
      , title = ""
      }
    , Cmd.batch
        [ makeRequest queryEndpoint gameId clientId
        , createGameSubscription (gamesSubscription gameId clientId |> Graphql.Document.serializeSubscription)
        , createChallengesSubscriptionGame (challengesSubscription clientId |> Graphql.Document.serializeSubscription)
        ]
    )


updateGame : Game -> Model -> ( Model, Cmd Msg )
updateGame game model =
    let
        mkGame state modalVisibility =
            { model
                | gameStateData =
                    if Game.isMine game then
                        { board = mkBoard game.moves
                        , selectedColumn = Nothing
                        , player = game.color
                        , lastMove = game.moves |> List.Extra.last
                        }
                            |> state
                            |> RemoteData.succeed

                    else
                        Observing (mkBoard game.moves) (game.moves |> List.Extra.last)
                            |> RemoteData.succeed
                , modalVisibility = modalVisibility
                , info = Just (Game.info game)
                , title =
                    if Game.isMine game then
                        "You Are Playing (Game #" ++ String.fromInt game.serialId ++ ")"

                    else
                        "Watching (Game #" ++ String.fromInt game.serialId ++ ")"
            }
    in
    case ( model.gameStateData, game.state ) of
        ( RemoteData.Success (InProgress _), GS.Draw ) ->
            ( mkGame Draw Modal.shown, Cmd.none )

        ( _, GS.Draw ) ->
            ( mkGame Draw Modal.hidden, Cmd.none )

        ( RemoteData.Success (InProgress _), GS.RedWon ) ->
            ( mkGame RedWon Modal.shown, Cmd.none )

        ( _, GS.RedWon ) ->
            ( mkGame RedWon Modal.hidden, Cmd.none )

        ( RemoteData.Success (InProgress _), GS.YellowWon ) ->
            ( mkGame YellowWon Modal.shown, Cmd.none )

        ( _, GS.YellowWon ) ->
            ( mkGame YellowWon Modal.hidden, Cmd.none )

        ( _, GS.InProgress ) ->
            ( mkGame InProgress Modal.hidden, Cmd.none )


update : CmdEndpoint -> Msg -> Model -> ( Model, Cmd Msg )
update cmdEndpoint msg model =
    case msg of
        GameQueryResponse (Ok [ game ]) ->
            updateGame game model

        GameQueryResponse _ ->
            ( model, Cmd.none )

        ChallengesSubscriptionResponse (Ok myChallenges) ->
            let
                maybeChallenge =
                    myChallenges |> List.Extra.find (\challenge -> challenge.gameId == model.gameId)
            in
            case maybeChallenge of
                Just challenge ->
                    ( { model
                        | gameStateData = RemoteData.succeed (WaitingForOpponent (toPlayer challenge.color))
                        , info = Just "Waiting for opponent …"
                        , title = "Waiting For Opponent (Challenge #" ++ String.fromInt challenge.serialId ++ ")"
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        ChallengesSubscriptionResponse _ ->
            ( model, Cmd.none )

        GameSubscriptionResponse (Ok [ game ]) ->
            updateGame game model

        GameSubscriptionResponse _ ->
            ( model, Cmd.none )

        Played column ->
            case model.gameStateData of
                RemoteData.Success (InProgress game) ->
                    if isLegalMove column game.player game.board then
                        ( { model
                            | gameStateData =
                                InProgress
                                    { board = addMove column game.board
                                    , selectedColumn = Nothing
                                    , player = game.player
                                    , lastMove = Just column
                                    }
                                    |> RemoteData.succeed
                            , info = Nothing
                          }
                        , play cmdEndpoint model.gameId column model.clientId
                        )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ColumnHover column ->
            case model.gameStateData of
                RemoteData.Success (InProgress game) ->
                    ( { model | gameStateData = InProgress { game | selectedColumn = column } |> RemoteData.succeed }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

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
viewBlock title content =
    Card.config [ Card.attrs [ Spacing.mt3 ] ]
        |> Card.headerH4 [] [ title ]
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


columnPlayableAttributes : Column -> Player -> Board -> List (Html.Attribute Msg)
columnPlayableAttributes column color board =
    if isLegalMove column color board then
        [ Html.Events.onMouseEnter (ColumnHover (Just column))
        , Html.Events.onMouseLeave (ColumnHover Nothing)
        , Html.Attributes.style "cursor" "pointer"
        ]

    else
        []


viewEmptyBoard : List (Html Msg)
viewEmptyBoard =
    empty
        |> Array.map (\col -> col |> Array.toList |> List.reverse |> List.append (List.repeat (6 - Array.length col) None))
        |> Array.toList
        |> List.indexedMap
            (\i col ->
                Html.div
                    [ Flex.block
                    , Flex.col
                    , Flex.justifyCenter
                    , Flex.alignItemsCenter
                    , Size.h100
                    , Html.Events.onClick (Played (Column i))
                    ]
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
                                    [ viewCell (cornersAttributes i j) False False cell
                                    ]
                            )
                    )
            )


viewBoard : Board -> Maybe Column -> Player -> Maybe ( Int, Int ) -> List (Html Msg)
viewBoard board selectedColumn color lastMove =
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
                        ++ columnPlayableAttributes (Column i) color board
                    )
                    (col
                        |> List.indexedMap
                            (\j cell ->
                                Html.div
                                    [ Html.Attributes.style "width" "10vmin"
                                    , Html.Attributes.style "height" "10vmin"
                                    , Html.Attributes.style "user-select" "none"
                                    , Html.Attributes.style "max-width" "120px"
                                    , Html.Attributes.style "max-height" "120px"
                                    , Html.Attributes.style "min-width" "40px"
                                    , Html.Attributes.style "min-height" "40px"
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
                                        (lastMove == Just ( i, j ))
                                        cell
                                    ]
                            )
                    )
            )


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


viewInfo : Maybe String -> Html.Html Msg
viewInfo =
    Maybe.Extra.unwrap (Spinner.spinner [] []) Html.text


coloredCircle : Player -> Html.Html Msg
coloredCircle color =
    case color of
        Red ->
            red

        Yellow ->
            yellow

        None ->
            Html.text ""


viewModal : Model -> Html Msg
viewModal model =
    Modal.config CloseModal
        |> Modal.small
        |> Modal.h4 [] [ Html.text "Game finished" ]
        |> Modal.body [] [ Html.h5 [] [ viewInfo model.info ] ]
        |> Modal.view model.modalVisibility


coordinatesLastMove : Board -> Column -> Maybe ( Int, Int )
coordinatesLastMove board (Column columnIndex) =
    board |> Array.get columnIndex |> Maybe.map (\column -> ( columnIndex, 6 - Array.length column ))


viewGame : Model -> Html.Html Msg
viewGame model =
    let
        viewGameAndInfo icon boardView player =
            Html.div []
                [ viewBlock
                    (Html.div [ Flex.block, Flex.row, Flex.justifyBetween ]
                        [ Html.div [ Flex.block, Flex.row, Flex.justifyBetween ] [ Html.div [ Spacing.mr3 ] [ Html.i [ Html.Attributes.class icon ] [] ], Html.text model.title ]
                        , coloredCircle player
                        ]
                    )
                    (Grid.container []
                        [ Grid.row []
                            [ Grid.col [] [ Html.div [ Spacing.mb4, Flex.block, Flex.row, Flex.justifyCenter, Flex.alignItemsCenter ] boardView ]
                            , Grid.col [] [ [ viewInfo model.info ] |> Html.h5 [] ]
                            ]
                        ]
                    )
                ]

        renderBoard game =
            viewBoard game.board game.selectedColumn game.player (game.lastMove |> Maybe.andThen (coordinatesLastMove game.board))
    in
    case model.gameStateData of
        RemoteData.NotAsked ->
            Spinner.spinner [] []

        RemoteData.Loading ->
            Spinner.spinner [] []

        RemoteData.Failure _ ->
            Html.text "failure"

        RemoteData.Success (Observing board lastMove) ->
            viewGameAndInfo
                "far fa-eye"
                (viewBoard board Nothing None (lastMove |> Maybe.andThen (coordinatesLastMove board)))
                None

        RemoteData.Success (WaitingForOpponent player) ->
            viewGameAndInfo "fas fa-gamepad" viewEmptyBoard player

        RemoteData.Success (Draw game) ->
            viewGameAndInfo "fas fa-gamepad" (renderBoard game) game.player

        RemoteData.Success (YellowWon game) ->
            viewGameAndInfo "fas fa-gamepad" (renderBoard game) game.player

        RemoteData.Success (RedWon game) ->
            viewGameAndInfo "fas fa-gamepad" (renderBoard game) game.player

        RemoteData.Success (InProgress game) ->
            viewGameAndInfo "fas fa-gamepad" (renderBoard game) game.player


view : Model -> Html.Html Msg
view model =
    Html.div [] [ viewGame model, viewModal model ]



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.gameStateData of
        RemoteData.Success _ ->
            Sub.batch
                [ gameReceived (Decode.decodeValue (gamesSubscription model.gameId model.clientId |> Graphql.Document.decoder) >> GameSubscriptionResponse)
                ]

        _ ->
            Sub.batch
                [ gameReceived (Decode.decodeValue (gamesSubscription model.gameId model.clientId |> Graphql.Document.decoder) >> GameSubscriptionResponse)
                , challengesReceivedGame (Decode.decodeValue (challengesSubscription model.clientId |> Graphql.Document.decoder) >> ChallengesSubscriptionResponse)
                ]



---- PORTS ----


port createGameSubscription : String -> Cmd msg


port createChallengesSubscriptionGame : String -> Cmd msg


port gameReceived : (Decode.Value -> msg) -> Sub msg


port challengesReceivedGame : (Decode.Value -> msg) -> Sub msg
