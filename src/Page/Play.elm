port module Page.Play exposing (Model, Msg, init, subscriptions, update, view)

import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form as Form
import Bootstrap.Form.Select as Select
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Spinner as Spinner exposing (spinner)
import Bootstrap.Utilities.Flex as Flex
import Bootstrap.Utilities.Spacing as Spacing
import Browser.Navigation as Nav
import Graphql.Document
import Graphql.Http
import Graphql.Operation exposing (RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Html
import Html.Attributes
import Html.Events
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import ReadModel.Enum.Color_enum as ColorEnum exposing (Color_enum)
import ReadModel.Object
import ReadModel.Object.Challenges
import ReadModel.Query as Query
import ReadModel.Subscription as Subscription
import RemoteData exposing (RemoteData)
import Svg
import Svg.Attributes
import Types.ClientId exposing (ClientId(..))
import Types.CmdEndpoint exposing (CmdEndpoint(..))
import Types.Color exposing (Color(..))
import Types.GameId exposing (GameId(..))
import Types.QueryEndpoint exposing (QueryEndpoint(..))
import UUID
import Url.Builder



---- MODEL ----


type alias Challenge =
    { gameId : GameId
    , serialId : Int
    , color : Color
    }


type alias Model =
    { clientId : ClientId
    , key : Nav.Key
    , selectedColor : Color
    , challenges : RemoteData () (List Challenge)
    , play : RemoteData () ()
    , accept : RemoteData () ()
    }



---- GRAPHQL ----


mapColor : Color_enum -> Color
mapColor enum =
    case enum of
        ColorEnum.Red ->
            Red

        ColorEnum.Yellow ->
            Yellow


selection : SelectionSet Challenge ReadModel.Object.Challenges
selection =
    SelectionSet.map3
        Challenge
        (SelectionSet.map GameId ReadModel.Object.Challenges.id)
        ReadModel.Object.Challenges.serial_id
        (SelectionSet.map mapColor ReadModel.Object.Challenges.color)


query : ClientId -> SelectionSet (List Challenge) RootQuery
query (ClientId clientId) =
    Query.villain_challenges identity { args = { my_id = Present clientId } } selection


sub : ClientId -> SelectionSet (List Challenge) RootSubscription
sub (ClientId clientId) =
    Subscription.villain_challenges identity { args = { my_id = Present clientId } } selection


makeRequest : QueryEndpoint -> ClientId -> Cmd Msg
makeRequest (QueryEndpoint url) clientId =
    query clientId
        |> Graphql.Http.queryRequest url
        |> Graphql.Http.send (RemoteData.fromResult >> QueryResponse)



---- HTTP ----


type alias NewGameRequest =
    { clientId : ClientId
    , color : Color
    }


gameCreatedDecoder : Decode.Decoder GameId
gameCreatedDecoder =
    Decode.field "gameId" Decode.string
        |> Decode.andThen
            (\v ->
                case UUID.fromString v of
                    Ok uuid ->
                        Decode.succeed (GameId uuid)

                    Err _ ->
                        Decode.fail <| "Couldn't decode game created response. Invalid game id. Expected UUID but got: " ++ v
            )


newGameEncoder : NewGameRequest -> Encode.Value
newGameEncoder { clientId, color } =
    let
        colorStr =
            case color of
                Yellow ->
                    "Yellow"

                Red ->
                    "Red"

        (ClientId uuid) =
            clientId
    in
    Encode.object
        [ ( "color", Encode.string colorStr )
        , ( "clientId", Encode.string (UUID.toString uuid) )
        ]


newGame : CmdEndpoint -> Color -> ClientId -> Cmd Msg
newGame (CmdEndpoint url) color clientId =
    Http.post
        { url = Url.Builder.crossOrigin url [ "games" ] []
        , body = Http.jsonBody (newGameEncoder (NewGameRequest clientId color))
        , expect = Http.expectJson NewGameResult gameCreatedDecoder
        }


acceptEncoder : ClientId -> Encode.Value
acceptEncoder (ClientId clientId) =
    Encode.object [ ( "clientId", Encode.string (UUID.toString clientId) ) ]


accept : CmdEndpoint -> GameId -> ClientId -> Cmd Msg
accept (CmdEndpoint url) (GameId gameId) clientId =
    Http.post
        { url = Url.Builder.crossOrigin url [ "games", UUID.toString gameId, "join" ] []
        , body = Http.jsonBody (acceptEncoder clientId)
        , expect = Http.expectWhatever (AcceptResult (GameId gameId))
        }



---- UPDATE ----


type Msg
    = NewGameResult (Result Http.Error GameId)
    | AcceptResult GameId (Result Http.Error ())
    | Accept GameId
    | ColorSelected String
    | PlaySubmit
    | QueryResponse (RemoteData (Graphql.Http.Error (List Challenge)) (List Challenge))
    | SubscriptionResponse Decode.Value


init : Nav.Key -> QueryEndpoint -> ClientId -> ( Model, Cmd Msg )
init key queryEndpoint clientId =
    ( Model clientId key Yellow RemoteData.Loading RemoteData.NotAsked (RemoteData.Success ())
    , Cmd.batch [ makeRequest queryEndpoint clientId, createChallengesSubscription (sub clientId |> Graphql.Document.serializeSubscription) ]
    )


update : CmdEndpoint -> Msg -> Model -> ( Model, Cmd Msg )
update cmdEndpoint msg model =
    case msg of
        NewGameResult (Err _) ->
            ( { model | play = RemoteData.Failure () }, Cmd.none )

        NewGameResult (Ok gameId) ->
            ( { model | play = RemoteData.NotAsked }, Nav.load <| Url.Builder.absolute [ "games", Types.GameId.toString gameId ] [] )

        Accept gameId ->
            ( { model | accept = RemoteData.Loading }, accept cmdEndpoint gameId model.clientId )

        AcceptResult _ (Err _) ->
            ( { model | accept = RemoteData.Failure () }, Cmd.none )

        AcceptResult gameId (Ok _) ->
            ( { model
                | accept = RemoteData.Success ()
                , challenges = model.challenges |> RemoteData.map (List.filter (.gameId >> (==) gameId >> not))
              }
            , Nav.pushUrl model.key (Url.Builder.absolute [ "games", Types.GameId.toString gameId ] [])
            )

        PlaySubmit ->
            ( { model | play = RemoteData.Loading }, newGame cmdEndpoint model.selectedColor model.clientId )

        ColorSelected "Yellow" ->
            ( { model | selectedColor = Yellow }, Cmd.none )

        ColorSelected "Red" ->
            ( { model | selectedColor = Red }, Cmd.none )

        ColorSelected _ ->
            ( model, Cmd.none )

        QueryResponse response ->
            ( { model | challenges = response |> RemoteData.mapError (always ()) }, Cmd.none )

        SubscriptionResponse value ->
            ( { model
                | challenges =
                    Decode.decodeValue (sub model.clientId |> Graphql.Document.decoder)
                        value
                        |> RemoteData.fromResult
                        |> RemoteData.mapError (always ())
              }
            , Cmd.none
            )



---- VIEW ----


muted : Html.Html Msg -> Html.Html Msg
muted =
    List.singleton >> Html.p [ Html.Attributes.class "text-muted" ]


viewBlock : String -> Html.Html Msg -> Card.Config Msg
viewBlock title content =
    Card.config [ Card.attrs [] ]
        |> Card.headerH4 [] [ Html.text title ]
        |> Card.block []
            [ Block.text []
                [ content ]
            ]


colorsWithLabels : List ( Color, String )
colorsWithLabels =
    [ ( Yellow, "Yellow" ), ( Red, "Red" ) ]


viewPlayForm : Model -> Html.Html Msg
viewPlayForm model =
    case model.play of
        RemoteData.NotAsked ->
            Form.form [ Html.Events.onSubmit PlaySubmit ]
                [ Form.group []
                    [ Form.label [ Html.Attributes.for "color" ] [ Html.text "Choose Your Color" ]
                    , Select.custom
                        [ Select.id "color"
                        , Select.onChange ColorSelected
                        ]
                        (colorsWithLabels
                            |> List.map
                                (\( c, l ) ->
                                    Select.item
                                        [ Html.Attributes.selected (model.selectedColor == c)
                                        ]
                                        [ Html.text l ]
                                )
                        )
                    , Form.help [] [ Html.text "Yellow always plays first." ]
                    ]
                , Button.button [ Button.primary ]
                    [ Html.i [ Html.Attributes.class "fas fa-plus" ] []
                    , Html.text " Create"
                    ]
                ]

        RemoteData.Loading ->
            Spinner.spinner [] []

        _ ->
            Html.text ""


coloredCircle : Color -> Html.Html Msg
coloredCircle color =
    case color of
        Red ->
            red

        Yellow ->
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


viewChallenge : Challenge -> ListGroup.Item Msg
viewChallenge challenge =
    ListGroup.li
        [ ListGroup.attrs [ Spacing.mt2 ], ListGroup.light ]
        [ Html.div [ Flex.block, Flex.row, Flex.justifyBetween ]
            [ Html.div []
                [ Button.button [ Button.success, Button.small, Button.onClick (Accept challenge.gameId), Button.attrs [ Spacing.mr2 ] ] [ Html.text "Accept" ]
                , Html.text ("Challenge #" ++ String.fromInt challenge.serialId)
                ]
            , coloredCircle challenge.color
            ]
        ]


viewChallenges : Model -> Html.Html Msg
viewChallenges model =
    case model.accept |> RemoteData.andThen (always model.challenges) of
        RemoteData.Success challenges ->
            if List.isEmpty challenges then
                Html.text "There are currently no challenges" |> muted

            else
                ListGroup.ul
                    (challenges |> List.map viewChallenge)

        RemoteData.Loading ->
            Spinner.spinner [] []

        RemoteData.NotAsked ->
            Html.text "No data" |> muted

        RemoteData.Failure _ ->
            Alert.simpleDanger [] [ Html.text "Failed to load data" ]


view : Model -> Html.Html Msg
view model =
    Card.deck
        [ viewBlock "Create Challenge" (viewPlayForm model)
        , viewBlock "Challenges From Other Players" (viewChallenges model)
        ]



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    challengesReceived SubscriptionResponse



---- PORTS ----


port createChallengesSubscription : String -> Cmd msg


port challengesReceived : (Decode.Value -> msg) -> Sub msg
