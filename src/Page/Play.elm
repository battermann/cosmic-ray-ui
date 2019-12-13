port module Page.Play exposing (Model, Msg, init, subscriptions, update, view)

import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form as Form
import Bootstrap.Form.Select as Select
import Bootstrap.Spinner as Spinner exposing (spinner)
import Bootstrap.Table as Table
import Bootstrap.Utilities.Flex as Flex
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
    , selectedColor : Color
    , challenges : RemoteData () (List Challenge)
    , play : RemoteData () ()
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
        , expect = Http.expectWhatever NewGameResult
        }


acceptEncoder : ClientId -> Encode.Value
acceptEncoder (ClientId clientId) =
    Encode.object [ ( "clientId", Encode.string (UUID.toString clientId) ) ]


accept : CmdEndpoint -> GameId -> ClientId -> Cmd Msg
accept (CmdEndpoint url) (GameId gameId) clientId =
    Http.post
        { url = Url.Builder.crossOrigin url [ "games", UUID.toString gameId, "join" ] []
        , body = Http.jsonBody (acceptEncoder clientId)
        , expect = Http.expectWhatever AcceptResult
        }



---- UPDATE ----


type Msg
    = NewGameResult (Result Http.Error ())
    | AcceptResult (Result Http.Error ())
    | Accept GameId
    | ColorSelected String
    | PlaySubmit
    | QueryResponse (RemoteData (Graphql.Http.Error (List Challenge)) (List Challenge))
    | SubscriptionResponse Decode.Value


init : QueryEndpoint -> ClientId -> ( Model, Cmd Msg )
init queryEndpoint clientId =
    ( Model clientId Yellow RemoteData.Loading RemoteData.NotAsked
    , Cmd.batch [ makeRequest queryEndpoint clientId, createChallengesSubscription (sub clientId |> Graphql.Document.serializeSubscription) ]
    )


update : CmdEndpoint -> Msg -> Model -> ( Model, Cmd Msg )
update cmdEndpoint msg model =
    case msg of
        NewGameResult (Err _) ->
            ( { model | play = RemoteData.Failure () }, Cmd.none )

        NewGameResult (Ok _) ->
            ( { model | play = RemoteData.NotAsked }, Cmd.none )

        Accept gameId ->
            ( model, accept cmdEndpoint gameId model.clientId )

        AcceptResult (Err _) ->
            ( model, Cmd.none )

        AcceptResult (Ok _) ->
            ( model, Cmd.none )

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


viewChallenge : Challenge -> Table.Row Msg
viewChallenge challenge =
    Table.tr []
        [ Table.td [] [ Button.button [ Button.success, Button.small, Button.onClick (Accept challenge.gameId) ] [ Html.text "Accept" ] ]
        , Table.td [] [ Html.text (String.fromInt challenge.serialId) ]
        , Table.td [] [ Html.div [ Flex.block, Flex.justifyAround, Flex.row ] [ coloredCircle challenge.color ] ]
        ]


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


viewChallenges : Model -> Html.Html Msg
viewChallenges model =
    case model.challenges of
        RemoteData.Success challenges ->
            if List.isEmpty challenges then
                Html.text "There are currently no challenges" |> muted

            else
                Table.simpleTable
                    ( Table.simpleThead
                        [ Table.th [] [ Html.text "" ]
                        , Table.th [] [ Html.text "#" ]
                        , Table.th [] [ Html.div [ Flex.block, Flex.justifyAround, Flex.row ] [ Html.text "Opponent" ] ]
                        ]
                    , Table.tbody []
                        (challenges |> List.map viewChallenge)
                    )

        RemoteData.Loading ->
            Spinner.spinner [] []

        RemoteData.NotAsked ->
            Html.text "No data" |> muted

        RemoteData.Failure _ ->
            Alert.simpleDanger [] [ Html.text "Failed to load data" ]


view : Model -> Html.Html Msg
view model =
    Card.deck
        [ viewBlock "Play" (viewPlayForm model)
        , viewBlock "Challenges" (viewChallenges model)
        ]



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    challengesReceived SubscriptionResponse



---- PORTS ----


port createChallengesSubscription : String -> Cmd msg


port challengesReceived : (Decode.Value -> msg) -> Sub msg
