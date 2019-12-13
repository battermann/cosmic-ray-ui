-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module ReadModel.Subscription exposing (ChallengesByPkRequiredArguments, ChallengesOptionalArguments, ColorByPkRequiredArguments, ColorOptionalArguments, GameOptionalArguments, GameRequiredArguments, GameStateByPkRequiredArguments, GameStateOptionalArguments, GamesByPkRequiredArguments, GamesOptionalArguments, LiveGamesOptionalArguments, LiveGamesRequiredArguments, MyChallengesOptionalArguments, MyChallengesRequiredArguments, MyGamesOptionalArguments, MyGamesRequiredArguments, PlayerByPkRequiredArguments, PlayerOptionalArguments, VillainChallengesOptionalArguments, VillainChallengesRequiredArguments, challenges, challenges_by_pk, color, color_by_pk, game, game_state, game_state_by_pk, games, games_by_pk, live_games, my_challenges, my_games, player, player_by_pk, villain_challenges)

import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode exposing (Decoder)
import ReadModel.Enum.Challenges_select_column
import ReadModel.Enum.Color_select_column
import ReadModel.Enum.Game_state_select_column
import ReadModel.Enum.Games_select_column
import ReadModel.Enum.Player_select_column
import ReadModel.InputObject
import ReadModel.Interface
import ReadModel.Object
import ReadModel.Scalar
import ReadModel.Union
import ScalarCodecs


type alias ChallengesOptionalArguments =
    { distinct_on : OptionalArgument (List ReadModel.Enum.Challenges_select_column.Challenges_select_column)
    , limit : OptionalArgument Int
    , offset : OptionalArgument Int
    , order_by : OptionalArgument (List ReadModel.InputObject.Challenges_order_by)
    , where_ : OptionalArgument ReadModel.InputObject.Challenges_bool_exp
    }


{-| fetch data from the table: "challenges"

  - distinct\_on - distinct select on columns
  - limit - limit the number of rows returned
  - offset - skip the first n rows. Use only with order\_by
  - order\_by - sort the rows by one or more columns
  - where\_ - filter the rows returned

-}
challenges : (ChallengesOptionalArguments -> ChallengesOptionalArguments) -> SelectionSet decodesTo ReadModel.Object.Challenges -> SelectionSet (List decodesTo) RootSubscription
challenges fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { distinct_on = Absent, limit = Absent, offset = Absent, order_by = Absent, where_ = Absent }

        optionalArgs =
            [ Argument.optional "distinct_on" filledInOptionals.distinct_on (Encode.enum ReadModel.Enum.Challenges_select_column.toString |> Encode.list), Argument.optional "limit" filledInOptionals.limit Encode.int, Argument.optional "offset" filledInOptionals.offset Encode.int, Argument.optional "order_by" filledInOptionals.order_by (ReadModel.InputObject.encodeChallenges_order_by |> Encode.list), Argument.optional "where" filledInOptionals.where_ ReadModel.InputObject.encodeChallenges_bool_exp ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "challenges" optionalArgs object_ (identity >> Decode.list)


type alias ChallengesByPkRequiredArguments =
    { id : ScalarCodecs.Uuid }


{-| fetch data from the table: "challenges" using primary key columns
-}
challenges_by_pk : ChallengesByPkRequiredArguments -> SelectionSet decodesTo ReadModel.Object.Challenges -> SelectionSet (Maybe decodesTo) RootSubscription
challenges_by_pk requiredArgs object_ =
    Object.selectionForCompositeField "challenges_by_pk" [ Argument.required "id" requiredArgs.id (ScalarCodecs.codecs |> ReadModel.Scalar.unwrapEncoder .codecUuid) ] object_ (identity >> Decode.nullable)


type alias ColorOptionalArguments =
    { distinct_on : OptionalArgument (List ReadModel.Enum.Color_select_column.Color_select_column)
    , limit : OptionalArgument Int
    , offset : OptionalArgument Int
    , order_by : OptionalArgument (List ReadModel.InputObject.Color_order_by)
    , where_ : OptionalArgument ReadModel.InputObject.Color_bool_exp
    }


{-| fetch data from the table: "color"

  - distinct\_on - distinct select on columns
  - limit - limit the number of rows returned
  - offset - skip the first n rows. Use only with order\_by
  - order\_by - sort the rows by one or more columns
  - where\_ - filter the rows returned

-}
color : (ColorOptionalArguments -> ColorOptionalArguments) -> SelectionSet decodesTo ReadModel.Object.Color -> SelectionSet (List decodesTo) RootSubscription
color fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { distinct_on = Absent, limit = Absent, offset = Absent, order_by = Absent, where_ = Absent }

        optionalArgs =
            [ Argument.optional "distinct_on" filledInOptionals.distinct_on (Encode.enum ReadModel.Enum.Color_select_column.toString |> Encode.list), Argument.optional "limit" filledInOptionals.limit Encode.int, Argument.optional "offset" filledInOptionals.offset Encode.int, Argument.optional "order_by" filledInOptionals.order_by (ReadModel.InputObject.encodeColor_order_by |> Encode.list), Argument.optional "where" filledInOptionals.where_ ReadModel.InputObject.encodeColor_bool_exp ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "color" optionalArgs object_ (identity >> Decode.list)


type alias ColorByPkRequiredArguments =
    { value : String }


{-| fetch data from the table: "color" using primary key columns
-}
color_by_pk : ColorByPkRequiredArguments -> SelectionSet decodesTo ReadModel.Object.Color -> SelectionSet (Maybe decodesTo) RootSubscription
color_by_pk requiredArgs object_ =
    Object.selectionForCompositeField "color_by_pk" [ Argument.required "value" requiredArgs.value Encode.string ] object_ (identity >> Decode.nullable)


type alias GameOptionalArguments =
    { distinct_on : OptionalArgument (List ReadModel.Enum.Games_select_column.Games_select_column)
    , limit : OptionalArgument Int
    , offset : OptionalArgument Int
    , order_by : OptionalArgument (List ReadModel.InputObject.Games_order_by)
    , where_ : OptionalArgument ReadModel.InputObject.Games_bool_exp
    }


type alias GameRequiredArguments =
    { args : ReadModel.InputObject.Game_args }


{-| execute function "game" which returns "games"

  - args - input parameters for function "game"
  - distinct\_on - distinct select on columns
  - limit - limit the number of rows returned
  - offset - skip the first n rows. Use only with order\_by
  - order\_by - sort the rows by one or more columns
  - where\_ - filter the rows returned

-}
game : (GameOptionalArguments -> GameOptionalArguments) -> GameRequiredArguments -> SelectionSet decodesTo ReadModel.Object.Games -> SelectionSet (List decodesTo) RootSubscription
game fillInOptionals requiredArgs object_ =
    let
        filledInOptionals =
            fillInOptionals { distinct_on = Absent, limit = Absent, offset = Absent, order_by = Absent, where_ = Absent }

        optionalArgs =
            [ Argument.optional "distinct_on" filledInOptionals.distinct_on (Encode.enum ReadModel.Enum.Games_select_column.toString |> Encode.list), Argument.optional "limit" filledInOptionals.limit Encode.int, Argument.optional "offset" filledInOptionals.offset Encode.int, Argument.optional "order_by" filledInOptionals.order_by (ReadModel.InputObject.encodeGames_order_by |> Encode.list), Argument.optional "where" filledInOptionals.where_ ReadModel.InputObject.encodeGames_bool_exp ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "game" (optionalArgs ++ [ Argument.required "args" requiredArgs.args ReadModel.InputObject.encodeGame_args ]) object_ (identity >> Decode.list)


type alias GameStateOptionalArguments =
    { distinct_on : OptionalArgument (List ReadModel.Enum.Game_state_select_column.Game_state_select_column)
    , limit : OptionalArgument Int
    , offset : OptionalArgument Int
    , order_by : OptionalArgument (List ReadModel.InputObject.Game_state_order_by)
    , where_ : OptionalArgument ReadModel.InputObject.Game_state_bool_exp
    }


{-| fetch data from the table: "game\_state"

  - distinct\_on - distinct select on columns
  - limit - limit the number of rows returned
  - offset - skip the first n rows. Use only with order\_by
  - order\_by - sort the rows by one or more columns
  - where\_ - filter the rows returned

-}
game_state : (GameStateOptionalArguments -> GameStateOptionalArguments) -> SelectionSet decodesTo ReadModel.Object.Game_state -> SelectionSet (List decodesTo) RootSubscription
game_state fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { distinct_on = Absent, limit = Absent, offset = Absent, order_by = Absent, where_ = Absent }

        optionalArgs =
            [ Argument.optional "distinct_on" filledInOptionals.distinct_on (Encode.enum ReadModel.Enum.Game_state_select_column.toString |> Encode.list), Argument.optional "limit" filledInOptionals.limit Encode.int, Argument.optional "offset" filledInOptionals.offset Encode.int, Argument.optional "order_by" filledInOptionals.order_by (ReadModel.InputObject.encodeGame_state_order_by |> Encode.list), Argument.optional "where" filledInOptionals.where_ ReadModel.InputObject.encodeGame_state_bool_exp ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "game_state" optionalArgs object_ (identity >> Decode.list)


type alias GameStateByPkRequiredArguments =
    { value : String }


{-| fetch data from the table: "game\_state" using primary key columns
-}
game_state_by_pk : GameStateByPkRequiredArguments -> SelectionSet decodesTo ReadModel.Object.Game_state -> SelectionSet (Maybe decodesTo) RootSubscription
game_state_by_pk requiredArgs object_ =
    Object.selectionForCompositeField "game_state_by_pk" [ Argument.required "value" requiredArgs.value Encode.string ] object_ (identity >> Decode.nullable)


type alias GamesOptionalArguments =
    { distinct_on : OptionalArgument (List ReadModel.Enum.Games_select_column.Games_select_column)
    , limit : OptionalArgument Int
    , offset : OptionalArgument Int
    , order_by : OptionalArgument (List ReadModel.InputObject.Games_order_by)
    , where_ : OptionalArgument ReadModel.InputObject.Games_bool_exp
    }


{-| fetch data from the table: "games"

  - distinct\_on - distinct select on columns
  - limit - limit the number of rows returned
  - offset - skip the first n rows. Use only with order\_by
  - order\_by - sort the rows by one or more columns
  - where\_ - filter the rows returned

-}
games : (GamesOptionalArguments -> GamesOptionalArguments) -> SelectionSet decodesTo ReadModel.Object.Games -> SelectionSet (List decodesTo) RootSubscription
games fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { distinct_on = Absent, limit = Absent, offset = Absent, order_by = Absent, where_ = Absent }

        optionalArgs =
            [ Argument.optional "distinct_on" filledInOptionals.distinct_on (Encode.enum ReadModel.Enum.Games_select_column.toString |> Encode.list), Argument.optional "limit" filledInOptionals.limit Encode.int, Argument.optional "offset" filledInOptionals.offset Encode.int, Argument.optional "order_by" filledInOptionals.order_by (ReadModel.InputObject.encodeGames_order_by |> Encode.list), Argument.optional "where" filledInOptionals.where_ ReadModel.InputObject.encodeGames_bool_exp ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "games" optionalArgs object_ (identity >> Decode.list)


type alias GamesByPkRequiredArguments =
    { id : ScalarCodecs.Uuid }


{-| fetch data from the table: "games" using primary key columns
-}
games_by_pk : GamesByPkRequiredArguments -> SelectionSet decodesTo ReadModel.Object.Games -> SelectionSet (Maybe decodesTo) RootSubscription
games_by_pk requiredArgs object_ =
    Object.selectionForCompositeField "games_by_pk" [ Argument.required "id" requiredArgs.id (ScalarCodecs.codecs |> ReadModel.Scalar.unwrapEncoder .codecUuid) ] object_ (identity >> Decode.nullable)


type alias LiveGamesOptionalArguments =
    { distinct_on : OptionalArgument (List ReadModel.Enum.Games_select_column.Games_select_column)
    , limit : OptionalArgument Int
    , offset : OptionalArgument Int
    , order_by : OptionalArgument (List ReadModel.InputObject.Games_order_by)
    , where_ : OptionalArgument ReadModel.InputObject.Games_bool_exp
    }


type alias LiveGamesRequiredArguments =
    { args : ReadModel.InputObject.Live_games_args }


{-| execute function "live\_games" which returns "games"

  - args - input parameters for function "live\_games"
  - distinct\_on - distinct select on columns
  - limit - limit the number of rows returned
  - offset - skip the first n rows. Use only with order\_by
  - order\_by - sort the rows by one or more columns
  - where\_ - filter the rows returned

-}
live_games : (LiveGamesOptionalArguments -> LiveGamesOptionalArguments) -> LiveGamesRequiredArguments -> SelectionSet decodesTo ReadModel.Object.Games -> SelectionSet (List decodesTo) RootSubscription
live_games fillInOptionals requiredArgs object_ =
    let
        filledInOptionals =
            fillInOptionals { distinct_on = Absent, limit = Absent, offset = Absent, order_by = Absent, where_ = Absent }

        optionalArgs =
            [ Argument.optional "distinct_on" filledInOptionals.distinct_on (Encode.enum ReadModel.Enum.Games_select_column.toString |> Encode.list), Argument.optional "limit" filledInOptionals.limit Encode.int, Argument.optional "offset" filledInOptionals.offset Encode.int, Argument.optional "order_by" filledInOptionals.order_by (ReadModel.InputObject.encodeGames_order_by |> Encode.list), Argument.optional "where" filledInOptionals.where_ ReadModel.InputObject.encodeGames_bool_exp ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "live_games" (optionalArgs ++ [ Argument.required "args" requiredArgs.args ReadModel.InputObject.encodeLive_games_args ]) object_ (identity >> Decode.list)


type alias MyChallengesOptionalArguments =
    { distinct_on : OptionalArgument (List ReadModel.Enum.Challenges_select_column.Challenges_select_column)
    , limit : OptionalArgument Int
    , offset : OptionalArgument Int
    , order_by : OptionalArgument (List ReadModel.InputObject.Challenges_order_by)
    , where_ : OptionalArgument ReadModel.InputObject.Challenges_bool_exp
    }


type alias MyChallengesRequiredArguments =
    { args : ReadModel.InputObject.My_challenges_args }


{-| execute function "my\_challenges" which returns "challenges"

  - args - input parameters for function "my\_challenges"
  - distinct\_on - distinct select on columns
  - limit - limit the number of rows returned
  - offset - skip the first n rows. Use only with order\_by
  - order\_by - sort the rows by one or more columns
  - where\_ - filter the rows returned

-}
my_challenges : (MyChallengesOptionalArguments -> MyChallengesOptionalArguments) -> MyChallengesRequiredArguments -> SelectionSet decodesTo ReadModel.Object.Challenges -> SelectionSet (List decodesTo) RootSubscription
my_challenges fillInOptionals requiredArgs object_ =
    let
        filledInOptionals =
            fillInOptionals { distinct_on = Absent, limit = Absent, offset = Absent, order_by = Absent, where_ = Absent }

        optionalArgs =
            [ Argument.optional "distinct_on" filledInOptionals.distinct_on (Encode.enum ReadModel.Enum.Challenges_select_column.toString |> Encode.list), Argument.optional "limit" filledInOptionals.limit Encode.int, Argument.optional "offset" filledInOptionals.offset Encode.int, Argument.optional "order_by" filledInOptionals.order_by (ReadModel.InputObject.encodeChallenges_order_by |> Encode.list), Argument.optional "where" filledInOptionals.where_ ReadModel.InputObject.encodeChallenges_bool_exp ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "my_challenges" (optionalArgs ++ [ Argument.required "args" requiredArgs.args ReadModel.InputObject.encodeMy_challenges_args ]) object_ (identity >> Decode.list)


type alias MyGamesOptionalArguments =
    { distinct_on : OptionalArgument (List ReadModel.Enum.Games_select_column.Games_select_column)
    , limit : OptionalArgument Int
    , offset : OptionalArgument Int
    , order_by : OptionalArgument (List ReadModel.InputObject.Games_order_by)
    , where_ : OptionalArgument ReadModel.InputObject.Games_bool_exp
    }


type alias MyGamesRequiredArguments =
    { args : ReadModel.InputObject.My_games_args }


{-| execute function "my\_games" which returns "games"

  - args - input parameters for function "my\_games"
  - distinct\_on - distinct select on columns
  - limit - limit the number of rows returned
  - offset - skip the first n rows. Use only with order\_by
  - order\_by - sort the rows by one or more columns
  - where\_ - filter the rows returned

-}
my_games : (MyGamesOptionalArguments -> MyGamesOptionalArguments) -> MyGamesRequiredArguments -> SelectionSet decodesTo ReadModel.Object.Games -> SelectionSet (List decodesTo) RootSubscription
my_games fillInOptionals requiredArgs object_ =
    let
        filledInOptionals =
            fillInOptionals { distinct_on = Absent, limit = Absent, offset = Absent, order_by = Absent, where_ = Absent }

        optionalArgs =
            [ Argument.optional "distinct_on" filledInOptionals.distinct_on (Encode.enum ReadModel.Enum.Games_select_column.toString |> Encode.list), Argument.optional "limit" filledInOptionals.limit Encode.int, Argument.optional "offset" filledInOptionals.offset Encode.int, Argument.optional "order_by" filledInOptionals.order_by (ReadModel.InputObject.encodeGames_order_by |> Encode.list), Argument.optional "where" filledInOptionals.where_ ReadModel.InputObject.encodeGames_bool_exp ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "my_games" (optionalArgs ++ [ Argument.required "args" requiredArgs.args ReadModel.InputObject.encodeMy_games_args ]) object_ (identity >> Decode.list)


type alias PlayerOptionalArguments =
    { distinct_on : OptionalArgument (List ReadModel.Enum.Player_select_column.Player_select_column)
    , limit : OptionalArgument Int
    , offset : OptionalArgument Int
    , order_by : OptionalArgument (List ReadModel.InputObject.Player_order_by)
    , where_ : OptionalArgument ReadModel.InputObject.Player_bool_exp
    }


{-| fetch data from the table: "player"

  - distinct\_on - distinct select on columns
  - limit - limit the number of rows returned
  - offset - skip the first n rows. Use only with order\_by
  - order\_by - sort the rows by one or more columns
  - where\_ - filter the rows returned

-}
player : (PlayerOptionalArguments -> PlayerOptionalArguments) -> SelectionSet decodesTo ReadModel.Object.Player -> SelectionSet (List decodesTo) RootSubscription
player fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { distinct_on = Absent, limit = Absent, offset = Absent, order_by = Absent, where_ = Absent }

        optionalArgs =
            [ Argument.optional "distinct_on" filledInOptionals.distinct_on (Encode.enum ReadModel.Enum.Player_select_column.toString |> Encode.list), Argument.optional "limit" filledInOptionals.limit Encode.int, Argument.optional "offset" filledInOptionals.offset Encode.int, Argument.optional "order_by" filledInOptionals.order_by (ReadModel.InputObject.encodePlayer_order_by |> Encode.list), Argument.optional "where" filledInOptionals.where_ ReadModel.InputObject.encodePlayer_bool_exp ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "player" optionalArgs object_ (identity >> Decode.list)


type alias PlayerByPkRequiredArguments =
    { value : String }


{-| fetch data from the table: "player" using primary key columns
-}
player_by_pk : PlayerByPkRequiredArguments -> SelectionSet decodesTo ReadModel.Object.Player -> SelectionSet (Maybe decodesTo) RootSubscription
player_by_pk requiredArgs object_ =
    Object.selectionForCompositeField "player_by_pk" [ Argument.required "value" requiredArgs.value Encode.string ] object_ (identity >> Decode.nullable)


type alias VillainChallengesOptionalArguments =
    { distinct_on : OptionalArgument (List ReadModel.Enum.Challenges_select_column.Challenges_select_column)
    , limit : OptionalArgument Int
    , offset : OptionalArgument Int
    , order_by : OptionalArgument (List ReadModel.InputObject.Challenges_order_by)
    , where_ : OptionalArgument ReadModel.InputObject.Challenges_bool_exp
    }


type alias VillainChallengesRequiredArguments =
    { args : ReadModel.InputObject.Villain_challenges_args }


{-| execute function "villain\_challenges" which returns "challenges"

  - args - input parameters for function "villain\_challenges"
  - distinct\_on - distinct select on columns
  - limit - limit the number of rows returned
  - offset - skip the first n rows. Use only with order\_by
  - order\_by - sort the rows by one or more columns
  - where\_ - filter the rows returned

-}
villain_challenges : (VillainChallengesOptionalArguments -> VillainChallengesOptionalArguments) -> VillainChallengesRequiredArguments -> SelectionSet decodesTo ReadModel.Object.Challenges -> SelectionSet (List decodesTo) RootSubscription
villain_challenges fillInOptionals requiredArgs object_ =
    let
        filledInOptionals =
            fillInOptionals { distinct_on = Absent, limit = Absent, offset = Absent, order_by = Absent, where_ = Absent }

        optionalArgs =
            [ Argument.optional "distinct_on" filledInOptionals.distinct_on (Encode.enum ReadModel.Enum.Challenges_select_column.toString |> Encode.list), Argument.optional "limit" filledInOptionals.limit Encode.int, Argument.optional "offset" filledInOptionals.offset Encode.int, Argument.optional "order_by" filledInOptionals.order_by (ReadModel.InputObject.encodeChallenges_order_by |> Encode.list), Argument.optional "where" filledInOptionals.where_ ReadModel.InputObject.encodeChallenges_bool_exp ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "villain_challenges" (optionalArgs ++ [ Argument.required "args" requiredArgs.args ReadModel.InputObject.encodeVillain_challenges_args ]) object_ (identity >> Decode.list)
