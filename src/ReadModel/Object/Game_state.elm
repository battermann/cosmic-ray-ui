-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module ReadModel.Object.Game_state exposing (GamesOptionalArguments, games, value)

import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode
import ReadModel.Enum.Games_select_column
import ReadModel.InputObject
import ReadModel.Interface
import ReadModel.Object
import ReadModel.Scalar
import ReadModel.Union
import ScalarCodecs


type alias GamesOptionalArguments =
    { distinct_on : OptionalArgument (List ReadModel.Enum.Games_select_column.Games_select_column)
    , limit : OptionalArgument Int
    , offset : OptionalArgument Int
    , order_by : OptionalArgument (List ReadModel.InputObject.Games_order_by)
    , where_ : OptionalArgument ReadModel.InputObject.Games_bool_exp
    }


{-| An array relationship

  - distinct\_on - distinct select on columns
  - limit - limit the number of rows returned
  - offset - skip the first n rows. Use only with order\_by
  - order\_by - sort the rows by one or more columns
  - where\_ - filter the rows returned

-}
games : (GamesOptionalArguments -> GamesOptionalArguments) -> SelectionSet decodesTo ReadModel.Object.Games -> SelectionSet (List decodesTo) ReadModel.Object.Game_state
games fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { distinct_on = Absent, limit = Absent, offset = Absent, order_by = Absent, where_ = Absent }

        optionalArgs =
            [ Argument.optional "distinct_on" filledInOptionals.distinct_on (Encode.enum ReadModel.Enum.Games_select_column.toString |> Encode.list), Argument.optional "limit" filledInOptionals.limit Encode.int, Argument.optional "offset" filledInOptionals.offset Encode.int, Argument.optional "order_by" filledInOptionals.order_by (ReadModel.InputObject.encodeGames_order_by |> Encode.list), Argument.optional "where" filledInOptionals.where_ ReadModel.InputObject.encodeGames_bool_exp ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "games" optionalArgs object_ (identity >> Decode.list)


value : SelectionSet String ReadModel.Object.Game_state
value =
    Object.selectionForField "String" "value" [] Decode.string
