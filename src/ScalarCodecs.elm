module ScalarCodecs exposing (Id, Int4_, Uuid, codecs)

import Json.Decode as Decode
import Json.Encode as Encode
import ReadModel.Scalar exposing (defaultCodecs)
import UUID exposing (UUID)


type alias Id =
    ReadModel.Scalar.Id


type alias Int4_ =
    List Int


type alias Uuid =
    UUID


codecs : ReadModel.Scalar.Codecs Id Int4_ Uuid
codecs =
    ReadModel.Scalar.defineCodecs
        { codecId = defaultCodecs.codecId
        , codecInt4_ =
            { encoder = \intList -> Encode.string (intList |> List.map String.fromInt |> String.join ",")
            , decoder = Decode.list Decode.int
            }
        , codecUuid =
            { encoder = \uuid -> Encode.string (UUID.toString uuid)
            , decoder =
                Decode.string
                    |> Decode.andThen
                        (\str ->
                            case UUID.fromString str of
                                Err _ ->
                                    Decode.fail "cannot decode uuid"

                                Ok v ->
                                    Decode.succeed v
                        )
            }
        }
