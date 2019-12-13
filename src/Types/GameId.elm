module Types.GameId exposing (GameId(..), toString)

import UUID exposing (UUID)


type GameId
    = GameId UUID


toString : GameId -> String
toString (GameId gameId) =
    UUID.toString gameId
