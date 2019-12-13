module Types.ClientId exposing (ClientId(..))

import UUID exposing (UUID)


type ClientId
    = ClientId UUID
