module Types.GameState exposing (GameState(..), isInProgress, toString)


type GameState
    = Draw
    | InProgress
    | RedWon
    | YellowWon


isInProgress : GameState -> Bool
isInProgress gs =
    case gs of
        InProgress ->
            True

        _ ->
            False


toString : GameState -> String
toString gs =
    case gs of
        InProgress ->
            "Runningn"

        RedWon ->
            "Red won"

        YellowWon ->
            "Yellow won"

        Draw ->
            "Draw"
