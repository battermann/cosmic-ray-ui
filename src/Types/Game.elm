module Types.Game exposing (Game, info, isMine, mostRecent)

import List.Extra
import Types.Column exposing (Column(..))
import Types.GameId exposing (GameId)
import Types.GameState exposing (GameState(..))
import Types.Player exposing (Player(..))


type alias Game =
    { id : GameId
    , serialId : Int
    , moves : List Column
    , state : GameState
    , color : Player
    }


mostRecent : Game -> Maybe ( Int, Int )
mostRecent game =
    game.moves
        |> List.Extra.last
        |> Maybe.map (\(Column col) -> ( col, 6 - (game.moves |> List.Extra.count ((==) (Column col))) ))


info : Game -> String
info game =
    let
        yellowToMove =
            modBy 2 (List.length game.moves) == 0
    in
    case ( game.state, game.color ) of
        ( InProgress, Red ) ->
            if yellowToMove then
                "Yellow To Move"

            else
                "It's Your Turn"

        ( InProgress, Yellow ) ->
            if yellowToMove then
                "It's Your Turn"

            else
                "Red To Move"

        ( InProgress, None ) ->
            if yellowToMove then
                "Yellow To Move"

            else
                "Red To Move"

        ( YellowWon, Yellow ) ->
            "You Win"

        ( YellowWon, _ ) ->
            "Yellow Wins"

        ( RedWon, Red ) ->
            "You Win"

        ( RedWon, _ ) ->
            "Red Wins"

        ( Draw, _ ) ->
            "Draw"


isMine : Game -> Bool
isMine game =
    case game.color of
        Red ->
            True

        Yellow ->
            True

        None ->
            False
