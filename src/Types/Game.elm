module Types.Game exposing (Game, info, mostRecent)

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
                "Yellow to move"

            else
                "It's your turn"

        ( InProgress, Yellow ) ->
            if yellowToMove then
                "It's your turn"

            else
                "Red to move"

        ( InProgress, None ) ->
            if yellowToMove then
                "Yellow to move"

            else
                "Red to move"

        ( YellowWon, Yellow ) ->
            "You win"

        ( YellowWon, _ ) ->
            "Yellow wins"

        ( RedWon, Red ) ->
            "You win"

        ( RedWon, _ ) ->
            "Red wins"

        ( Draw, _ ) ->
            "Draw"
