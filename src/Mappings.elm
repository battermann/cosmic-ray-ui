module Mappings exposing (mapGameState, mapPlayer)

import ReadModel.Enum.Game_state_enum as GsEnum exposing (Game_state_enum)
import ReadModel.Enum.Player_enum as PEnum exposing (Player_enum)
import Types.GameState exposing (GameState(..))
import Types.Player as Player exposing (Player)


mapGameState : Game_state_enum -> GameState
mapGameState enum =
    case enum of
        GsEnum.Draw ->
            Draw

        GsEnum.In_progress ->
            InProgress

        GsEnum.Red_won ->
            RedWon

        GsEnum.Yellow_won ->
            YellowWon


mapPlayer : Player_enum -> Player
mapPlayer enum =
    case enum of
        PEnum.Red ->
            Player.Red

        PEnum.Yellow ->
            Player.Yellow

        PEnum.None ->
            Player.None
