module Game.Triggers exposing (..)

import Time exposing (Time)

import Game.GameState as GameState exposing (GameState)


type alias Trigger = GameState -> Bool


gameTimePassed : Time -> Trigger
gameTimePassed t = (\s -> s.gameTime >= t)


manualOnly : Trigger
manualOnly s = False


resourceAbove : String -> Int -> Trigger
resourceAbove name amount =
  (\s ->
    case GameState.getResourceNamed name s of
      Nothing -> False
      Just r -> r.amount >= amount)
