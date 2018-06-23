module Game.Triggers exposing (..)

import Time exposing (Time)

import Game.Fire as Fire
import Game.GameState as GameState exposing (GameState)


type alias Trigger = GameState -> Bool


and : Trigger -> Trigger -> Trigger
and t1 t2 = (\s -> (t1 s) && (t2 s))


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


fireExtinguished : Trigger
fireExtinguished s = Fire.isExtinguished s.fire


resourceActive : String -> Trigger
resourceActive name =
  (\s -> GameState.resourceActive name s)