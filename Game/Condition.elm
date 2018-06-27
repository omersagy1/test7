module Game.Condition exposing (..)

import Time exposing (Time)

import Game.Action as Action exposing (Action)
import Game.Fire as Fire
import Game.GameState as GameState exposing (GameState)


type alias Condition = GameState -> Bool


and : Condition -> Condition -> Condition
and t1 t2 = (\s -> (t1 s) && (t2 s))


gameTimePassed : Time -> Condition
gameTimePassed t = (\s -> s.gameTime >= t)


manualOnly : Condition
manualOnly s = False


resourceAbove : String -> Int -> Condition
resourceAbove name amount =
  (\s ->
    case GameState.getResourceNamed name s of
      Nothing -> False
      Just r -> r.amount >= amount)


resourceActive : String -> Condition
resourceActive name =
  (\s -> GameState.resourceActive name s)


fireExtinguished : Condition
fireExtinguished s = Fire.isExtinguished s.fire


fireStoked : Condition
fireStoked = actionPerformed Action.StokeFire


actionPerformed : Action -> Condition
actionPerformed a = (\s -> GameState.actionPerformed a s)


milestoneReached : String -> Condition
milestoneReached name = GameState.milestoneReached name


timePassedSince : String -> Time -> Condition
timePassedSince name target = 
  (\s -> 
    let t = GameState.timeSince name s 
    in
      case t of
        Nothing -> False
        Just passed -> passed > target)
