module Game.ConditionFns exposing (condition)

import Time exposing (Time)

import Common.Annex exposing (..)
import Common.Randomizer as Randomizer
import Game.Action as Action exposing (Action)
import Game.Condition exposing (..)
import Game.Fire as Fire
import Game.GameState as GameState exposing (GameState)


condition : Condition -> GameState -> (Bool, GameState)
condition c s =
  case c of
    Pure pureCondition -> (pure pureCondition s, s)
    Chance p -> chance p s
    And c1 c2 -> and c1 c2 s
    Or c1 c2 -> or c1 c2 s
    Not c -> notFn c s


pure : PureCondition -> GameState -> Bool
pure c s =
  case c of
    GameTimePassed t -> gameTimePassed t s
    Never -> manualOnly s
    ResourceAmountAbove name val -> resourceAbove name val s
    ResourceActive name -> resourceActive name s
    FireExtinguished -> fireExtinguished s
    FireStoked -> actionPerformed Action.StokeFire s
    ActionPerformed action -> customActionPerformed action s
    MilestoneReached name -> milestoneReached name s
    TimeSinceMilestone name t -> timePassedSince name t s
    MilestoneAtCount name x -> milestoneAtCount name x s
    MilestoneGreaterThan name x -> milestoneGreaterThan name x s


type alias ConditionFn = GameState -> Bool


gameTimePassed : Time -> ConditionFn
gameTimePassed t = (\s -> s.gameTime >= t)


manualOnly : ConditionFn
manualOnly s = False


resourceAbove : String -> Int -> ConditionFn
resourceAbove name amount =
  (\s ->
    case GameState.getResourceNamed name s of
      Nothing -> False
      Just r -> r.amount >= amount)


resourceActive : String -> ConditionFn
resourceActive name =
  (\s -> GameState.resourceActive name s)


fireExtinguished : ConditionFn
fireExtinguished s = Fire.isExtinguished s.fire


actionPerformed : Action -> ConditionFn
actionPerformed a = (\s -> GameState.actionPerformed a s)

customActionPerformed : String -> ConditionFn
customActionPerformed name = 
  (\s -> GameState.customActionPerformed name s)

milestoneReached : String -> ConditionFn
milestoneReached name = GameState.milestoneReached name


timePassedSince : String -> Time -> ConditionFn
timePassedSince name target = 
  (\s -> 
    GameState.timeSince name s 
    |> maybePred ((<=) target))


milestoneAtCount : String -> Int -> ConditionFn
milestoneAtCount name target =
  (\s ->
    GameState.milestoneCounter name s
    |> (==) target)


milestoneGreaterThan : String -> Int -> ConditionFn
milestoneGreaterThan name target =
  (\s ->
    GameState.milestoneCounter name s
    |> (<) target)


chance : Float -> GameState -> (Bool, GameState)
chance p s =
  let
    (x, r) = Randomizer.float 0 1 s.randomizer
    success = p > x
  in
    (success, { s | randomizer = r })


and : Condition -> Condition -> GameState -> (Bool, GameState)
and c1 c2 s =
  let
    (a, s1) = condition c1 s
    (b, s2) = condition c2 s1
  in
    (a && b, s2)


or : Condition -> Condition -> GameState -> (Bool, GameState)
or c1 c2 s =
  let
    (a, s1) = condition c1 s
    (b, s2) = condition c2 s1
  in
    (a || b, s2)


notFn : Condition -> GameState -> (Bool, GameState)
notFn c s =
  let
    (a, s1) = condition c s
  in
    (not a, s1)