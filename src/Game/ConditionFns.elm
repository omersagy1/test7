module Game.ConditionFns exposing (condition, pure)

import Time exposing (Time)

import Common.Annex exposing (..)
import Common.Randomizer as Randomizer exposing (Randomizer)
import Game.ActionName as ActionName
import Game.Condition exposing (..)
import Game.Fire as Fire
import Game.GameState as GameState exposing (GameState)


-- Narrows the type of the function so it can't access/modify anything
-- but the randomizer.
type alias RandomState a =
  { a | randomizer : Randomizer }


condition : Condition -> GameState -> (Bool, RandomState GameState)
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
    Never -> False
    Always -> True
    ResourceAmountAbove name val -> resourceAbove name val s
    ResourceActive name -> resourceActive name s
    FireExtinguished -> fireExtinguished s
    FireStoked -> actionPerformed ActionName.StokeFire s
    ActionPerformed action -> customActionPerformed action s
    MilestoneReached name -> milestoneReached name s
    TimeSinceMilestone name t -> timePassedSince name t s
    MilestoneAtCount name x -> milestoneAtCount name x s
    MilestoneGreaterThan name x -> milestoneGreaterThan name x s


type alias ConditionFn = GameState -> Bool


gameTimePassed : Time -> ConditionFn
gameTimePassed t = (\s -> s.gameTime >= t)


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


actionPerformed : ActionName.Name -> ConditionFn
actionPerformed a = (\s -> GameState.actionPerformed a s)


customActionPerformed : ActionName.Name -> ConditionFn
customActionPerformed name = 
  (\s -> GameState.actionPerformed name s)


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


chance : Float -> RandomState GameState -> (Bool, RandomState GameState)
chance p s =
  let
    (x, r) = Randomizer.float 0 1 s.randomizer
    success = p > x
  in
    (success, { s | randomizer = r })


and : Condition -> Condition -> GameState -> (Bool, RandomState GameState)
and c1 c2 s =
  let
    (a, s1) = condition c1 s
    (b, s2) = condition c2 s1
  in
    (a && b, s2)


or : Condition -> Condition -> GameState -> (Bool, RandomState GameState)
or c1 c2 s =
  let
    (a, s1) = condition c1 s
    (b, s2) = condition c2 s1
  in
    (a || b, s2)


notFn : Condition -> GameState -> (Bool, RandomState GameState)
notFn c s =
  let
    (a, s1) = condition c s
  in
    (not a, s1)