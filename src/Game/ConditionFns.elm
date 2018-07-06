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
    P pureCondition -> (pure pureCondition s, s)
    R randomCondition -> random randomCondition s


pure : PureCondition -> GameState -> Bool
pure c s =
  case c of
    And c1 c2 -> and c1 c2 s
    Or c1 c2 -> or c1 c2 s
    Not c -> notFn c s
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


random : RandomCondition -> GameState -> (Bool, GameState)
random c s =
  case c of
    Chance p -> chance p s
    AndR c1 c2 -> andr c1 c2 s


type alias ConditionFn = GameState -> Bool


and : PureCondition -> PureCondition -> ConditionFn
and c1 c2 s = (pure c1 s) && (pure c2 s)

or : PureCondition -> PureCondition -> ConditionFn
or c1 c2 s = (pure c1 s) || (pure c2 s)

notFn : PureCondition -> ConditionFn
notFn c s = not (pure c s)


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


andr : Condition -> Condition -> GameState -> (Bool, GameState)
andr c1 c2 s =
  let
    (a, s1) = condition c1 s
    (b, s2) = condition c2 s1
  in
    (a && b, s2)