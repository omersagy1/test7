module Game.Condition exposing (..)

import Time exposing (Time)

import Game.Action as Action exposing (Action)
import Game.Fire as Fire
import Game.GameState as GameState exposing (GameState)


type Condition = And Condition Condition
                 | GameTimePassed Time
                 | Never
                 | ResourceAmountAbove String Int 
                 | ResourceActive String
                 | FireExtinguished
                 | FireStoked
                 | ActionPerformed Action
                 | MilestoneReached String
                 | TimeSinceMilestone String Time


conditionFn : Condition -> ConditionFn
conditionFn c =
  case c of
    And c1 c2 -> and (conditionFn c1) (conditionFn c2)
    GameTimePassed t -> gameTimePassed t
    Never -> manualOnly
    ResourceAmountAbove name val -> resourceAbove name val
    ResourceActive name -> resourceActive name
    FireExtinguished -> fireExtinguished
    FireStoked -> fireStoked
    ActionPerformed action -> actionPerformed action
    MilestoneReached name -> milestoneReached name
    TimeSinceMilestone name t -> timePassedSince name t


type alias ConditionFn = GameState -> Bool


and : ConditionFn -> ConditionFn -> ConditionFn
and t1 t2 = (\s -> (t1 s) && (t2 s))


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


fireStoked : ConditionFn
fireStoked = actionPerformed Action.StokeFire


actionPerformed : Action -> ConditionFn
actionPerformed a = (\s -> GameState.actionPerformed a s)


milestoneReached : String -> ConditionFn
milestoneReached name = GameState.milestoneReached name


timePassedSince : String -> Time -> ConditionFn
timePassedSince name target = 
  (\s -> 
    let t = GameState.timeSince name s 
    in
      case t of
        Nothing -> False
        Just passed -> passed > target)
