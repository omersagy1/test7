module Game.Condition exposing (..)

import Time exposing (Time)

import Annex exposing (..)
import Game.Action as Action exposing (Action)
import Game.Fire as Fire
import Game.GameState as GameState exposing (GameState)


type Condition = And Condition Condition
                 | Not Condition
                 | GameTimePassed Time
                 | Never
                 | ResourceAmountAbove String Int 
                 | ResourceActive String
                 | FireExtinguished
                 | FireStoked
                 | ActionPerformed Action
                 | CustomActionPerformed String
                 | MilestoneReached String
                 | TimeSinceMilestone String Time
                 | MilestoneAtCount String Int


conditionFn : Condition -> ConditionFn
conditionFn c =
  case c of
    And c1 c2 -> and (conditionFn c1) (conditionFn c2)
    Not c -> notFn (conditionFn c)
    GameTimePassed t -> gameTimePassed t
    Never -> manualOnly
    ResourceAmountAbove name val -> resourceAbove name val
    ResourceActive name -> resourceActive name
    FireExtinguished -> fireExtinguished
    FireStoked -> fireStoked
    ActionPerformed action -> actionPerformed action
    CustomActionPerformed name -> customActionPerformed name
    MilestoneReached name -> milestoneReached name
    TimeSinceMilestone name t -> timePassedSince name t
    MilestoneAtCount name x -> milestoneAtCount name x
      


type alias ConditionFn = GameState -> Bool


and : ConditionFn -> ConditionFn -> ConditionFn
and t1 t2 = (\s -> (t1 s) && (t2 s))

notFn : ConditionFn -> ConditionFn
notFn c = (\s -> not (c s))

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
    |> maybePred ((==) target))
