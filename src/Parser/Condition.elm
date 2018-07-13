module Parser.Condition exposing (..)

import Common.Annex exposing (..)
import Game.Condition as Condition exposing (..)


chance = Chance 
and = And 
or = Or 
ifnot = Not 
gameTimePassed = GameTimePassed >> Pure
never = Never |> Pure
unconditionally = Always |> Pure
resourceAmountAbove = ResourceAmountAbove >>> Pure
resourceActive = ResourceActive >> Pure
fireExtinguished = FireExtinguished |> Pure
fireStoked = FireStoked |> Pure
actionPerformed = ActionPerformed >> Pure
milestoneReached = MilestoneReached >> Pure
timeSinceMilestone = TimeSinceMilestone >>> Pure
milestoneAtCount = MilestoneAtCount >>> Pure
milestoneGreaterThan = MilestoneGreaterThan >>> Pure