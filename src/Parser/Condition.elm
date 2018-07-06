module Parser.Condition exposing (..)

import Common.Annex exposing (..)
import Game.Condition as Condition exposing (..)

-- Condition Builders --
chance = Chance >> R
and = And >>> R
or = Or >>> R
fail = Not >> R
gameTimePassed = GameTimePassed >> P
never = Never |> P
resourceAmountAbove = ResourceAmountAbove >>> P
resourceActive = ResourceActive >> P
fireExtinguished = FireExtinguished |> P
fireStoked = FireStoked |> P
actionPerformed = ActionPerformed >> P
milestoneReached = MilestoneReached >> P
timeSinceMilestone = TimeSinceMilestone >>> P
milestoneAtCount = MilestoneAtCount >>> P
milestoneGreaterThan = MilestoneGreaterThan >>> P